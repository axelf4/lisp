/** Checkpoint/Restore In Userspace.
 *
 * Serializes application state to disk, similarly to the CRIU
 * project, but assumes a cooperative program instead of parasite code
 * infection. Dumps memory maps, registers and signal handlers.
 * Currently supports only Linux x86-64 with a single thread.
 *
 * @see https://criu.org/Main_Page
 */

#define _GNU_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <limits.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include "util.h"

#define PAGE_SIZE (1u << 12)
#define TASK_SIZE ((1ul << 47) - PAGE_SIZE)

enum VmaType {
	VMA_REGULAR = 1 << 0, ///< Regular memory area to be dumped and restored.
	VMA_FILE = 1 << 1, ///< Memory-mapped file.
	VMA_VSYSCALL = 1 << 2, ///< Injected by kernel for virtual syscall implementation.
	VMA_STACK = 1 << 8, ///< Need to take care of guard page.
	VMA_VDSO = 1 << 9,
	VMA_VVAR = 1 << 10,
};
#define VMA_SHOULD_DUMP (VMA_REGULAR | VMA_FILE)

/** Virtual memory area (VMA). */
struct Map {
	enum VmaType type;
	unsigned long start, end,
		offset; ///< The offset into the file.
	int prot, flags;
	char pathname[128]; // FIXME
};

struct CheckpointHdr {
	unsigned num_maps, num_iovs;
	unsigned long mmap_min_addr, vdso_addr;
	struct rt_sigframe *frame;
	struct sigaction sigacts[__SIGRTMIN];
	struct Map maps[];
};

typedef void restore_fn(uintptr_t hint, struct CheckpointHdr *hdr, int fd);

#if IS_RESTORER_DSO
#include <asm/prctl.h>

#define SYS_VAR0()
#define SYS_VAR1(_1) SYS_VAR0()
#define SYS_VAR2(_1, _2) SYS_VAR1(_1)
#define SYS_VAR3(_1, _2, _3) SYS_VAR2(_1, _2)
#define SYS_VAR4(_1, _2, _3, _4) SYS_VAR3(_1, _2, _3) register long _a4 __asm__ ("r10") = _4;
#define SYS_VAR5(_1, _2, _3, _4, _5) SYS_VAR4(_1, _2, _3, _4) register long _a5 __asm__ ("r8") = _5;
#define SYS_VAR6(_1, _2, _3, _4, _5, _6) SYS_VAR5(_1, _2, _3, _4, _5) register long _a6 __asm__ ("r9") = _6;
#define SYS_IN0()
#define SYS_IN1(_1) SYS_IN0(), "D" (_1)
#define SYS_IN2(_1, _2) SYS_IN1(_1), "S" (_2)
#define SYS_IN3(_1, _2, _3) SYS_IN2(_1, _2), "d" (_3)
#define SYS_IN4(_1, _2, _3, _4) SYS_IN3(_1, _2, _3), "r" (_a4)
#define SYS_IN5(_1, _2, _3, _4, _5) SYS_IN4(_1, _2, _3, _4), "r" (_a5)
#define SYS_IN6(_1, _2, _3, _4, _5, _6) SYS_IN5(_1, _2, _3, _4, _5), "r" (_a6)
#define SYS_BODY(n, type, name, ...) { SYS_VAR##n(__VA_ARGS__) long ret; \
		__asm__ volatile ("syscall" : "=a" (ret)						\
			: "0" (__NR_##name) SYS_IN##n(__VA_ARGS__)					\
			: "rcx", "r11", "cc", "memory");							\
		return (type)ret; }

#define SYSCALL1(type, name, t1, a1) type _##name(t1 a1) SYS_BODY(1, type, name, a1)
#define SYSCALL2(type, name, t1, a1, t2, a2) \
	type _##name(t1 a1, t2 a2) SYS_BODY(2, type, name, a1, a2)
#define SYSCALL3(type, name, t1, a1, t2, a2, t3, a3) \
	type _##name(t1 a1, t2 a2, t3 a3) SYS_BODY(3, type, name, a1, a2, a3)
#define SYSCALL4(type, name, t1, a1, t2, a2, t3, a3, t4, a4) \
	type _##name(t1 a1, t2 a2, t3 a3, t4 a4) SYS_BODY(4, type, name, a1, a2, a3, a4)
#define SYSCALL6(type, name, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6) \
	type _##name(t1 a1, t2 a2, t3 a3, t4 a4, t5 a5, t6 a6) \
	SYS_BODY(6, type, name, a1, a2, a3, a4, a5, a6)

static SYSCALL2(int, munmap, void *, addr, size_t, len)
static SYSCALL6(void *, mmap, void *, addr, size_t, length, int, prot, int, flags, int, fd, off_t, offset)
static SYSCALL3(int, mprotect, void *, addr, size_t, size, int, prot)
static SYSCALL1(int, close, int, fd)
static SYSCALL2(int, open, const char *, pathname, int, flags)
static SYSCALL4(ssize_t, preadv, int, fd, const struct iovec *, iov, int, iovcnt, off_t, offset)
static SYSCALL2(int, arch_prctl, int, op, unsigned long, addr)

static bool restore_map(struct Map *map) {
	int fd = -1, prot = map->prot
		| (map->type & VMA_FILE && map->flags & MAP_SHARED ? 0 : PROT_WRITE);
	if (map->type & VMA_FILE) {
		int flags = map->prot & PROT_WRITE && map->flags & MAP_SHARED ? O_RDWR
			: O_RDONLY;
		if ((fd = _open(map->pathname, flags)) < 0) return false;
	}

	void *p = _mmap((void *)map->start, map->end - map->start, prot,
		map->flags | MAP_FIXED_NOREPLACE | MAP_NORESERVE, fd, map->offset);
	if (fd >= 0) _close(fd);
	return p == (void *)map->start;
}

restore_fn do_restore;
[[noreturn]] void do_restore(uintptr_t hint, struct CheckpointHdr *hdr, int fd) {
	struct iovec *iov = (struct iovec *)(hdr->maps + hdr->num_maps),
		*iov_end = iov + hdr->num_iovs;
	uintptr_t end = ALIGN_UP(iov_end, PAGE_SIZE);
	if (_munmap(0, hint) || _munmap((void *)end, TASK_SIZE - end)) goto err;

	for (struct Map *x = hdr->maps, *end = x + hdr->num_maps; x < end; ++x)
		if (x->type & VMA_SHOULD_DUMP && !restore_map(x)) goto err;

	off_t offset = (char *)iov_end - (char *)hdr;
	do {
		ssize_t n;
		if ((n = _preadv(fd, iov, MIN(iov_end - iov, IOV_MAX), offset)) <= 0) goto err;
		offset += n;
		for (size_t k; n; n -= k) {
			k = MIN(iov->iov_len, (size_t)n);
			if (iov->iov_len -= k) iov->iov_base = (char *)iov->iov_base + k;
			else ++iov;
		}
	} while (iov < iov_end);
	_close(fd);
	// Drop PROT_WRITE from mappings without it
	for (struct Map *x = hdr->maps, *end = x + hdr->num_maps; x < end; ++x)
		if (x->type & VMA_SHOULD_DUMP && !(x->prot & PROT_WRITE))
			_mprotect((void *)x->start, x->end - x->start, x->prot);

	// Map vDSO(+VVAR)
	if (_arch_prctl(ARCH_MAP_VDSO_64, hdr->vdso_addr) < 0) goto err;

	__asm__ volatile ("lea rsp, [%[frame]+8]\n\t"
		"syscall"
		: : "a" (__NR_rt_sigreturn), [frame] "r" (hdr->frame) : "cc", "memory");
	unreachable();
err: __builtin_trap();
}
#else
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <elf.h>
#include <link.h>
#include <sys/rseq.h>
#include <sys/stat.h>

#define FOR_PHDRS(ehdr, phdr) \
	for (ElfW(Phdr) *phdr = (ElfW(Phdr) *)((char *)(ehdr) + (ehdr)->e_phoff), \
				*_end = phdr + (ehdr)->e_phnum; phdr < _end; ++phdr)

#define PM_GUARD_REGION (1ull << 58)
#define PM_FILE (1ull << 61) ///< Page is file-page or shared-anon.
#define PM_SWAP (1ull << 62)
#define PM_PRESENT (1ull << 63)

#define MAX_MAPS 128
#define MAX_IOVS 3072

static unsigned long mmap_min_addr() {
	unsigned long x = 0x10000;
	FILE *f;
	if ((f = fopen("/proc/sys/vm/mmap_min_addr", "r"))) {
		fscanf(f, "%lu", &x);
		fclose(f);
	}
	return x;
}

/** Parses a line of /proc/pid/maps. */
static bool parse_map(FILE *f, struct Map *result) {
	char line[512];
	if (!fgets(line, sizeof line, f)) return false;

	unsigned long start, end;
	char r, w, x, s;
	unsigned long offset;
	unsigned dev_major, dev_minor;
	unsigned long inode;
	int path_ofs, n [[maybe_unused]] = sscanf(
		line, "%lx-%lx %c%c%c%c %lx %x:%x %lu %n",
		&start, &end, &r, &w, &x, &s, &offset,
		&dev_major, &dev_minor, &inode, &path_ofs);
	assert(n == 10);
	// TODO Parse /proc/pid/smaps VmFlags

	int prot = (r == 'r' ? PROT_READ : 0)
		| (w == 'w' ? PROT_WRITE : 0)
		| (x == 'x' ? PROT_EXEC : 0),
		flags = s == 's' ? MAP_SHARED : MAP_PRIVATE;

	char *pathname = line + path_ofs;
	size_t path_len = strlen(pathname);
	if (pathname[path_len - 1] == '\n') pathname[--path_len] = '\0';

	enum VmaType type = pathname[0] == '\0' ? VMA_REGULAR
		: !strcmp(pathname, "[vsyscall]") ? VMA_VSYSCALL
		: !strcmp(pathname, "[vdso]") ? VMA_VDSO
		: !strcmp(pathname, "[vvar]") || !strcmp(pathname, "[vvar_vclock]")
		? (prot & PROT_READ ? VMA_VVAR : 0)
		: !strcmp(pathname, "[heap]") ? VMA_REGULAR
		: !strcmp(pathname, "[stack]") ? VMA_REGULAR | VMA_STACK
		: VMA_FILE;
	if (!(type & VMA_FILE)) flags |= MAP_ANONYMOUS;
	*result = (struct Map)
		{ .type = type, .start = start, .end = end, .offset = offset,
		  .prot = prot, .flags = flags };
	strcpy(result->pathname, pathname);
	return true;
}

static bool should_dump(enum VmaType type, uint64_t pme) {
	return pme & (PM_SWAP | PM_PRESENT)
		&& !(pme & PM_GUARD_REGION)
		&& !(pme & PM_FILE && type & VMA_FILE) // COW?
		&& !(type & VMA_VVAR);
}

static bool do_splice(int fd, struct iovec *iov, struct iovec *iov_end) {
	int pipefd[2], ret = false;
	if (pipe(pipefd)) goto out;
	while (iov < iov_end) {
		ssize_t n;
		if ((n = vmsplice(pipefd[1], iov, MIN(iov_end - iov, IOV_MAX), SPLICE_F_GIFT)) <= 0)
			goto out_close;

		for (ssize_t m = n, k; m; m -= k)
			if ((k = splice(pipefd[0], NULL, fd, NULL, m, SPLICE_F_MOVE | SPLICE_F_MORE)) <= 0)
				goto out_close;

		for (size_t k; n; n -= k) {
			k = MIN(iov->iov_len, (size_t)n);
			if (iov->iov_len -= k) iov->iov_base = (char *)iov->iov_base + k;
			else ++iov;
		}
	}
	ret = true;
out_close:
	close(pipefd[0]);
	close(pipefd[1]);
out: return ret;
}

static int fd;

static void signal_handler([[maybe_unused]] int sig) {
	struct rt_sigframe *frame = (struct rt_sigframe *)
		((char *)__builtin_frame_address(0) + sizeof(long));
	struct Map maps[MAX_MAPS], *map = maps;
	struct iovec iovs[MAX_IOVS], *iov = iovs;
	unsigned long vdso_addr = 0;
	FILE *fmaps, *pagemap;
	if (!((fmaps = fopen("/proc/self/maps", "r"))
			&& (pagemap = fopen("/proc/self/pagemap", "rb")))) die("fopen failed");
	for (;; ++map) {
		if (map >= maps + LENGTH(maps)) die("map overflow");
		if (!parse_map(fmaps, map)) break;
		if (map->type & VMA_VDSO) vdso_addr = map->start;
		if (!(map->type & VMA_SHOULD_DUMP && map->prot & (PROT_READ | PROT_EXEC))) continue;

		uint64_t pme;
		fseek(pagemap, map->start / PAGE_SIZE * sizeof pme, SEEK_SET);
		for (uintptr_t p = map->start; p < map->end; p += PAGE_SIZE) {
			if (fread(&pme, sizeof pme, 1, pagemap) < 1) die("fread failed");
			if (!should_dump(map->type, pme)) continue;

			if (iov > iovs
				&& (uintptr_t)iov[-1].iov_base + iov[-1].iov_len == p)
				iov[-1].iov_len += PAGE_SIZE;
			else
				*iov++ = (struct iovec){ (void *)p, PAGE_SIZE };
			if (iov >= iovs + LENGTH(iovs)) die("iov overflow");
		}
	}
	fclose(pagemap);
	fclose(fmaps);

	struct CheckpointHdr hdr =
		{ .num_maps = map - maps, .num_iovs = iov - iovs,
		  .mmap_min_addr = mmap_min_addr(), .vdso_addr = vdso_addr,
		  .frame = frame };
	// Dump signals
	for (int sig = 1; sig < __SIGRTMIN; ++sig) {
		if (sig == SIGKILL || sig == SIGSTOP) continue;
		if (sigaction(sig, NULL, hdr.sigacts + sig)) die("sigaction failed");
	}

	write(fd, &hdr, sizeof hdr);
	write(fd, maps, hdr.num_maps * sizeof *maps);
	write(fd, iovs, hdr.num_iovs * sizeof *iovs); // Dump iovecs to restore with preadv()
	if (!do_splice(fd, iovs, iov)) die("do_splice failed");
	exit(EXIT_SUCCESS);
}

void checkpoint(int _fd) {
	fd = _fd;

	struct sigaction action;
	action.sa_handler = signal_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = 0;
	if (sigaction(SIGCONT, &action, NULL)) die("sigaction failed");
	// Dump within signal handler to rt_sigreturn akin to longjmp()
	raise(SIGCONT);
}

/** Finds start of region not intersecting union of current maps and @a xs. */
static uintptr_t restorer_mmap_hint(struct CheckpointHdr *hdr, size_t len) {
	FILE *f;
	if (!(f = fopen("/proc/self/maps", "r"))) return 0;
	struct Map *x = hdr->maps, *xend = x + hdr->num_maps, y;
	uintptr_t i = hdr->mmap_min_addr, xstart = x->start, ystart = 0;
	goto do_init_y;
	for (;;)
		if (i + len > xstart) { i = x->end; xstart = ++x < xend ? x->start : UINTPTR_MAX; }
		else if (i + len > ystart) {
			i = y.end;
		do_init_y: ystart = parse_map(f, &y) ? y.start : UINTPTR_MAX;
		} else break;
	fclose(f);
	return i + len >= i // Overflow?
		&& (xstart < UINTPTR_MAX || ystart < UINTPTR_MAX) ? i : 0;
}

static bool map_segment(int fd, ElfW(Phdr) *phdr, char *hint) {
	assert(phdr->p_align <= PAGE_SIZE);
	ElfW(Addr) mapstart = phdr->p_vaddr & ~(PAGE_SIZE - 1),
		dataend = phdr->p_vaddr + phdr->p_filesz,
		allocend = phdr->p_vaddr + phdr->p_memsz,
		mapend = ALIGN_UP(dataend, PAGE_SIZE),
		zeropage = dataend & ~(PAGE_SIZE - 1);
	ElfW(Off) mapoff = phdr->p_offset & ~(PAGE_SIZE - 1);

	int prot = (phdr->p_flags & PF_R ? PROT_READ : 0)
		| (phdr->p_flags & PF_W ? PROT_WRITE : 0)
		| (phdr->p_flags & PF_X ? PROT_EXEC : 0);
	if (mmap(hint + mapstart, mapend - mapstart, prot,
			MAP_PRIVATE | MAP_FIXED, fd, mapoff) == MAP_FAILED) return false;

	if (!(prot & PROT_WRITE))
		mprotect(hint + zeropage, allocend - zeropage, prot | PROT_WRITE);
	memset(hint + dataend, 0, allocend - dataend);
	return true;
}

#pragma GCC diagnostic ignored "-Wcast-align"

static restore_fn *load_img(int fd, struct CheckpointHdr *chdr, size_t *size, uintptr_t *hint) {
	struct stat statbuf;
	if (fstat(fd, &statbuf)) die("fstat failed");
	ElfW(Ehdr) *hdr;
	assert((size_t)statbuf.st_size >= sizeof *hdr);
	if ((hdr = mmap(NULL, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED)
		die("mmap failed");
	assert(!memcmp(hdr->e_ident, ELFMAG, SELFMAG) && "invalid ELF magic");
	assert(hdr->e_type == ET_DYN);

	ElfW(Phdr) *last_load = NULL;
	FOR_PHDRS(hdr, phdr) if (phdr->p_type == PT_LOAD) last_load = phdr;
	if (!last_load) die("no LOAD segment");
	size_t maplength = last_load->p_vaddr + last_load->p_memsz;
	*size += ALIGN_UP(maplength, PAGE_SIZE);
	if (!(*hint = restorer_mmap_hint(chdr, *size))) die("restorer_mmap_hint failed");

	char *strtab = NULL, *symtab = NULL, *hash = NULL;
	FOR_PHDRS(hdr, phdr) switch (phdr->p_type) { // Loop through program header table
	case PT_LOAD:
		if (!map_segment(fd, phdr, (char *)*hint)) die("map_segment failed");
		break;
	case PT_DYNAMIC:
		for (ElfW(Dyn) *dyn = (ElfW(Dyn) *)((char *)hdr + phdr->p_offset),
					*end = dyn + phdr->p_filesz / sizeof *dyn; dyn < end; ++dyn)
			switch (dyn->d_tag) {
			case DT_STRTAB: strtab = (char *)hdr + dyn->d_un.d_ptr; break;
			case DT_SYMTAB: symtab = (char *)hdr + dyn->d_un.d_ptr; break;
			case DT_HASH: hash = (char *)hdr + dyn->d_un.d_ptr; break;
			}
		break;
	}
	if (!strtab || !symtab || !hash) die("missing DT_STRTAB/DT_SYMTAB/DT_HASH");

	ElfW(Word) nchain = ((ElfW(Word) *)hash)[1];
	for (ElfW(Sym) *sym = (ElfW(Sym) *)symtab, *end = sym + nchain; sym < end; ++sym)
		if (!strcmp(strtab + sym->st_name, "do_restore"))
			return (restore_fn *)(*hint + sym->st_value);
	unreachable();
}

static int rseq(struct rseq *rseq, uint32_t rseq_len, int flags, uint32_t sig) {
	return syscall(__NR_rseq, rseq, rseq_len, flags, sig);
}

static void unregister_rseq() {
#if defined(__GLIBC__) && defined(RSEQ_SIG)
	if (!__rseq_size) return;
	struct rseq *rseq_abi = (struct rseq *)
		((char *)__builtin_thread_pointer() + __rseq_offset);
	int ret [[maybe_unused]] = rseq(rseq_abi, __rseq_size, RSEQ_FLAG_UNREGISTER, RSEQ_SIG);
	assert(!ret && "unregistering rseq failed");
#endif
}

void restore(int fd) {
	struct CheckpointHdr hdr0, *hdr;
	if (pread(fd, &hdr0, sizeof hdr0, 0) < (ssize_t)sizeof hdr0) die("pread failed");
	size_t info_len = sizeof *hdr
		+ hdr0.num_maps * sizeof(struct Map) + hdr0.num_iovs * sizeof(struct iovec);
	if ((hdr = mmap(NULL, info_len, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0))
		== MAP_FAILED) die("mmap failed");

	uintptr_t hint;
	int so_fd;
	if ((so_fd = open(LIBRESTORE_SO, O_RDONLY)) < 0) die("open failed");
	size_t size = /* stack */ PAGE_SIZE + info_len;
	restore_fn *f = load_img(so_fd, hdr, &size, &hint);
	close(so_fd);

	// Set up restorer memory in gap between current&target mappings:
	//
	// | restorer code | stack | info (header, maps and iovecs) |
	char *new_info = (char *)(hint + size - info_len);
	// Map restorer stack
	if (mmap(new_info - PAGE_SIZE, PAGE_SIZE, PROT_READ | PROT_WRITE,
			MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_FIXED, -1, 0) == MAP_FAILED)
		die("mmap failed");
	if (mremap(hdr, info_len, info_len, MREMAP_MAYMOVE | MREMAP_FIXED, new_info)
		== MAP_FAILED) die("mremap failed");
	hdr = (struct CheckpointHdr *)new_info;

	// Restore signals
	for (int sig = 1; sig < __SIGRTMIN; ++sig) {
		if (sig == SIGKILL || sig == SIGSTOP) continue;
		if (sigaction(sig, hdr0.sigacts + sig, NULL)) die("sigaction failed");
	}

	// After unmapping, the kernel updating rseq.cpu_id, etc. would SIGSEGV
	unregister_rseq();

	register int fd2 __asm__ ("rdx") = fd;
	__asm__ volatile ("mov rsp, %[sp]\n\t"
		"call %[f]"
		:
		: [f] "rm" (f), [sp] "irm" (new_info), "D" (hint), "S" (hdr), "r" (fd2)
		: "cc", "memory");
	unreachable();
}
#endif
