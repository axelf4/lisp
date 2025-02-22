/** Checkpoint/Restore in Userspace.
 *
 * Simplification of the CRIU project, as the program is known to be
 * cooperative.
 *
 * @see https://criu.org/Main_Page
 */

#define _GNU_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include "util.h"

#define PAGE_SIZE (1u << 12)
#define TASK_SIZE ((1ul << 47) - PAGE_SIZE)

enum VmaType {
	VMA_REGULAR = 1 << 0, ///< Regular memory area to be dumped and restored.
	VMA_FILE = 1 << 1, ///< Memory-mapped file.
	/** Injected by kernel for virtual syscall implementation.
	 *
	 * Not to be dumped.
	 */
	VMA_VSYSCALL = 1 << 2,
	/** Need to take care of guard page. */
	VMA_STACK = 1 << 8,
	/* vDSO area. */
	VMA_VDSO = 1 << 9,
	// TODO Should just remap not dump!
	VMA_VVAR = 1 << 10,
};

#define VMA_SHOULD_DUMP (VMA_REGULAR | VMA_FILE)

/** Virtual memory area (VMA). */
struct Map {
	enum VmaType type;
	unsigned long start, end,
		offset; ///< The offset into the file.
	int prot, flags;
	char pathname[128]; // TODO
};

struct CheckpointHdr {
	unsigned num_maps, num_iovs, maps_offset;
	unsigned long mmap_min_addr;
	struct rt_sigframe *frame;
	struct Map maps[];
};

typedef void restore_fn(uintptr_t hint, struct CheckpointHdr *hdr);

#pragma GCC diagnostic ignored "-Wcast-align"

#if IS_RESTORER_DSO
static int _munmap(void *addr, size_t len) {
	int ret;
	__asm__ volatile ("syscall"
		: "=a" (ret)
		: "0" (__NR_munmap), "D" (addr), "S" (len)
        : "rcx", "r11", "cc", "memory");
	return ret;
}

static void *_mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset) {
	register int flags2 __asm__ ("r10") = flags;
	register int fd2 __asm__ ("r8") = fd;
	register int offset2 __asm__ ("r9") = offset;
	void *ret;
	__asm__ volatile ("syscall"
		: "=a" (ret)
		: "0" (__NR_mmap), "D" (addr), "S" (length), "d" (prot),
		"r" (flags2), "r" (fd2), "r" (offset2)
        : "rcx", "r11", "cc");
	return ret;
}

static int _open(const char *pathname, int flags) {
	int ret;
	__asm__ volatile ("syscall"
		: "=a" (ret)
		: "0" (__NR_open), "D" (pathname), "S" (flags)
        : "rcx", "r11", "cc");
	return ret;
}

static int _close(int fd) {
	int ret;
	__asm__ volatile ("syscall" : "=a" (ret) : "0" (__NR_close), "D" (fd)
        : "cc", "rcx", "r11");
	return ret;
}

static ssize_t _readv(int fd, const struct iovec *iov, int iovcnt) {
	ssize_t ret;
	__asm__ volatile ("syscall"
		: "=a" (ret)
		: "0" (__NR_readv), "D" (fd), "S" (iov), "d" (iovcnt)
        : "rcx", "r11", "cc", "memory");
	return ret;
}

static bool restore_map(struct Map *map) {
	int prot = map->prot | PROT_WRITE, fd = -1;
	if (map->type & VMA_FILE) {
		int flags = map->prot & PROT_WRITE && map->flags & MAP_SHARED ? O_RDWR
			: O_RDONLY;
		if ((fd = _open(map->pathname, flags)) < 0) return false;
	}

	void *p = _mmap((void *) map->start, map->end - map->start,
		prot, map->flags | MAP_FIXED_NOREPLACE | MAP_NORESERVE, fd, map->offset);
	if (fd != -1) _close(fd);
	return p == (void *) map->start;
}

restore_fn do_restore;
[[noreturn]] void do_restore(uintptr_t hint, struct CheckpointHdr *hdr) {
	uintptr_t end = ALIGN_UP(hdr + 1, PAGE_SIZE);
	if (_munmap(0, hint)
		|| _munmap((void *) end, TASK_SIZE - end))
		__builtin_trap();

	for (struct Map *x = (struct Map *) ((char *) hdr - hdr->maps_offset),
				*end = x + hdr->num_maps; x < end; ++x)
		if (x->type & VMA_SHOULD_DUMP && !restore_map(x)) __builtin_trap();

	int fd;
	if ((fd = _open("dump", O_RDONLY)) < 0) __builtin_trap();
	struct iovec *iov_end = (struct iovec *) hdr, *iov = iov_end - hdr->num_iovs;
	do {
		ssize_t n;
		if ((n = _readv(fd, iov, iov_end - iov)) < 0) __builtin_trap();
		for (size_t k; n; n -= k) {
			k = MIN(iov->iov_len, (size_t) n);
			if (iov->iov_len -= k) iov->iov_base = (char *) iov->iov_base + k;
			else ++iov;
		}
	} while (iov < iov_end);
	_close(fd);

	__asm__ volatile ("lea rsp, [%[frame]+8]\n\t"
		"syscall"
		: : "a" (__NR_rt_sigreturn), [frame] "r" (hdr->frame) : "cc", "memory");
	unreachable();
}
#else
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <signal.h>
#include <string.h>
#include <elf.h>
#include <link.h>
#include <sys/rseq.h>
#include <sys/stat.h>

#define PM_FILE (1ull << 61) ///< Page is file-page or shared-anon.
#define PM_SWAP (1ull << 62)
#define PM_PRESENT (1ull << 63)

#define FOR_PHDRS(ehdr, phdr) \
	for (ElfW(Phdr) *phdr = (ElfW(Phdr) *) ((char *) (ehdr) + (ehdr)->e_phoff), \
				*_end = phdr + (ehdr)->e_phnum; phdr < _end; ++phdr)

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
	int path_ofs, n = sscanf(line, "%lx-%lx %c%c%c%c %lx %x:%x %lu %n",
		&start, &end, &r, &w, &x, &s, &offset,
		&dev_major, &dev_minor, &inode, &path_ofs);
	assert(n >= 10);
	// TODO Parse /proc/pid/smaps VmFlags

	int prot = PROT_NONE;
	if (r == 'r') prot |= PROT_READ;
	if (w == 'w') prot |= PROT_WRITE;
	if (x == 'x') prot |= PROT_EXEC;
	int flags = s == 's' ? MAP_SHARED : MAP_PRIVATE;
	assert(flags & MAP_PRIVATE && "cannot restore shared VMAs yet");

	char *pathname = line + path_ofs;
	size_t path_len = strlen(pathname);
	if (pathname[path_len - 1] == '\n') pathname[--path_len] = '\0';

	enum VmaType type = pathname[0] == '\0' ? type = VMA_REGULAR
		: !strcmp(pathname, "[vsyscall]") ? VMA_VSYSCALL
		: !strcmp(pathname, "[vdso]") ? VMA_REGULAR | (prot == PROT_READ ? VMA_VDSO : 0)
		: !strcmp(pathname, "[vvar]") || !strcmp(pathname, "[vvar_vclock]")
		? VMA_REGULAR | (prot == PROT_READ ? VMA_VVAR : 0)
		: !strcmp(pathname, "[stack]") ? VMA_REGULAR | VMA_STACK
		: VMA_FILE;
	if (!(type & VMA_FILE)) flags |= MAP_ANONYMOUS;
	*result = (struct Map) {
		.type = type, .start = start, .end = end, .offset = offset,
		.prot = prot, .flags = flags,
	};
	strcpy(result->pathname, pathname);
	return true;
}

static bool should_dump_page(enum VmaType type, uint64_t pme) {
	return pme & (PM_SWAP | PM_PRESENT)
		&& !(pme & PM_FILE && type & VMA_FILE); // COW?
}

static bool do_splice(int pipefd[static 2], int fd, struct iovec *iov, struct iovec *iov_end) {
	while (iov < iov_end) {
		ssize_t n;
		if ((n = vmsplice(pipefd[1], iov, iov_end - iov, SPLICE_F_GIFT)) < 0)
			return false;

		for (ssize_t m = n, k; m; m -= k)
			if ((k = splice(pipefd[0], NULL, fd, NULL, m, SPLICE_F_MOVE | SPLICE_F_MORE)) < 0)
				return false;

		for (size_t k; n; n -= k) {
			k = MIN(iov->iov_len, (size_t) n);
			if (iov->iov_len -= k) iov->iov_base = (char *) iov->iov_base + k;
			else ++iov;
		}
	}
	return true;
}

static void signal_handler([[maybe_unused]] int sig) {
	struct rt_sigframe *frame = (struct rt_sigframe *)
		((char *) __builtin_frame_address(0) + sizeof(long));

	FILE *f, *maps, *pagemap;
	if (!((f = fopen("checkpoint", "wb"))
			&& (maps = fopen("/proc/self/maps", "r"))
			&& (pagemap = fopen("/proc/self/pagemap", "rb")))) die("fopen failed");
	// Dump maps to disk
	unsigned num_maps = 0;
	struct iovec iov[IOV_MAX], *iov_end = iov;
	struct Map map;
	while (parse_map(maps, &map)) {
		++num_maps;
		fwrite(&map, sizeof map, 1, f);
		if (!(map.type & VMA_SHOULD_DUMP && map.prot & PROT_READ)) continue;

		uint64_t pme;
		fseek(pagemap, map.start / PAGE_SIZE * sizeof pme, SEEK_SET);
		for (uintptr_t p = map.start; p < map.end; p += PAGE_SIZE) {
			if (fread(&pme, sizeof pme, 1, pagemap) < 1) die("fread failed");
			if (!should_dump_page(map.type, pme)) continue;

			if ((uintptr_t) iov_end[-1].iov_base + iov_end[-1].iov_len == p)
				iov_end[-1].iov_len += PAGE_SIZE;
			else
				*iov_end++ = (struct iovec) { (void *) p, PAGE_SIZE };
			assert((size_t) (iov_end - iov) < LENGTH(iov));
		}
	}

	size_t num_iovs = iov_end - iov;
	fwrite(iov, sizeof *iov, num_iovs, f); // Serialize iovecs to restore with readv

	int fd;
	if ((fd = open("dump", O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR)) < 0)
		die("open failed");
	int pipefd[2];
	if (pipe(pipefd)) die("pipe failed");
	if (!do_splice(pipefd, fd, iov, iov_end)) die("do_splice failed");
	close(pipefd[0]);
	close(pipefd[1]);
	close(fd);
	fclose(pagemap);
	fclose(maps);

	struct CheckpointHdr hdr = {
		.num_maps = num_maps, .num_iovs = num_iovs,
		.mmap_min_addr = mmap_min_addr(),
		.maps_offset = num_maps * sizeof map + num_iovs * sizeof(struct iovec),
		.frame = frame,
	};
	fwrite(&hdr, sizeof hdr, 1, f);
	fclose(f);
	exit(0);
}

void checkpoint() {
	struct sigaction action;
	action.sa_handler = signal_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = 0;
	if (sigaction(SIGCONT, &action, NULL)) die("sigaction failed");

	raise(SIGCONT);
}

static int rseq(struct rseq *rseq, uint32_t rseq_len, int flags, uint32_t sig) {
	return syscall(__NR_rseq, rseq, rseq_len, flags, sig);
}

/** Finds start of region not intersecting union of current maps and @a xs. */
static uintptr_t restorer_mmap_hint(struct CheckpointHdr *hdr, size_t len) {
	FILE *f;
	if (!(f = fopen("/proc/self/maps", "r"))) return -1;
	struct Map *x = (struct Map *) ((char *) hdr - hdr->maps_offset),
		*xend = x + hdr->num_maps, y;
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
	assert(phdr->p_align <= (size_t) PAGE_SIZE);
	ElfW(Addr) mapstart = phdr->p_vaddr & ~(PAGE_SIZE - 1),
		dataend = phdr->p_vaddr + phdr->p_filesz,
		allocend = phdr->p_vaddr + phdr->p_memsz,
		mapend = ALIGN_UP(dataend, PAGE_SIZE),
		zeropage = dataend & ~(PAGE_SIZE - 1);
	ElfW(Off) mapoff = phdr->p_offset & ~(PAGE_SIZE - 1);

	int prot = (phdr->p_flags & PF_R ? PROT_READ : 0)
		| (phdr->p_flags & PF_W ? PROT_WRITE : 0)
		| (phdr->p_flags & PF_X ? PROT_EXEC : 0);
	prot |= PROT_WRITE;
	if (mmap(hint + mapstart, mapend - mapstart, prot,
			MAP_FIXED | MAP_PRIVATE, fd, mapoff) == MAP_FAILED)
		return false;

	if (!(prot & PROT_WRITE))
		mprotect(hint + zeropage, allocend - zeropage, prot | PROT_WRITE);
	memset(hint + dataend, 0, allocend - dataend);
	return true;
}

static ElfW(Addr) find_sym(ElfW(Ehdr) *hdr, const char *name, ElfW(Shdr) *shdrs, ElfW(Shdr) *dynsym) {
    const char *strings = (char *) hdr + shdrs[dynsym->sh_link].sh_offset;
    for (ElfW(Sym) *sym = (ElfW(Sym) *) ((char *) hdr + dynsym->sh_offset),
				*end = (ElfW(Sym) *) ((char *) sym + dynsym->sh_size);
			sym < end; ++sym)
        if (!strcmp(name, strings + sym->st_name)) return sym->st_value;
    return 0;
}

static restore_fn *image_load(int fd, struct CheckpointHdr *chdr, size_t *size, uintptr_t *hint) {
	struct stat statbuf;
	if (fstat(fd, &statbuf)) die("fstat failed");
	ElfW(Ehdr) *hdr;
	if ((hdr = mmap(NULL, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED)
		die("mmap failed");
	assert((size_t) statbuf.st_size >= sizeof *hdr);
	assert(!memcmp(hdr->e_ident, ELFMAG, SELFMAG) && "invalid ELF magic");
	assert(hdr->e_type == ET_DYN);

	ElfW(Phdr) *last_load = NULL;
	FOR_PHDRS(hdr, phdr) if (phdr->p_type == PT_LOAD) last_load = phdr;
	size_t maplength = last_load->p_vaddr + last_load->p_memsz;
	*size += ALIGN_UP(maplength, PAGE_SIZE);
	if (!(*hint = restorer_mmap_hint(chdr, *size))) die("restorer_mmap_hint failed");

    // Map PT_LOAD entries in the program header table
	FOR_PHDRS(hdr, phdr)
        if (phdr->p_type == PT_LOAD && !map_segment(fd, phdr, (char *) *hint))
			die("map_segment failed");

    // Section table
    ElfW(Shdr) *shdrs = (ElfW(Shdr) *) ((char *) hdr + hdr->e_shoff),
		*dynsym = NULL;
    for (ElfW(Shdr) *shdr = shdrs, *end = shdrs + hdr->e_shnum; shdr < end; ++shdr)
		switch (shdr->sh_type) {
		case SHT_DYNSYM: dynsym = shdr; break;
		}
	if (!dynsym) die("missing dynamic symbol table");
	return (restore_fn *) (*hint + find_sym(hdr, "do_restore", shdrs, dynsym));
}

static void unregister_rseq() {
#if defined(__GLIBC__) && defined(RSEQ_SIG)
	if (!__rseq_size) return;
	struct rseq *rseq_abi = (struct rseq *)
		((char *) __builtin_thread_pointer() + __rseq_offset);
	// glibc reports registered size, e.g. 20, but older kernels expect full 32
	uint32_t rseq_len = MAX(__rseq_size, sizeof *rseq_abi);
	int ret = rseq(rseq_abi, rseq_len, RSEQ_FLAG_UNREGISTER, RSEQ_SIG);
	assert(!ret && "unregistering rseq failed");
#endif
}

void restore() {
	int checkpoint_fd;
	if ((checkpoint_fd = open("checkpoint", O_RDONLY)) < 0) die("open failed");
	struct stat statbuf;
	if (fstat(checkpoint_fd, &statbuf)) die("fstat failed");
	char *info;
	if ((info = mmap(NULL, statbuf.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE,
				checkpoint_fd, 0)) == MAP_FAILED) die("mmap failed");
	close(checkpoint_fd);
	struct CheckpointHdr *hdr = (struct CheckpointHdr *) (info + statbuf.st_size) - 1;

	int fd;
	if ((fd = open(LIBRESTORE_SO, O_RDONLY)) < 0) die("open failed");
	size_t size = /* stack */ PAGE_SIZE + /* info */ statbuf.st_size;
	uintptr_t hint;
	restore_fn *f;
	if (!(f = image_load(fd, hdr, &size, &hint))) die("image_load failed");
	close(fd);

	char *new_info = (char *) (hint + size - statbuf.st_size);
	// Map restorer stack
	if (mmap(new_info - PAGE_SIZE, PAGE_SIZE, PROT_READ | PROT_WRITE,
			MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN, -1, 0) == MAP_FAILED)
		die("mmap failed");

	if (mremap(info, statbuf.st_size, statbuf.st_size, MREMAP_MAYMOVE | MREMAP_FIXED,
			new_info) == MAP_FAILED) die("mremap failed");
	struct CheckpointHdr *new_hdr = (struct CheckpointHdr *) (hint + size) - 1;

	unregister_rseq();

	__asm__ volatile ("mov rsp, %[sp]\n\t"
		"call %[f]"
		:
		: [f] "rm" (f), [sp] "irm" (new_info), "D" (hint), "S" (new_hdr)
		: "cc", "memory");
	unreachable();
}
#endif
