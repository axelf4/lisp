#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "util.h"

enum TermError {
	TERM_OK,
	TERM_ERR_TCGETATTR,
	TERM_ERR_TCSETATTR,
	TERM_ERR_FILE,
};

static struct termios orig_termios;

static void disable_raw_mode() {
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios)) puts("tcsetattr failed");
}

static FILE *open_terminfo_in_dir(const char *term, int dir_len, const char dir[static dir_len]) {
	char path[PATH_MAX];
	int n = snprintf(path, sizeof path, "%.*s/%c/%s", dir_len, dir, *term, term);
	if (!(0 <= n && n < (int) sizeof path)) return NULL;

	printf("Opening: '%s'\r\n", path);
	FILE *f = fopen(path, "rb");
	return f;
}

static FILE *open_terminfo_desc(const char *term) {
	size_t dir_len;
	const char *dir;

	if ((dir = getenv("TERMINFO")))
		return open_terminfo_in_dir(term, strlen(dir), dir);

	FILE *f;

	// TODO ~/.terminfo
	/* if ((f = open_terminfo_in_dir(term, sizeof ""))) */

	if ((dir = getenv("TERMINFO_DIRS"))) do {
		const char *end = strchr(dir, ':');
		if ((f = open_terminfo_in_dir(term, dir_len = end ? end - dir : (int) strlen(dir), dir)))
			return f;
		dir += end ? dir_len + 1 : dir_len;
	} while (*dir);
	return NULL;
}

static enum TermError parse_terminfo(FILE *f) {
	// Legacy format max size 4096
	uint8_t b[32768];
	size_t n = fread(b, sizeof *b, sizeof b, f);
	if (!feof(f)) return TERM_ERR_FILE;

	if (n < 6 * sizeof(int16_t)) return TERM_ERR_FILE;
	int16_t magic = b[0] | b[1] << 8;
	if (magic != 01036) { printf("bad magic: %d\n\r", magic); return TERM_ERR_FILE; }

	return TERM_OK;
}

static enum TermError term_init() {
	if (tcgetattr(STDIN_FILENO, &orig_termios)) return TERM_ERR_TCGETATTR;

	struct termios termios = orig_termios;
	cfmakeraw(&termios);
	atexit(disable_raw_mode);
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &termios)) return TERM_ERR_TCSETATTR;

	// Load terminfo (see terminfo(5))
	const char *term;
	if (!(term = getenv("TERM"))) return TERM_OK;

	FILE *f = open_terminfo_desc(term);
	if (f) {
		printf("Result of parsing terminfo: %d\n\r", parse_terminfo(f));
		fclose(f);
	} else {
		printf("error: %d\n\r", errno);
	}

	return TERM_OK;
}

static void redraw() {
	if (write(STDOUT_FILENO, "\x1b[2J\x1b[H", 7) == -1)
		UNREACHABLE("write failed\n");
}

void start_tui() {
	printf("\r\nResult of terminal init: %d\r\n", term_init());

	/*
	puts("Reading chars");
	char c;
	redraw();
	while (read(STDIN_FILENO, &c, 1) == 1 && c != 'q') {
		redraw();
		if (iscntrl(c)) {
			printf("%d\n", c);
		} else {
			printf("%d ('%c')\n", c, c);
		}
	}
	puts("Stop reading chars");
	*/
}
