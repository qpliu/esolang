#include <assert.h>
#include <stdio.h>

#include "dgol_libs.h"
#include "scope.h"

static void dgol_lib_io_readbyte(struct scope *scope, int arg_count, struct var **args)
{
	int c = getchar();
	if (c == EOF) {
		if (arg_count > 1) {
			vars_add_edge(args[0], args[1]);
		}
		return;
	}
	if (arg_count > 1) {
		vars_remove_edge(args[0], args[1]);
	}
	if (arg_count > 2) {
		if (c & 1) {
			vars_add_edge(args[0], args[2]);
		} else {
			vars_remove_edge(args[0], args[2]);
		}
	}
	if (arg_count > 3) {
		if (c & 2) {
			vars_add_edge(args[0], args[3]);
		} else {
			vars_remove_edge(args[0], args[3]);
		}
	}
	if (arg_count > 4) {
		if (c & 4) {
			vars_add_edge(args[0], args[4]);
		} else {
			vars_remove_edge(args[0], args[4]);
		}
	}
	if (arg_count > 5) {
		if (c & 8) {
			vars_add_edge(args[0], args[5]);
		} else {
			vars_remove_edge(args[0], args[5]);
		}
	}
	if (arg_count > 6) {
		if (c & 16) {
			vars_add_edge(args[0], args[6]);
		} else {
			vars_remove_edge(args[0], args[6]);
		}
	}
	if (arg_count > 7) {
		if (c & 32) {
			vars_add_edge(args[0], args[7]);
		} else {
			vars_remove_edge(args[0], args[7]);
		}
	}
	if (arg_count > 8) {
		if (c & 64) {
			vars_add_edge(args[0], args[8]);
		} else {
			vars_remove_edge(args[0], args[8]);
		}
	}
	if (arg_count > 9) {
		if (c & 128) {
			vars_add_edge(args[0], args[9]);
		} else {
			vars_remove_edge(args[0], args[9]);
		}
	}
}

static void dgol_lib_io_writebyte(struct scope *scope, int arg_count, struct var **args)
{
	int c = 0;
	if (arg_count > 1 && vars_has_edge(args[0], args[1])) {
		c |= 1;
	}
	if (arg_count > 2 && vars_has_edge(args[0], args[2])) {
		c |= 2;
	}
	if (arg_count > 3 && vars_has_edge(args[0], args[3])) {
		c |= 4;
	}
	if (arg_count > 4 && vars_has_edge(args[0], args[4])) {
		c |= 8;
	}
	if (arg_count > 5 && vars_has_edge(args[0], args[5])) {
		c |= 16;
	}
	if (arg_count > 6 && vars_has_edge(args[0], args[6])) {
		c |= 32;
	}
	if (arg_count > 7 && vars_has_edge(args[0], args[7])) {
		c |= 64;
	}
	if (arg_count > 8 && vars_has_edge(args[0], args[8])) {
		c |= 128;
	}
	putchar(c);
}

static struct dgol_lib_routine DGOL_LIB_IO_ROUTINES[] = {
	{ "READBYTE", dgol_lib_io_readbyte },
	{ "WRITEBYTE", dgol_lib_io_writebyte }
};

static struct dgol_lib_module DGOL_LIB_MODULES[] = {
	{ "IO", 2, DGOL_LIB_IO_ROUTINES }
};

static struct dgol_lib DGOL_LIB = {
	1,
	DGOL_LIB_MODULES
};

struct dgol_lib *dgol_lib()
{
	return &DGOL_LIB;
}

void dgol_lib_free(struct dgol_lib *dgol_lib)
{
	/* statically allocated */
	assert(dgol_lib == &DGOL_LIB);
}
