#ifndef DGOL_LIBS_H
#define DGOL_LIBS_H

#include "scope.h"

struct dgol_lib_routine {
	char *name;
	void (*routine)(struct scope *scope, int arg_count, struct var **args);
};

struct dgol_lib_module {
	char *name;
	int routine_count;
	struct dgol_lib_routine *routines;
};

struct dgol_lib {
	int module_count;
	struct dgol_lib_module *modules;
};

struct dgol_lib *dgol_lib();

void dgol_lib_free(struct dgol_lib *dgol_lib);

#endif /* DGOL_LIBS_H */
