#ifndef RESOLVER_H
#define RESOLVER_H

#include "ast.h"
#include "dgol_libs.h"
#include "scope.h"

struct stmt {
	enum stmt_type stmt_type;
	char *filename;
	int line_number;
	int arg_count;
	int *args;
	int stmt_count;
	struct stmt *stmts;
};

struct routine {
	int parameter_count;
	int var_count;
	int stmt_count;
	struct stmt *stmts;
	void (*library_routine)(struct scope *scope, int arg_count, struct var **args);
};

struct module {
	int routine_count;
	struct routine *routines;
};

struct program {
	int module_count;
	struct module *modules;
	struct routine program;
};

struct program *resolve(struct ast_module *modules, struct dgol_lib *libs);

void resolver_free_program(struct program *program);

#endif /* RESOLVER_H */
