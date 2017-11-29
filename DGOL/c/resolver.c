#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "dgol_libs.h"
#include "indexer.h"
#include "mymalloc.h"
#include "resolver.h"

static void resolver_index_routines(struct ast_module *module, struct indexer *routine_indexer, struct indexer *public_routine_set)
{
	assert(module);
	for (struct ast_routine *routine = module->first_routine; routine; routine = routine->next_routine) {
		if (indexer_index_unique(routine_indexer, routine->name) < 0) {
			fprintf(stderr, "MULTIPLE SUBROUTINES WITH NAME %s.%s\n", module->name, routine->name);
			exit(1);
		}
	}
	for (struct ast_name_list *export = module->first_export; export; export = export->next_name) {
		if (indexer_find_index(routine_indexer, export->name) < 0) {
			fprintf(stderr, "EXPORTING UNDEFINED SUBROUTINE %s.%s\n", module->name, export->name);
			exit(1);
		}
		if (indexer_index_unique(public_routine_set, export->name) < 0) {
			fprintf(stderr, "MULTIPLE EXPORT OF SUBROUTINE %s.%s\n", module->name, export->name);
			exit(1);
		}
	}
}

static void resolver_index_lib_routines(struct dgol_lib_module *module, struct indexer *routine_indexer, struct indexer *public_routine_set)
{
	assert(module);
	for (int i = 0; i < module->routine_count; i++) {
		int routine_index = indexer_index_unique(routine_indexer, module->routines[i].name);
		assert(routine_index >= 0);
		indexer_index_unique(public_routine_set, module->routines[i].name);
	}
}

static void resolve_routine(struct routine *routine, struct ast_routine *ast_routine, int module_index, int module_count, struct indexer **routine_indexer, struct indexer **public_routine_set)
{
	//...
}

static void resolve_module(struct program *program, int module_index, struct ast_module *ast_module, struct indexer **routine_indexer, struct indexer **public_routine_set)
{
	//...
}

static void resolve_lib_module(struct program *program, int module_index, struct dgol_lib_module *lib_module)
{
	//...
}

struct program *resolve(struct ast_module *modules, struct dgol_lib *libs)
{
	assert(modules);
	assert(libs);

	struct indexer *module_indexer = indexer_new();
	struct ast_module *program_ast_module = 0;
	int program_index = -1;
	for (struct ast_module *m = modules; m; m = m->next_ast_module) {
		int module_index = indexer_index_unique(module_indexer, m->name);
		if (module_index < 0) {
			fprintf(stderr, "MULTIPLE MODULES WITH NAME %s\n", m->name);
			exit(1);
		}
		if (m->program) {
			if (program_index >= 0) {
				fprintf(stderr, "MULTIPLE PROGRAM MODULES\n");
				exit(1);
			}
			program_index = module_index;
			program_ast_module = m;
		}
	}

	if (program_index < 0) {
		fprintf(stderr, "NO PROGRAM MODULE DEFINED\n");
		exit(1);
	}

	struct indexer *lib_module_set = indexer_new();
	for (int i = 0; i < libs->module_count; i++) {
		if (indexer_index_unique(module_indexer, libs->modules[i].name) >= 0) {
			int module_index = indexer_index_unique(lib_module_set, libs->modules[i].name);
			assert(module_index >= 0);
		}
	}

	int module_count = indexer_count(module_indexer);

	struct indexer **routine_indexer = mymalloc(module_count*sizeof(struct indexer *));
	struct indexer **public_routine_set = mymalloc(module_count*sizeof(struct indexer *));
	for (int i = 0; i < module_count; i++) {
		routine_indexer[i] = indexer_new();
		public_routine_set[i] = indexer_new();
	}
	for (struct ast_module *m = modules; m; m = m->next_ast_module) {
		int module_index = indexer_find_index(module_indexer, m->name);
		assert(module_index >= 0);
		resolver_index_routines(m, routine_indexer[module_index], public_routine_set[module_index]);
	}
	for (int i = 0; i < libs->module_count; i++) {
		int module_index = indexer_find_index(module_indexer, libs->modules[i].name);
		assert(module_index >= 0);
		if (indexer_find_index(lib_module_set, libs->modules[i].name) >= 0) {
			resolver_index_lib_routines(&libs->modules[i], routine_indexer[module_index], public_routine_set[module_index]);
		}
	}

	struct program *program = mymalloc(sizeof(struct program));
	program->module_count = module_count;
	for (struct ast_module *m = modules; m; m = m->next_ast_module) {
		int module_index = indexer_find_index(module_indexer, m->name);
		assert(module_index >= 0);
		resolve_module(program, module_index, m, routine_indexer, public_routine_set);
	}
	for (int i = 0; i < libs->module_count; i++) {
		int module_index = indexer_find_index(module_indexer, libs->modules[i].name);
		assert(module_index >= 0);
		if (indexer_find_index(lib_module_set, libs->modules[i].name) >= 0) {
			resolve_lib_module(program, module_index, &libs->modules[i]);
		}
	}

	resolve_routine(&program->program, program_ast_module->program, program_index, module_count, routine_indexer, public_routine_set);

	for (int i = 0; i < module_count; i++) {
		indexer_free(routine_indexer[i]);
		indexer_free(public_routine_set[i]);
	}
	myfree(routine_indexer);
	myfree(public_routine_set);
	indexer_free(lib_module_set);
	indexer_free(module_indexer);
	return program;
}

static void resolver_free_stmt(struct stmt *stmt)
{
	if (stmt->args) {
		myfree(stmt->args);
	}
	for (int i = 0; i < stmt->stmt_count; i++) {
		resolver_free_stmt(&stmt->stmts[i]);
	}
	if (stmt->stmts) {
		myfree(stmt->stmts);
	}
}

static void resolver_free_routine(struct routine *routine)
{
	for (int i = 0; i < routine->stmt_count; i++) {
		resolver_free_stmt(&routine->stmts[i]);
	}
	if (routine->stmts) {
		myfree(routine->stmts);
	}
}

static void resolver_free_module(struct module *module)
{
	for (int i = 0; i < module->routine_count; i++) {
		resolver_free_routine(&module->routines[i]);
	}
	if (module->routines) {
		myfree(module->routines);
	}
	myfree(module);
}

void resolver_free_program(struct program *program)
{
	assert(program);
	for (int i = 0; i < program->module_count; i++) {
		resolver_free_module(&program->modules[i]);
	}
	if (program->modules) {
		myfree(program->modules);
	}
	myfree(program);
}
