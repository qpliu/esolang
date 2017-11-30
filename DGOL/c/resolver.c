#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

struct do_label_stack {
	int do_label_index;
	struct do_label_stack *parent;
};

static void resolve_stmts(int *stmt_count, struct stmt **stmts, struct do_label_stack *do_label_stack, struct ast_statement *first_statement, int module_index, int module_count, struct indexer *module_indexer, struct indexer **routine_indexer, struct indexer **public_routine_set, struct indexer *scope_indexer, int is_program_body, struct indexer *use_set)
{
	*stmt_count = 0;
	for (struct ast_statement *ast_stmt = first_statement; ast_stmt; ast_stmt = ast_stmt->next_statement) {
		(*stmt_count)++;
	}
	if (*stmt_count == 0) {
		*stmts = 0;
		return;
	}
	*stmts = mymalloc(*stmt_count*sizeof(struct stmt));
	int i = 0;
	for (struct ast_statement *ast_stmt = first_statement; ast_stmt; ast_stmt = ast_stmt->next_statement) {
		struct stmt *stmt = &*stmts[i];
		i++;
		stmt->stmt_type = ast_stmt->statement_type;
		stmt->arg_count = 0;
		for (struct ast_name_list *arg = ast_stmt->first_argument; arg; arg = arg->next_name) {
			stmt->arg_count++;
		}
		if (stmt->arg_count == 0) {
			stmt->args = 0;
		} else {
			stmt->args = mymalloc(stmt->arg_count*sizeof(int));
		}
		int j = 0;
		for (struct ast_name_list *arg = ast_stmt->first_argument; arg; arg = arg->next_name) {
			switch (stmt->stmt_type) {
			case stmt_let_eq:
			case stmt_let_add_edge:
				assert(stmt->arg_count == 2);
				if (j == 1 && !strcmp(arg->name, "0")) {
					stmt->args[j] = -1;
				} else {
					stmt->args[j] = indexer_index(scope_indexer, arg->name);
				}
				break;
			case stmt_let_remove_edge:
			case stmt_if_branch_eq:
			case stmt_if_branch_edge:
			case stmt_do_edges:
				assert(stmt->arg_count == 2);
				stmt->args[j] = indexer_index(scope_indexer, arg->name);
				break;
			case stmt_call:
				assert(stmt->arg_count >= 2);
				if (j == 0) {
					if (arg->name) {
						if (indexer_find_index(use_set, arg->name) < 0) {
							fprintf(stderr, "CALL UNUSED MODULE %s\n", arg->name);
							exit(1);
						}
						stmt->args[j] = indexer_find_index(module_indexer, arg->name);
						if (stmt->args[j] < 0) {
							fprintf(stderr, "CALL UNKNOWN MODULE %s\n", arg->name);
							exit(1);
						}
					} else {
						stmt->args[j] = module_index;
					}
					assert(stmt->args[j] < module_count);
				} else if (j == 1) {
					stmt->args[j] = indexer_find_index(routine_indexer[stmt->args[0]], arg->name);
					if (stmt->args[j] < 0) {
						fprintf(stderr, "CALL UNKNOWN SUBROUTINE %s\n", arg->name);
						exit(1);
					}
					if (stmt->args[0] != module_index && indexer_find_index(public_routine_set[stmt->args[0]], arg->name) < 0) {
						fprintf(stderr, "CALL UNKNOWN SUBROUTINE %s\n", arg->name);
						exit(1);
					}
				} else {
					stmt->args[j] = indexer_index(scope_indexer, arg->name);
				}
				break;
			case stmt_do_loop:
			case stmt_exit:
				assert(stmt->arg_count == 1);
				stmt->args[j] = indexer_index(scope_indexer, arg->name);
				break;
			case stmt_return:
			case stmt_if:
			case stmt_if_branch_else:
				assert(stmt->arg_count == 0);
			default:
				assert(0);
				break;
			}
			j++;
		}
		switch (stmt->stmt_type) {
		case stmt_let_eq:
		case stmt_let_add_edge:
		case stmt_let_remove_edge:
		case stmt_call:
			assert(!ast_stmt->first_child_statement);
			break;
		case stmt_return:
			assert(!ast_stmt->first_child_statement);
			if (is_program_body) {
				fprintf(stderr, "INVALID RETURN\n");
				exit(1);
			}
			break;
		case stmt_if:
		case stmt_if_branch_eq:
		case stmt_if_branch_edge:
		case stmt_if_branch_else:
			resolve_stmts(&stmt->stmt_count, &stmt->stmts, do_label_stack, ast_stmt->first_child_statement, module_index, module_count, module_indexer, routine_indexer, public_routine_set, scope_indexer, is_program_body, use_set);
			break;
		case stmt_do_edges:
		case stmt_do_loop:
		{
			struct do_label_stack push_do_label_stack = { stmt->args[0], do_label_stack };
			resolve_stmts(&stmt->stmt_count, &stmt->stmts, &push_do_label_stack, ast_stmt->first_child_statement, module_index, module_count, module_indexer, routine_indexer, public_routine_set, scope_indexer, is_program_body, use_set);
			break;
		}
		case stmt_exit:
			assert(!ast_stmt->first_child_statement);
			int do_label_found = 0;
			for (struct do_label_stack *s = do_label_stack; s; s = s->parent) {
				if (stmt->args[0] == s->do_label_index) {
					do_label_found = 1;
					break;
				}
			}
			if (!do_label_found) {
				fprintf(stderr, "INVALID EXIT\n");
				exit(1);
			}
			break;
		default:
			assert(0);
			break;
		}
	}
}

static void resolve_routine(struct routine *routine, struct ast_routine *ast_routine, int module_index, int module_count, struct indexer *module_indexer, struct indexer **routine_indexer, struct indexer **public_routine_set, int is_program_body, struct indexer *use_set)
{
	struct indexer *scope_indexer = indexer_new();
	for (struct ast_name_list *parameter = ast_routine->first_parameter; parameter; parameter = parameter->next_name) {
		int parameter_index = indexer_index_unique(scope_indexer, parameter->name);
		if (parameter_index < 0) {
			fprintf(stderr, "DUPLICATE PARAMETER NAME %s FOR %s\n", parameter->name, ast_routine->name);
			exit(1);
		}
	}
	routine->parameter_count = indexer_count(scope_indexer);
	resolve_stmts(&routine->stmt_count, &routine->stmts, 0, ast_routine->first_statement, module_index, module_count, module_indexer, routine_indexer, public_routine_set, scope_indexer, is_program_body, use_set);
	routine->var_count = indexer_count(scope_indexer);

	indexer_free(scope_indexer);
}

static void resolve_module(struct program *program, int module_index, struct ast_module *ast_module, struct indexer *module_indexer, struct indexer **routine_indexer, struct indexer **public_routine_set)
{
	struct indexer *use_set = indexer_new();
	for (struct ast_name_list *use = ast_module->first_use; use; use = use->next_name) {
		int use_index = indexer_index_unique(use_set, use->name);
		if (use_index < 0) {
			fprintf(stderr, "DUPLICATE USE %s\n", use->name);
			exit(1);
		}
	}
	int routine_count = indexer_count(routine_indexer[module_index]);
	program->modules[module_index].routine_count = routine_count;
	program->modules[module_index].routines = mymalloc(routine_count*sizeof(struct routine));
	for (struct ast_routine *ast_routine = ast_module->first_routine; ast_routine; ast_routine = ast_routine->next_routine) {
		int routine_index = indexer_find_index(routine_indexer[module_index], ast_routine->name);
		assert(routine_index >= 0);
		resolve_routine(&program->modules[module_index].routines[routine_index],  ast_routine, module_index, program->module_count, module_indexer, routine_indexer, public_routine_set, 0, use_set);
	}
	indexer_free(use_set);
}

static void resolve_lib_module(struct program *program, int module_index, struct dgol_lib_module *lib_module, struct indexer **routine_indexer)
{
	program->modules[module_index].routine_count = lib_module->routine_count;
	program->modules[module_index].routines = mymalloc(lib_module->routine_count*sizeof(struct routine));
	for (int i = 0; i < lib_module->routine_count; i++) {
		int routine_index = indexer_find_index(routine_indexer[module_index], lib_module->routines[i].name);
		assert(routine_index >= 0);
		program->modules[module_index].routines[routine_index].library_routine = lib_module->routines[i].routine;
	}
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
		resolve_module(program, module_index, m, module_indexer, routine_indexer, public_routine_set);
	}
	for (int i = 0; i < libs->module_count; i++) {
		int module_index = indexer_find_index(module_indexer, libs->modules[i].name);
		assert(module_index >= 0);
		if (indexer_find_index(lib_module_set, libs->modules[i].name) >= 0) {
			resolve_lib_module(program, module_index, &libs->modules[i], routine_indexer);
		}
	}

	struct indexer *use_set = indexer_new();
	for (struct ast_name_list *use = program_ast_module->first_use; use; use = use->next_name) {
		int use_index = indexer_index_unique(use_set, use->name);
		if (use_index < 0) {
			fprintf(stderr, "DUPLICATE USE %s\n", use->name);
			exit(1);
		}
	}

	resolve_routine(&program->program, program_ast_module->program, program_index, module_count, module_indexer, routine_indexer, public_routine_set, 1, use_set);

	for (int i = 0; i < module_count; i++) {
		indexer_free(routine_indexer[i]);
		indexer_free(public_routine_set[i]);
	}
	myfree(routine_indexer);
	myfree(public_routine_set);
	indexer_free(use_set);
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
