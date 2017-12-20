#include <alloca.h>
#include <assert.h>

#include "ast.h"
#include "interpret.h"
#include "resolver.h"
#include "scope.h"
#include "vars.h"

#define EXIT_CODE_RETURN (-1)
#define EXIT_CODE_FALLTHROUGH (-2)
#define EXIT_CODE_IF_BRANCH_TAKEN (-3)

static int interpret_stmts(struct program *program, int stmt_count, struct stmt *stmts, struct scope **scope);
static void interpret_routine(struct program *program, struct routine *routine, struct scope **scope, int arg_count, struct var **args);

static int interpret_stmt(struct program *program, struct stmt *stmt, struct scope **scope)
{
#if 0
	{
		char *stmt_type = 0;
		switch (stmt->stmt_type) {
		case stmt_let_eq: stmt_type = "LET="; break;
		case stmt_let_add_edge: stmt_type = "LET>"; break;
		case stmt_let_remove_edge: stmt_type = "LET<"; break;
		case stmt_if: stmt_type = "IF"; break;
		case stmt_call: stmt_type = "CALL"; break;
		case stmt_return: stmt_type = "RETURN"; break;
		case stmt_do_loop: stmt_type = "DO"; break;
		case stmt_do_edges: stmt_type = "DO<"; break;
		case stmt_exit: stmt_type = "EXIT"; break;
		case stmt_if_branch_eq: stmt_type = "IF="; break;
		case stmt_if_branch_edge: stmt_type = "IF>"; break;
		case stmt_if_branch_else: stmt_type = "ELSE"; break;
		default: assert(0);
		}
		printf("%s:%d %s\n", stmt->filename, stmt->line_number, stmt_type);
	}
#endif
	switch (stmt->stmt_type) {
	case stmt_let_eq:
		assert(stmt->arg_count == 2);
		assert(!stmt->stmts);
		assert(stmt->args[0] >= 0);
		struct var *rhs;
		if (stmt->args[1] >= 0) {
			rhs = scope_get(*scope, stmt->args[1]);
		} else {
			rhs = scope_new_var(*scope);
		}
		scope_set(*scope, stmt->args[0], rhs);
		return EXIT_CODE_FALLTHROUGH;
	case stmt_let_add_edge:
	{
		assert(stmt->arg_count == 2);
		assert(!stmt->stmts);
		assert(stmt->args[0] >= 0);
		struct var *rhs;
		if (stmt->args[1] >= 0) {
			rhs = scope_get(*scope, stmt->args[1]);
		} else {
			rhs = scope_new_var(*scope);
		}
		vars_add_edge(scope_get(*scope, stmt->args[0]), rhs);
		return EXIT_CODE_FALLTHROUGH;
	}
	case stmt_let_remove_edge:
	{
		assert(stmt->arg_count == 2);
		assert(!stmt->stmts);
		assert(stmt->args[0] >= 0);
		assert(stmt->args[1] >= 0);
		vars_remove_edge(scope_get(*scope, stmt->args[0]), scope_get(*scope, stmt->args[1]));
		return EXIT_CODE_FALLTHROUGH;
	}
	case stmt_if:
	{
		assert(stmt->arg_count == 0);
		assert(stmt->stmts);
		int exit_code = interpret_stmts(program, stmt->stmt_count, stmt->stmts, scope);
		if (exit_code == EXIT_CODE_IF_BRANCH_TAKEN) {
			return EXIT_CODE_FALLTHROUGH;
		}
		return exit_code;
	}
	case stmt_call:
	{
		assert(stmt->arg_count >= 2);
		assert(!stmt->stmts);
		assert(stmt->args[0] >= 0 && stmt->args[0] < program->module_count);
		assert(stmt->args[1] >= 0 && stmt->args[1] < program->modules[stmt->args[0]].routine_count);
		struct var **args = 0;
		if (stmt->arg_count > 2) {
			args = alloca((stmt->arg_count-2)*sizeof(struct var *));
			for (int i = 2; i < stmt->arg_count; i++) {
				if (stmt->args[i] >= 0) {
					args[i-2] = scope_get(*scope, stmt->args[i]);
				} else {
					args[i-2] = scope_new_var(*scope);
				}
			}
		}
		interpret_routine(program, &program->modules[stmt->args[0]].routines[stmt->args[1]], scope, stmt->arg_count - 2, args);
		return EXIT_CODE_FALLTHROUGH;
	}
	case stmt_return:
		assert(stmt->arg_count == 0);
		assert(!stmt->stmts);
		return EXIT_CODE_RETURN;
	case stmt_do_loop:
		assert(stmt->arg_count == 1);
		for (;;) {
			int exit_code = interpret_stmts(program, stmt->stmt_count, stmt->stmts, scope);
			if (exit_code == stmt->args[0]) {
				return EXIT_CODE_FALLTHROUGH;
			} else if (exit_code != EXIT_CODE_FALLTHROUGH) {
				return exit_code;
			}
			scope_gc(*scope);
		}
	case stmt_do_edges:
	{
		assert(stmt->arg_count == 2);
		assert(stmt->args[0] >= 0);
		assert(stmt->args[1] >= 0);
		scope_push_do_edges(*scope, scope_get(*scope, stmt->args[1]));
		int exit_code = EXIT_CODE_FALLTHROUGH;
		for (;;) {
			struct var *var = scope_pop_do_edge(*scope);
			if (!var) {
				exit_code = EXIT_CODE_FALLTHROUGH;
				break;
			}
			scope_set(*scope, stmt->args[0], var);
			exit_code = interpret_stmts(program, stmt->stmt_count, stmt->stmts, scope);
			if (exit_code == stmt->args[0]) {
				exit_code = EXIT_CODE_FALLTHROUGH;
				break;
			} else if (exit_code != EXIT_CODE_FALLTHROUGH) {
				break;
			}
		}
		scope_pop_do_edges(*scope);
		scope_gc(*scope);
		return exit_code;
	}
	case stmt_exit:
		assert(stmt->arg_count == 1);
		assert(!stmt->stmts);
		return stmt->args[0];
	case stmt_if_branch_eq:
	{
		assert(stmt->arg_count == 2);
		assert(stmt->args[0] >= 0);
		assert(stmt->args[1] >= 0);
		if (!vars_eq(scope_get(*scope, stmt->args[0]), scope_get(*scope, stmt->args[1]))) {
			return EXIT_CODE_FALLTHROUGH;
		}
		int exit_code = interpret_stmts(program, stmt->stmt_count, stmt->stmts, scope);
		if (exit_code == EXIT_CODE_FALLTHROUGH) {
			return EXIT_CODE_IF_BRANCH_TAKEN;
		}
		return exit_code;
	}
	case stmt_if_branch_edge:
	{
		assert(stmt->arg_count == 2);
		assert(stmt->args[0] >= 0);
		assert(stmt->args[1] >= 0);
		if (!vars_has_edge(scope_get(*scope, stmt->args[0]), scope_get(*scope, stmt->args[1]))) {
			return EXIT_CODE_FALLTHROUGH;
		}
		int exit_code = interpret_stmts(program, stmt->stmt_count, stmt->stmts, scope);
		if (exit_code == EXIT_CODE_FALLTHROUGH) {
			return EXIT_CODE_IF_BRANCH_TAKEN;
		}
		return exit_code;
	}
	case stmt_if_branch_else:
	{
		assert(stmt->arg_count == 0);
		int exit_code = interpret_stmts(program, stmt->stmt_count, stmt->stmts, scope);
		if (exit_code == EXIT_CODE_FALLTHROUGH) {
			return EXIT_CODE_IF_BRANCH_TAKEN;
		}
		return exit_code;
	}
	default:
		assert(0);
	}
	return EXIT_CODE_FALLTHROUGH;
}

static int interpret_stmts(struct program *program, int stmt_count, struct stmt *stmts, struct scope **scope)
{
	for (int i = 0; i < stmt_count; i++) {
		int exit_code = interpret_stmt(program, &stmts[i], scope);
		if (exit_code != EXIT_CODE_FALLTHROUGH) {
			return exit_code;
		}
	}
	return EXIT_CODE_FALLTHROUGH;
}

static void interpret_routine(struct program *program, struct routine *routine, struct scope **scope, int arg_count, struct var **args)
{
	assert(routine);
	if (routine->library_routine) {
		routine->library_routine(*scope, arg_count, args);
		scope_gc(*scope);
		return;
	}
	*scope = scope_push(*scope, routine->var_count);

	for (int i = 0; i < arg_count && i < routine->parameter_count; i++) {
		scope_refer(*scope, i, args[i]);
	}

	int exit_code = interpret_stmts(program, routine->stmt_count, routine->stmts, scope);
	assert(exit_code == EXIT_CODE_RETURN || exit_code == EXIT_CODE_FALLTHROUGH);

	*scope = scope_pop(*scope);
	if (*scope) {
		scope_gc(*scope);
	}
}

void interpret(struct program *program)
{
	struct scope *scope = 0;
	interpret_routine(program, &program->program, &scope, 0, 0);
	assert(!scope);
}
