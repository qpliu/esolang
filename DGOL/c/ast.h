#ifndef AST_H
#define AST_H

struct ast_name_list {
	char *name;
	struct ast_name_list *next_name;
};

enum stmt_type {
	stmt_let_eq,
	stmt_let_add_edge,
	stmt_let_remove_edge,
	stmt_if,
	stmt_call,
	stmt_return,
	stmt_do_loop,
	stmt_do_edges,
	stmt_exit,
	stmt_if_branch_eq,
	stmt_if_branch_edge,
	stmt_if_branch_else
};

struct ast_statement {
	enum stmt_type statement_type;
	struct ast_name_list *first_argument;
	struct ast_statement *first_child_statement;
	struct ast_statement *next_statement;
};

struct ast_routine {
	char *name;
	struct ast_name_list *first_parameter;
	struct ast_statement *first_statement;
	struct ast_routine *next_routine;
};

struct ast_module {
	char *name;
	struct ast_name_list *first_use;
	struct ast_routine *first_routine;
	struct ast_name_list *first_export;
	struct ast_routine *program;
	struct ast_module *next_ast_module;
};

#endif /* AST_H */
