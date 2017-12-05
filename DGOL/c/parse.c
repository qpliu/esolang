#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "mymalloc.h"
#include "parse.h"

#define LINE_BUFFER_LEN 1000

static int parse_read_line(FILE *file, char *line_buffer, int line_buffer_len, int eof_expected)
{
	int count = 0;
	int in_comment = 0;
	for (;;) {
		char c = fgetc(file);
		if (c == EOF) {
			line_buffer[count] = 0;
			if (eof_expected && count > 0) {
				fprintf(stderr, "UNEXPECTED TRAILING GARBAGE\n");
				exit(1);
			} else if (!eof_expected) {
				fprintf(stderr, "UNEXPECTED EOF\n");
				exit(1);
			}
			return count;
		}
		if (c == '\n') {
			if (count == 0) {
				in_comment = 0;
				continue;
			}
			if (eof_expected) {
				fprintf(stderr, "UNEXPECTED TRAILING GARBAGE\n");
				exit(1);
			}
			line_buffer[count] = 0;
			return count;
		}
		if (isspace(c) || in_comment) {
			continue;
		}
		if (c == '*') {
			in_comment = 1;
			continue;
		}
		line_buffer[count] = c;
		count++;
		if (count >= line_buffer_len) {
			fprintf(stderr, "LINE TOO LONG\n");
			exit(1);
		}
	}
}

static char *parse_strdup(char *line_buffer, int start_index, int end_index)
{
	char *str = mymalloc((end_index-start_index+1)*sizeof(char));
	memcpy(str, line_buffer+start_index, (end_index-start_index)*sizeof(char));
	str[end_index-start_index] = 0;
	return str;
}

static int parse_token_end_index(char *line_buffer, int line_len, int start_index)
{
	if (start_index >= line_len) {
		return line_len;
	}
	if (!isalnum(line_buffer[start_index])) {
		return start_index+1;
	}
	for (int i = start_index+1; i < line_len; i++) {
		if (!isalnum(line_buffer[i])) {
			return i;
		}
	}
	return line_len;
}

static int parse_is_identifier(char *line_buffer, int start_index, int end_index, int allow_0)
{
	if (start_index >= end_index) {
		return 0;
	}
	for (int i = start_index; i < end_index; i++) {
		if (!isalnum(line_buffer[i])) {
			return 0;
		}
	}
	if (!allow_0 && start_index+1 == end_index && line_buffer[start_index] == '0') {
		return 0;
	}
	return 1;
}

static struct ast_name_list *parse_use(struct ast_module *module, char *line_buffer, int line_len)
{
	if (line_len < 4 || line_buffer[0] != 'U' || line_buffer[1] != 'S' || line_buffer[2] != 'E') {
		return 0;
	}
	if (!parse_is_identifier(line_buffer, 3, line_len, 0)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	struct ast_name_list *use = mymalloc(sizeof(struct ast_name_list));
	use->name = parse_strdup(line_buffer, 3, line_len);
	return use;
}

static struct ast_name_list *parse_argument_list(char *line_buffer, int line_len, int start_index, int allow_0)
{
	if (line_buffer[start_index] != '(') {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	start_index++;
	if (start_index >= line_len) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	if (line_buffer[start_index] == ')') {
		if (start_index+1 < line_len) {
			fprintf(stderr, "SYNTAX ERROR\n");
			exit(1);
		}
		return 0;
	}
	struct ast_name_list *result = 0;
	struct ast_name_list **name = &result;
	for (;;) {
		if (start_index >= line_len) {
			fprintf(stderr, "SYNTAX ERROR\n");
			exit(1);
		}
		int end_index = parse_token_end_index(line_buffer, line_len, start_index);
		if (end_index >= line_len || !parse_is_identifier(line_buffer, start_index, end_index, allow_0)) {
			fprintf(stderr, "SYNTAX ERROR\n");
			exit(1);
		}
		*name = mymalloc(sizeof(struct ast_name_list));
		(*name)->name = parse_strdup(line_buffer, start_index, end_index);
		name = &(*name)->next_name;
		if (line_buffer[end_index] == ',') {
			start_index = end_index + 1;
		} else if (line_buffer[end_index] == ')' && end_index + 1 == line_len) {
			return result;
		} else {
			fprintf(stderr, "SYNTAX ERROR\n");
			exit(1);
		}
	}
}

static struct ast_statement *parse_if_branch(char *line_buffer, int line_len, int first_if_branch)
{
	int else_offset = 0;
	if (!first_if_branch) {
		else_offset = 4;
		if (line_len < 4 || line_buffer[0] != 'E' || line_buffer[1] != 'L' || line_buffer[2] != 'S' || line_buffer[3] != 'E') {
			fprintf(stderr, "SYNTAX ERROR\n");
			exit(1);
		}
		if (line_len == 4) {
			struct ast_statement *branch = mymalloc(sizeof(struct ast_statement));
			branch->statement_type = stmt_if_branch_else;
			return branch;
		}
	}
	if (line_len < else_offset + 5 || line_buffer[else_offset] != 'I' || line_buffer[else_offset+1] != 'F') {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	int cond_index = parse_token_end_index(line_buffer, line_len, else_offset+2);
	if (cond_index+2 > line_len || (line_buffer[cond_index] != '=' && line_buffer[cond_index] != '>') || !parse_is_identifier(line_buffer, else_offset+2, cond_index, 0) || !parse_is_identifier(line_buffer, cond_index+1, line_len, 0)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	struct ast_statement *branch = mymalloc(sizeof(struct ast_statement));
	if (line_buffer[cond_index] == '=') {
		branch->statement_type = stmt_if_branch_eq;
	} else if (line_buffer[cond_index] == '>') {
		branch->statement_type = stmt_if_branch_edge;
	} else {
		assert(0);
	}
	branch->first_argument = mymalloc(sizeof(struct ast_name_list));
	branch->first_argument->name = parse_strdup(line_buffer, else_offset+2, cond_index);
	branch->first_argument->next_name = mymalloc(sizeof(struct ast_name_list));
	branch->first_argument->next_name->name = parse_strdup(line_buffer, cond_index+1, line_len);
	return branch;
}

static int parse_statements(FILE *file, char *line_buffer, int line_buffer_len, struct ast_statement **first_statement)
{
	for (;;) {
		int line_len = parse_read_line(file, line_buffer, line_buffer_len, 0);
		if (line_len > 5 && line_buffer[0] == 'L' && line_buffer[1] == 'E' && line_buffer[2] == 'T') {
			int lhs_start_index = 3;
			int lhs_end_index = parse_token_end_index(line_buffer, line_len, lhs_start_index);
			if (lhs_end_index > line_len - 2 || !parse_is_identifier(line_buffer, lhs_start_index, lhs_end_index, 0) || !parse_is_identifier(line_buffer, lhs_end_index + 1, line_len, 1)) {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			enum stmt_type stmt_type;
			if (line_buffer[lhs_end_index] == '=') {
				stmt_type = stmt_let_eq;
			} else if (line_buffer[lhs_end_index] == '>') {
				stmt_type = stmt_let_add_edge;
			} else if (line_buffer[lhs_end_index] == '<' && parse_is_identifier(line_buffer,  lhs_end_index + 1, line_len, 0)) {
				stmt_type = stmt_let_remove_edge;
			} else {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			*first_statement = mymalloc(sizeof(struct ast_statement));
			(*first_statement)->statement_type = stmt_type;
			(*first_statement)->first_argument = mymalloc(sizeof(struct ast_name_list));
			(*first_statement)->first_argument->name = parse_strdup(line_buffer, lhs_start_index, lhs_end_index);
			(*first_statement)->first_argument->next_name = mymalloc(sizeof(struct ast_name_list));
			(*first_statement)->first_argument->next_name->name = parse_strdup(line_buffer, lhs_end_index+1, line_len);
			first_statement = &(*first_statement)->next_statement;
		} else if (line_len > 4 && line_buffer[0] == 'I' && line_buffer[1] == 'F') {
			*first_statement = mymalloc(sizeof(struct ast_statement));
			(*first_statement)->statement_type = stmt_if;
			int first_if_branch = 1;
			for (struct ast_statement **branch = &(*first_statement)->first_child_statement;;) {
				*branch = parse_if_branch(line_buffer, line_len, first_if_branch);
				line_len = parse_statements(file, line_buffer, line_buffer_len, &(*branch)->first_child_statement);
				if ((*branch)->statement_type == stmt_if_branch_else || line_len < 4 || line_buffer[0] != 'E' || line_buffer[1] != 'L' || line_buffer[2] != 'S' || line_buffer[3] != 'E') {
					break;
				}
				first_if_branch = 0;
				branch = &(*branch)->next_statement;
			}
			if (line_len != 5 || line_buffer[0] != 'E' || line_buffer[1] != 'N' || line_buffer[2] != 'D' || line_buffer[3] != 'I' || line_buffer[4] != 'F') {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			first_statement = &(*first_statement)->next_statement;
		} else if (line_len > 6 && line_buffer[0] == 'C' && line_buffer[1] == 'A' && line_buffer[2] == 'L' && line_buffer[3] == 'L') {
			char *module_name = 0;
			char *routine_name = 0;
			int name_start_index = 4;
			int name_end_index = parse_token_end_index(line_buffer, line_len, name_start_index);
			if (!parse_is_identifier(line_buffer, name_start_index, name_end_index, 0)) {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			routine_name = parse_strdup(line_buffer, name_start_index, name_end_index);
			if (name_end_index + 1 < line_len && line_buffer[name_end_index] == '.') {
				module_name = routine_name;
				name_start_index = name_end_index + 1;
				name_end_index = parse_token_end_index(line_buffer, line_len, name_start_index);
				if (!parse_is_identifier(line_buffer, name_start_index, name_end_index, 0)) {
					fprintf(stderr, "SYNTAX ERROR\n");
					exit(1);
				}
				routine_name = parse_strdup(line_buffer, name_start_index, name_end_index);
			}
			*first_statement = mymalloc(sizeof(struct ast_statement));
			(*first_statement)->statement_type = stmt_call;
			(*first_statement)->first_argument = mymalloc(sizeof(struct ast_name_list));
			(*first_statement)->first_argument->name = module_name;
			(*first_statement)->first_argument->next_name = mymalloc(sizeof(struct ast_name_list));
			(*first_statement)->first_argument->next_name->name = routine_name;
			(*first_statement)->first_argument->next_name->next_name = parse_argument_list(line_buffer, line_len, name_end_index, 1);
			first_statement = &(*first_statement)->next_statement;
		} else if (line_len == 6 && line_buffer[0] == 'R' && line_buffer[1] == 'E' && line_buffer[2] == 'T' && line_buffer[3] == 'U' && line_buffer[4] == 'R' && line_buffer[5] == 'N') {
			*first_statement = mymalloc(sizeof(struct ast_statement));
			(*first_statement)->statement_type = stmt_return;
			first_statement = &(*first_statement)->next_statement;
		} else if (line_len > 2 && line_buffer[0] == 'D' && line_buffer[1] == 'O') {
			int index = parse_token_end_index(line_buffer, line_len, 2);
			if (index == line_len && parse_is_identifier(line_buffer, 2, index, 0)) {
				*first_statement = mymalloc(sizeof(struct ast_statement));
				(*first_statement)->statement_type = stmt_do_loop;
				(*first_statement)->first_argument = mymalloc(sizeof(struct ast_name_list));
				(*first_statement)->first_argument->name = parse_strdup(line_buffer, 2, index);
			} else if (index < line_len - 1 && line_buffer[index] == '<' && parse_is_identifier(line_buffer, index+1, line_len, 0)) {
				*first_statement = mymalloc(sizeof(struct ast_statement));
				(*first_statement)->statement_type = stmt_do_edges;
				(*first_statement)->first_argument = mymalloc(sizeof(struct ast_name_list));
				(*first_statement)->first_argument->name = parse_strdup(line_buffer, 2, index);
				(*first_statement)->first_argument->next_name = mymalloc(sizeof(struct ast_name_list));
				(*first_statement)->first_argument->next_name->name = parse_strdup(line_buffer, index+1, line_len);
			} else {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			line_len = parse_statements(file, line_buffer, line_buffer_len, &(*first_statement)->first_child_statement);
			if (line_len != 5 || line_buffer[0] != 'E' || line_buffer[1] != 'N' || line_buffer[2] != 'D' || line_buffer[3] != 'D' || line_buffer[4] != 'O') {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			first_statement = &(*first_statement)->next_statement;
		} else if (line_len > 4 && line_buffer[0] == 'E' && line_buffer[1] == 'X' && line_buffer[2] == 'I' && line_buffer[3] == 'T') {
			if (!parse_is_identifier(line_buffer, 4, line_len, 0)) {
				fprintf(stderr, "SYNTAX ERROR\n");
				exit(1);
			}
			*first_statement = mymalloc(sizeof(struct ast_statement));
			(*first_statement)->statement_type = stmt_exit;
			(*first_statement)->first_argument = mymalloc(sizeof(struct ast_name_list));
			(*first_statement)->first_argument->name = parse_strdup(line_buffer, 4, line_len);
			first_statement = &(*first_statement)->next_statement;
		} else {
			return line_len;
		}
	}
}

static struct ast_routine *parse_subroutine(FILE *file, char *line_buffer, int line_buffer_len, int line_len)
{
	if (line_len < 13 || line_buffer[0] != 'S' || line_buffer[1] != 'U' || line_buffer[2] != 'B' || line_buffer[3] != 'R' || line_buffer[4] != 'O' || line_buffer[5] != 'U' || line_buffer[6] != 'T' || line_buffer[7] != 'I' || line_buffer[8] != 'N' || line_buffer[9] != 'E') {
		return 0;
	}
	int start_index = 10;
	int end_index = parse_token_end_index(line_buffer, line_len, start_index);
	if (!parse_is_identifier(line_buffer, start_index, end_index, 0)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}

	struct ast_routine *routine = mymalloc(sizeof(struct ast_routine));
	routine->name = parse_strdup(line_buffer, start_index, end_index);
	routine->first_parameter = parse_argument_list(line_buffer, line_len, end_index, 0);

	line_len = parse_statements(file, line_buffer, line_buffer_len, &routine->first_statement);
	if (line_len < 4 || line_buffer[0] != 'E' || line_buffer[1] != 'N' || line_buffer[2] != 'D' || strcmp(routine->name, line_buffer + 3)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	return routine;
}

static int parse_exports(FILE *file, char *line_buffer, int line_buffer_len, int line_len, struct ast_module *module)
{
	if (line_len < 8 || line_buffer[0] != 'L' || line_buffer[1] != 'I' || line_buffer[2] != 'B' || line_buffer[3] != 'R' || line_buffer[4] != 'A' || line_buffer[5] != 'R' || line_buffer[6] != 'Y') {
		return 0;
	}
	if (!parse_is_identifier(line_buffer, 7, line_len, 0)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	module->name = parse_strdup(line_buffer, 7, line_len);

	for (struct ast_name_list **export = &module->first_export;;) {
		line_len = parse_read_line(file, line_buffer, line_buffer_len, 0);
		if (line_len > 10 && line_buffer[0] == 'S' && line_buffer[1] == 'U' && line_buffer[2] == 'B' && line_buffer[3] == 'R' && line_buffer[4] == 'O' && line_buffer[5] == 'U' && line_buffer[6] == 'T' && line_buffer[7] == 'I' && line_buffer[8] == 'N' && line_buffer[9] == 'E' && parse_is_identifier(line_buffer, 10, line_len, 0)) {
			*export = mymalloc(sizeof(struct ast_name_list));
			(*export)->name = parse_strdup(line_buffer, 10, line_len);
			export = &(*export)->next_name;
		} else if (line_len > 3 && line_buffer[0] == 'E' && line_buffer[1] == 'N' && line_buffer[2] == 'D' && !strcmp(module->name, line_buffer + 3)) {
			return 1;
		} else {
			fprintf(stderr, "SYNTAX ERROR\n");
			exit(1);
		}
	}
	return 1;
}

static int parse_program(FILE *file, char *line_buffer, int line_buffer_len, int line_len, struct ast_module *module)
{
	if (line_len < 8 || line_buffer[0] != 'P' || line_buffer[1] != 'R' || line_buffer[2] != 'O' || line_buffer[3] != 'G' || line_buffer[4] != 'R' || line_buffer[5] != 'A' || line_buffer[6] != 'M') {
		return 0;
	}
	if (!parse_is_identifier(line_buffer, 7, line_len, 0)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	module->name = parse_strdup(line_buffer, 7, line_len);
	module->program = mymalloc(sizeof(struct ast_routine));
	line_len = parse_statements(file, line_buffer, line_buffer_len, &module->program->first_statement);
	if (line_len < 4 || line_buffer[0] != 'E' || line_buffer[1] != 'N' || line_buffer[2] != 'D' || strcmp(module->name, line_buffer + 3)) {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	return 1;
}

struct ast_module *parse(FILE *file)
{
	struct ast_module *module = mymalloc(sizeof(struct ast_module));

	char line_buffer[LINE_BUFFER_LEN];
	int line_len = parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 0);
	for (struct ast_name_list **use = &module->first_use;;) {
		*use = parse_use(module, line_buffer, line_len);
		if (!*use) {
			break;
		}
		use = &(*use)->next_name;
		line_len = parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 0);
	}

	for (struct ast_routine **routine = &module->first_routine;;) {
		*routine = parse_subroutine(file, line_buffer, LINE_BUFFER_LEN, line_len);
		if (!*routine) {
			break;
		}
		routine = &(*routine)->next_routine;
		line_len = parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 0);
	}

	if (parse_exports(file, line_buffer, LINE_BUFFER_LEN, line_len, module)) {
		parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 1);
	} else if (parse_program(file, line_buffer, LINE_BUFFER_LEN, line_len, module)) {
		parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 1);
	} else {
		fprintf(stderr, "SYNTAX ERROR\n");
		exit(1);
	}
	return module;
}

static void parse_free_ast_name_list(struct ast_name_list *name_list)
{
	while (name_list) {
		struct ast_name_list *name = name_list;
		name_list = name_list->next_name;
		if (name->name) {
			myfree(name->name);
		}
		myfree(name);
	}
}

static void parse_free_ast_statements(struct ast_statement *statements)
{
	while (statements) {
		struct ast_statement *statement = statements;
		statements = statements->next_statement;
		parse_free_ast_name_list(statement->first_argument);
		parse_free_ast_statements(statement->first_child_statement);
		myfree(statement);
	}
}

static void parse_free_ast_routines(struct ast_routine *routines)
{
	while (routines) {
		struct ast_routine *routine = routines;
		routines = routines->next_routine;
		if (routine->name) {
			myfree(routine->name);
		}
		parse_free_ast_name_list(routine->first_parameter);
		parse_free_ast_statements(routine->first_statement);
		myfree(routine);
	}
}

void parse_free_ast_modules(struct ast_module *modules)
{
	while (modules) {
		struct ast_module *module = modules;
		modules = modules->next_ast_module;
		myfree(module->name);
		parse_free_ast_name_list(module->first_use);
		parse_free_ast_routines(module->first_routine);
		parse_free_ast_name_list(module->first_export);
		parse_free_ast_routines(module->program);
		myfree(module);
	}
}
