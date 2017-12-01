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

static int parse_use(struct ast_module *module, char *line_buffer, int line_len)
{
	if (line_len < 4 || line_buffer[0] != 'U' || line_buffer[1] != 'S' || line_buffer[2] != 'E') {
		return 0;
	}
	struct ast_name_list **use = &module->first_use;
	while (*use) {
		use = &(*use)->next_name;
	}
	*use = mymalloc(sizeof(struct ast_name_list));
	(*use)->name = parse_strdup(line_buffer, 3, line_len);
	return 1;
}

static int parse_subroutine(FILE *file, struct ast_module *module, char *line_buffer, int line_buffer_len, int line_len)
{
	if (line_len < 13 || line_buffer[0] != 'S' || line_buffer[1] != 'U' || line_buffer[2] != 'B' || line_buffer[3] != 'R' || line_buffer[4] != 'O' || line_buffer[5] != 'U' || line_buffer[6] != 'T' || line_buffer[7] != 'I' || line_buffer[8] != 'N' || line_buffer[9] != 'E') {
		return 0;
	}
	//...
	return 1;
}

static int parse_exports(FILE *file, struct ast_module *module, char *line_buffer, int line_buffer_len, int line_len)
{
	if (line_len < 8 || line_buffer[0] != 'L' || line_buffer[1] != 'I' || line_buffer[2] != 'B' || line_buffer[3] != 'R' || line_buffer[4] != 'A' || line_buffer[5] != 'R' || line_buffer[6] != 'Y') {
		return 0;
	}
	//...
	return 1;
}

static int parse_program(FILE *file, struct ast_module *module, char *line_buffer, int line_buffer_len, int line_len)
{
	if (line_len < 8 || line_buffer[0] != 'P' || line_buffer[1] != 'R' || line_buffer[2] != 'O' || line_buffer[3] != 'G' || line_buffer[4] != 'R' || line_buffer[5] != 'A' || line_buffer[6] != 'M') {
		return 0;
	}
	//...
	return 1;
}

struct ast_module *parse(FILE *file)
{
	struct ast_module *module = mymalloc(sizeof(struct ast_module));

	char line_buffer[LINE_BUFFER_LEN];
	int line_len = parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 0);
	for (;;) {
		if (!parse_use(module, line_buffer, line_len)) {
			break;
		}
		line_len = parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 0);
	}

	for (;;) {
		if (!parse_subroutine(file, module, line_buffer, LINE_BUFFER_LEN, line_len)) {
			break;
		}
		line_len = parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 0);
	}

	if (parse_exports(file, module, line_buffer, LINE_BUFFER_LEN, line_len)) {
		parse_read_line(file, line_buffer, LINE_BUFFER_LEN, 1);
	} else if (parse_program(file, module, line_buffer, LINE_BUFFER_LEN, line_len)) {
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
		myfree(routine->name);
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
