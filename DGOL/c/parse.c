#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "mymalloc.h"
#include "parse.h"

#define LINE_BUFFER_LEN 1000

static int parse_read_line(FILE *file, char *line_buffer, int line_buffer_len)
{
	int count = 0;
	int in_comment = 0;
	for (;;) {
		char c = fgetc(file);
		if (c == EOF || c == '\n') {
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

struct ast_module *parse(FILE *file)
{
	char line_buffer[LINE_BUFFER_LEN];
	//...
	return 0;
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
