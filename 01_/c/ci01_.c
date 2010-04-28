#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SYMBOL_LENGTH 1000

static int little_endian = 0;

#ifndef NDEBUG
static int malloc_count = 0;
static int free_count = 0;
#endif

static void *my_malloc(size_t size)
{
#ifndef NDEBUG
	malloc_count++;
#endif
	void *p = malloc(size);
	if (!p) {
		perror("malloc");
		exit(1);
	}
	return p;
}

#ifndef NDEBUG
static void my_free(void *p)
{
	free_count++;
	free(p);
}
#else
#define my_free free
#endif

/* data */

enum bit_list_type { ZERO, ONE, NIL, DELAYED };

struct literal_bit_list {
	enum bit_list_type bit;
	struct literal_bit_list *next_bit;
};

static struct literal_bit_list *new_literal_bit_list(enum bit_list_type bit)
{
	struct literal_bit_list *bits;
	assert(bit == ZERO || bit == ONE || bit == NIL);
	if (bit == NIL)
		return NULL;
	bits = my_malloc(sizeof(struct literal_bit_list));
	bits->bit = bit;
	bits->next_bit = NULL;
	return bits;
}

static struct literal_bit_list *append_literal_bit(struct literal_bit_list **bits, enum bit_list_type bit)
{
	while (*bits)
		bits = &(*bits)->next_bit;
	*bits = new_literal_bit_list(bit);
	return *bits;
}

#ifndef NDEBUG
static void free_literal_bit_list(struct literal_bit_list *bits)
{
	if (bits->next_bit)
		free_literal_bit_list(bits->next_bit);
	my_free(bits);
}
#endif

/* function definitions */

enum pattern_type {
	PATTERN_NIL, PATTERN_WILD, PATTERN_BOUND
};

struct pattern {
	struct pattern *next_pattern;
	enum pattern_type pattern_type;
	struct literal_bit_list *bits;
};

static struct pattern *new_pattern(enum pattern_type pattern_type, struct literal_bit_list *bits)
{
	struct pattern *pattern;

	pattern = my_malloc(sizeof(struct pattern));
	pattern->next_pattern = NULL;
	pattern->pattern_type = pattern_type;
	pattern->bits = bits;
	return pattern;
}

static void append_pattern(struct pattern **patterns, struct pattern *pattern)
{
	assert(!pattern->next_pattern);
	while (*patterns)
		patterns = &(*patterns)->next_pattern;
	*patterns = pattern;
}

#ifndef NDEBUG
static void free_pattern_list(struct pattern *pattern)
{
	if (pattern->next_pattern)
		free_pattern_list(pattern->next_pattern);
	if (pattern->bits)
		free_literal_bit_list(pattern->bits);
	my_free(pattern);
}
#endif

struct expr {
	struct expr *next_expr;
	enum {
		EXPR_BINDING, EXPR_LITERAL, EXPR_FUNCTION, EXPR_CONCAT
	} expr_type;
	union {
		int binding_index;
		struct literal_bit_list *literal_bits;
		struct {
			struct function *function;
			struct expr *params;
		} f;
		struct expr *concat_exprs;
	} data;
};

static struct expr *new_binding_expr(int binding_index)
{
	struct expr *expr = my_malloc(sizeof(struct expr));
	expr->next_expr = NULL;
	expr->expr_type = EXPR_BINDING;
	expr->data.binding_index = binding_index;
	return expr;
}

static struct expr *new_literal_expr(struct literal_bit_list *literal_bits)
{
	struct expr *expr = my_malloc(sizeof(struct expr));
	expr->next_expr = NULL;
	expr->expr_type = EXPR_LITERAL;
	expr->data.literal_bits = literal_bits;
	return expr;
}

static struct expr *new_function_expr(struct function *function)
{
	struct expr *expr = my_malloc(sizeof(struct expr));
	expr->next_expr = NULL;
	expr->expr_type = EXPR_FUNCTION;
	expr->data.f.function = function;
	expr->data.f.params = NULL;
	return expr;
}

static struct expr *new_concat_expr(struct expr *first_expr)
{
	struct expr *expr = my_malloc(sizeof(struct expr));
	assert(first_expr && !first_expr->next_expr);
	expr->next_expr = NULL;
	expr->expr_type = EXPR_CONCAT;
	expr->data.concat_exprs = first_expr;
	return expr;
}

static void append_expr(struct expr **exprs, struct expr *expr)
{
	assert(!expr->next_expr);
	while (*exprs)
		exprs = &(*exprs)->next_expr;
	*exprs = expr;
}

#ifndef NDEBUG
static void free_expr_list(struct expr *expr)
{
	if (expr->next_expr)
		free_expr_list(expr->next_expr);
	switch (expr->expr_type) {
	case EXPR_LITERAL:
		if (expr->data.literal_bits)
			free_literal_bit_list(expr->data.literal_bits);
		break;
	case EXPR_FUNCTION:
		if (expr->data.f.params)
			free_expr_list(expr->data.f.params);
		break;
	case EXPR_CONCAT:
		if (expr->data.concat_exprs)
			free_expr_list(expr->data.concat_exprs);
		break;
	default:
		break;
	}
	my_free(expr);
}
#endif

struct def {
	struct def *next_def;
	struct pattern *patterns;
	int binding_count;
	struct expr *body;
};

static void add_pattern(struct def *def, struct pattern *pattern)
{
	append_pattern(&def->patterns, pattern);
}

static void append_def(struct def **defs, struct def *def)
{
	assert(!def->next_def);
	while (*defs)
		defs = &(*defs)->next_def;
	*defs = def;
}

#ifndef NDEBUG
static void free_def_list(struct def *def)
{
	if (def->next_def)
		free_def_list(def->next_def);
	if (def->patterns)
		free_pattern_list(def->patterns);
	if (def->body)
		free_expr_list(def->body);
	my_free(def);
}
#endif

struct function {
	char *name;
	int arity;
	struct def *defs;
};

static void add_def(struct function *function, struct def *def)
{
	int arity;
	assert(!def->next_def);

	{
		struct pattern *pattern = def->patterns;
		arity = 0;
		while (pattern) {
			pattern = pattern->next_pattern;
			arity++;
		}
	}

	if (function->defs) {
		if (arity != function->arity) {
			fprintf(stderr, "%d-ary definition of %s originally defined to be %d-ary\n", arity, function->name, function->arity);
			exit(1);
		}
		append_def(&function->defs, def);
	} else {
		assert(function->arity < 0 && arity >= 0);
		function->arity = arity;
		function->defs = def;
	}
}

struct function_table {
	int ch;
	struct function_table *lt;
	struct function_table *gt;
	struct function_table *eq;
	struct function function;
};

static struct function *lookup_function(char *name, int name_length, struct function_table *function_table)
{
	if (!function_table)
		return NULL;
	if (name_length <= 0)
		return &function_table->function;
	if (*name < function_table->ch)
		return lookup_function(name, name_length, function_table->lt);
	if (*name > function_table->ch)
		return lookup_function(name, name_length, function_table->gt);
	return lookup_function(name + 1, name_length - 1, function_table->eq);
}

static void set_function(char *name, int name_length, struct function *function)
{
	function->name = my_malloc((name_length + 1)*sizeof(char));
	memcpy(function->name, name, name_length*sizeof(char));
	function->name[name_length] = 0;
	function->arity = -1;
}

static struct function_table *new_function_table(char *name, int name_length, char *full_name, int full_name_length)
{
	struct function_table *function_table = my_malloc(sizeof(struct function_table));
	function_table->lt = NULL;
	function_table->gt = NULL;
	function_table->function.defs = NULL;
	if (name_length > 0) {
		function_table->ch = *name;
		function_table->function.name = NULL;
		function_table->function.arity = -1;
		function_table->eq = new_function_table(name + 1, name_length - 1, full_name, full_name_length);
	} else {
		function_table->ch = 0;
		function_table->eq = NULL;
		set_function(full_name, full_name_length, &function_table->function);
	}
	return function_table;
}

static struct function *add_function_(char *name, int name_length, struct function_table *function_table, char *full_name, int full_name_length)
{
	if (name_length <= 0) {
		if (!function_table->function.name) {
			assert(function_table->function.arity < 0);
			set_function(full_name, full_name_length, &function_table->function);
		}
		return &function_table->function;
	}
	if (!function_table->lt && !function_table->gt && !function_table->eq) {
		function_table->ch = *name;
		function_table->eq = new_function_table(name + 1, name_length - 1, full_name, full_name_length);
	} else if (*name < function_table->ch) {
		if (function_table->lt)
			return add_function_(name, name_length, function_table->lt, full_name, full_name_length);
		function_table->lt = new_function_table(name, name_length, full_name, full_name_length);
	} else if (*name > function_table->ch) {
		if (function_table->gt)
			return add_function_(name, name_length, function_table->gt, full_name, full_name_length);
		function_table->gt = new_function_table(name, name_length, full_name, full_name_length);
	} else {
		if (function_table->eq)
			return add_function_(name + 1, name_length - 1, function_table->eq, full_name, full_name_length);
		function_table->eq = new_function_table(name + 1, name_length - 1, full_name, full_name_length);
	}
	return lookup_function(name, name_length, function_table);
}

static struct function *add_function(char *name, int name_length, struct function_table *function_table)
{
	return add_function_(name, name_length, function_table, name, name_length);
}

#ifndef NDEBUG
static void free_function_table(struct function_table *function_table)
{
	if (function_table->lt)
		free_function_table(function_table->lt);
	if (function_table->gt)
		free_function_table(function_table->gt);
	if (function_table->eq)
		free_function_table(function_table->eq);
	if (function_table->function.name)
		my_free(function_table->function.name);
	if (function_table->function.defs)
		free_def_list(function_table->function.defs);
	my_free(function_table);
}
#endif

/* parse into syntax tree */

static struct expr *pop_expr(struct expr **exprs)
{
	struct expr *expr;
	expr = *exprs;
	*exprs = (*exprs)->next_expr;
	expr->next_expr = NULL;
	if (expr->expr_type == EXPR_FUNCTION) {
		int i;
		for (i = 0; i < expr->data.f.function->arity; i++) {
			struct expr *arg;
			if (!*exprs) {
				fprintf(stderr, "Function %s takes %d arguments, got %d\n", expr->data.f.function->name, expr->data.f.function->arity, i);
				exit(1);
			}
			arg = pop_expr(exprs);
			append_expr(&expr->data.f.params, arg);
		}
	}
	return expr;
}

static void parse_def_body(struct def *def)
{
	struct expr *expr;
	struct expr *exprs;

	exprs = def->body;
	expr = pop_expr(&exprs);
	if (!exprs) {
		def->body = expr;
	} else {
		def->body = new_concat_expr(expr);
		while (exprs) {
			expr = pop_expr(&exprs);
			append_expr(&def->body->data.concat_exprs, expr);
		}
	}
}

static void parse_bodies(struct function_table *function_table)
{
	struct def *def;

	if (function_table->lt)
		parse_bodies(function_table->lt);
	if (function_table->gt)
		parse_bodies(function_table->gt);
	if (function_table->eq)
		parse_bodies(function_table->eq);

	if (!function_table->function.name)
		return;

	def = function_table->function.defs;
	if (!def) {
		fprintf(stderr, "Undefined symbol: %s\n", function_table->function.name);
		exit(1);
	}

	while (def) {
		parse_def_body(def);
		def = def->next_def;
	}
}

/* tokenization */

enum token_type {
	TOKEN_ZERO, TOKEN_ONE, TOKEN_DOT, TOKEN_EQ, TOKEN_NIL, TOKEN_SYMBOL,
	TOKEN_NONE
};

static char current_symbol_token[MAX_SYMBOL_LENGTH];
static int current_symbol_length;
static char *current_filename;
static int current_linenumber;
static FILE *current_stream;
static enum token_type current_pushback_token;

static void pushback_token(enum token_type token)
{
	current_pushback_token = token;
}

static void read_symbol_token()
{
	int c;
	current_symbol_length = 0;
	for (;;) {
		c = fgetc(current_stream);
		switch (c) {
		case '\n':
			current_linenumber++;
			return;
		case ' ': case '\t': case '\r': case EOF:
			return;
		case '0': case '1': case '.': case '_': case '=':
			ungetc(c, current_stream);
			return;
		default:
			if (current_symbol_length >= MAX_SYMBOL_LENGTH) {
				fprintf(stderr, "Exceeded MAX_SYMBOL_LENGTH=%d\n", MAX_SYMBOL_LENGTH);
				exit(1);
			}
			current_symbol_token[current_symbol_length++] = c;
			break;
		}
	}
}

static void skip_comment()
{
	int c;
	do {
		c = fgetc(current_stream);
	} while (c != '\n' && c != EOF);
	current_linenumber++;
}

static enum token_type next_token()
{
	int c;
	if (current_pushback_token != TOKEN_NONE) {
		enum token_type token = current_pushback_token;
		current_pushback_token = TOKEN_NONE;
		return token;
	}
	for (;;) {
		c = fgetc(current_stream);
		switch (c) {
		case '\n':
			current_linenumber++;
			continue;
		case ' ': case '\t': case '\r':
			continue;
		case EOF: return TOKEN_NONE;
		case '0': return TOKEN_ZERO;
		case '1': return TOKEN_ONE;
		case '.': return TOKEN_DOT;
		case '_': return TOKEN_NIL;
		case '=':
			c = fgetc(current_stream);
			if (c != '=') {
				ungetc(c, current_stream);
				return TOKEN_EQ;
			}
			skip_comment();
			continue;
		default:
			ungetc(c, current_stream);
			read_symbol_token();
			return TOKEN_SYMBOL;
		}
	}
}

/* parsing */

struct parameter_table {
	int ch;
	struct parameter_table *lt;
	struct parameter_table *gt;
	struct parameter_table *eq;
	int index;
};

static int lookup_parameter_table(char *name, int name_length, struct parameter_table *parameter_table)
{
	if (!parameter_table)
		return -1;
	if (name_length <= 0)
		return parameter_table->index;
	if (*name < parameter_table->ch)
		return lookup_parameter_table(name, name_length, parameter_table->lt);
	if (*name > parameter_table->ch)
		return lookup_parameter_table(name, name_length, parameter_table->gt);
	return lookup_parameter_table(name + 1, name_length - 1, parameter_table->eq);
}

static struct parameter_table *new_parameter_table(char *name, int name_length, int index)
{
	struct parameter_table *parameter_table = my_malloc(sizeof(struct parameter_table));
	assert(index >= 0);
	parameter_table->lt = NULL;
	parameter_table->gt = NULL;
	if (name_length > 0) {
		parameter_table->ch = *name;
		parameter_table->index = -1;
		parameter_table->eq = new_parameter_table(name + 1, name_length - 1, index);
	} else {
		parameter_table->ch = 0;
		parameter_table->index = index;
		parameter_table->eq = NULL;
	}
	return parameter_table;
}

static void add_parameter_table(char *name, int name_length, int index, struct parameter_table *parameter_table)
{
	assert(index >= 0);
	if (name_length <= 0) {
		assert(parameter_table->index < 0);
		parameter_table->index = index;
		return;
	}
	if (!parameter_table->lt && !parameter_table->gt && !parameter_table->eq) {
		parameter_table->ch = *name;
		parameter_table->eq = new_parameter_table(name + 1, name_length - 1, index);
	} else if (*name < parameter_table->ch) {
		if (parameter_table->lt)
			add_parameter_table(name, name_length, index, parameter_table->lt);
		else
			parameter_table->lt = new_parameter_table(name, name_length, index);
	} else if (*name > parameter_table->ch) {
		if (parameter_table->gt)
			add_parameter_table(name, name_length, index, parameter_table->gt);
		else
			parameter_table->gt = new_parameter_table(name, name_length, index);
	} else {
		if (parameter_table->eq)
			add_parameter_table(name + 1, name_length - 1, index, parameter_table->eq);
		else
			parameter_table->eq = new_parameter_table(name + 1, name_length - 1, index);
	}
}

static void free_parameter_table(struct parameter_table *parameter_table)
{
	if (parameter_table->lt)
		free_parameter_table(parameter_table->lt);
	if (parameter_table->gt)
		free_parameter_table(parameter_table->gt);
	if (parameter_table->eq)
		free_parameter_table(parameter_table->eq);
	my_free(parameter_table);
}

static struct literal_bit_list *parse_literal_bit_list(int eat_nil)
{
	enum token_type token_type;
	struct literal_bit_list *bit_list_head;
	struct literal_bit_list *bit_list;

	token_type = next_token();
	switch (token_type) {
	case TOKEN_ZERO:
		bit_list_head = new_literal_bit_list(ZERO);
		break;
	case TOKEN_ONE:
		bit_list_head = new_literal_bit_list(ONE);
		break;
	case TOKEN_NIL:
		if (eat_nil)
			return NULL;
		/*FALLTHROUGH*/
	default:
		pushback_token(token_type);
		return NULL;
	}
	bit_list = bit_list_head;
	for (;;) {
		token_type = next_token();
		switch (token_type) {
		case TOKEN_ZERO:
			bit_list = append_literal_bit(&bit_list, ZERO);
			break;
		case TOKEN_ONE:
			bit_list = append_literal_bit(&bit_list, ONE);
			break;
		case TOKEN_NIL:
			if (eat_nil)
				return bit_list_head;
			/*FALLTHROUGH*/
		default:
			pushback_token(token_type);
			return bit_list_head;
		}
	}
}

static struct pattern *parse_pattern(struct parameter_table *parameter_table, int pattern_index)
{
	enum token_type token_type;
	enum pattern_type pattern_type;
	struct literal_bit_list *bits;

	bits = parse_literal_bit_list(0);
	token_type = next_token();
	switch (token_type) {
	case TOKEN_DOT:
		pattern_type = PATTERN_WILD;
		break;
	case TOKEN_NIL:
		pattern_type = PATTERN_NIL;
		break;
	case TOKEN_SYMBOL:
		pattern_type = PATTERN_BOUND;
		if (lookup_parameter_table(current_symbol_token, current_symbol_length, parameter_table) >= 0) {
			fprintf(stderr, "%s:%d: Multiple patterns binding %.*s\n", current_filename, current_linenumber, current_symbol_length, current_symbol_token);
			exit(1);
		}
		add_parameter_table(current_symbol_token, current_symbol_length, pattern_index, parameter_table);
		break;
	default:
		pattern_type = PATTERN_WILD;
		pushback_token(token_type);
		break;
	}
	
	return new_pattern(pattern_type, bits);
}

static struct def *parse_def(struct function_table *function_table)
{
	struct def *def;
	struct parameter_table parameter_table;
	int pattern_index;

	def = my_malloc(sizeof(struct def));
	def->next_def = NULL;
	def->patterns = NULL;
	def->body = NULL;

	parameter_table.ch = 0;
	parameter_table.lt = NULL;
	parameter_table.gt = NULL;
	parameter_table.eq = NULL;
	parameter_table.index = -1;

	pattern_index = 0;
	for (;;) {
		enum token_type token_type;
		struct pattern *pattern;

		token_type = next_token();
		if (token_type == TOKEN_NONE) {
			fprintf(stderr, "Unexpected EOF, expected function head\n");
			exit(1);
		}
		if (token_type == TOKEN_EQ)
			break;
		pushback_token(token_type);
		pattern = parse_pattern(&parameter_table, pattern_index);
		add_pattern(def, pattern);
		if (pattern->pattern_type == PATTERN_BOUND)
			pattern_index++;
	}
	def->binding_count = pattern_index;

	for (;;) {
		enum token_type token_type;
		token_type = next_token(current_stream);
		if (token_type == TOKEN_DOT)
			break;

		switch (token_type) {
		case TOKEN_NONE:
			fprintf(stderr, "Unexpected EOF, expected function body\n");
			exit(1);
			break;
		case TOKEN_EQ:
			fprintf(stderr, "%s:%d: Parse error: Unexpected =\n", current_filename, current_linenumber);
			exit(1);
			break;
		case TOKEN_NIL:
			append_expr(&def->body, new_literal_expr(NULL));
			break;
		case TOKEN_ONE:
		case TOKEN_ZERO:
			pushback_token(token_type);
			append_expr(&def->body, new_literal_expr(parse_literal_bit_list(1)));
			break;
		case TOKEN_DOT:
			assert(0);
			break;
		case TOKEN_SYMBOL:
			pattern_index = lookup_parameter_table(current_symbol_token, current_symbol_length, &parameter_table);
			if (pattern_index >= 0)
				append_expr(&def->body, new_binding_expr(pattern_index));
			else
				append_expr(&def->body, new_function_expr(add_function(current_symbol_token, current_symbol_length, function_table)));
		}
	}

	if (parameter_table.lt)
		free_parameter_table(parameter_table.lt);
	if (parameter_table.gt)
		free_parameter_table(parameter_table.gt);
	if (parameter_table.eq)
		free_parameter_table(parameter_table.eq);

	return def;
}

static void parse_file(char *filename, struct function_table *function_table)
{
	current_filename = filename;
	current_linenumber = 1;
	current_stream = fopen(filename, "r");
	if (!current_stream) {
		perror(filename);
		exit(1);
	}
	current_pushback_token = TOKEN_NONE;

	for (;;) {
		enum token_type token_type;
		struct function *function;

		token_type = next_token();

		if (token_type == TOKEN_NONE)
			break;

		if (token_type != TOKEN_SYMBOL) {
			fprintf(stderr, "%s:%d: Unexpected token.  Expected function name, got ", current_filename, current_linenumber);
			switch (token_type) {
			case TOKEN_ZERO: fprintf(stderr, "0\n"); break;
			case TOKEN_ONE: fprintf(stderr, "1\n"); break;
			case TOKEN_DOT: fprintf(stderr, ".\n"); break;
			case TOKEN_NIL: fprintf(stderr, "_\n"); break;
			case TOKEN_EQ: fprintf(stderr, "=\n"); break;
			default:
				break;
			}
			exit(1);
		}

		function = add_function(current_symbol_token, current_symbol_length, function_table);
		add_def(function, parse_def(function_table));
	}

	fclose(current_stream);
}

/* values */

struct value_list;

union promise {
	struct literal_bit_list *literal;
	struct {
		struct function *function;
		struct value_list *args;
	} funcall;
	struct value_list *concat;
	struct {
		FILE *stream;
		char byte;
		int bit_index;
	} file;
	struct value *file_copy;
};

struct value {
	unsigned int ref_count;
	enum bit_list_type head;
	struct value *tail;
	enum {
		VALUE_FORCED, VALUE_LITERAL,
		VALUE_FUNCALL, VALUE_CONCAT, VALUE_FILE, VALUE_FILE_COPY
	} value_type;
	union promise promise;
};

struct value_list {
	unsigned int ref_count;
	struct value *value;
	struct value_list *next;
};

static void append_value_list(struct value_list **list, struct value *value)
{
	while (*list)
		list = &(*list)->next;
	*list = my_malloc(sizeof(struct value_list));
	(*list)->ref_count = 1;
	(*list)->value = value;
	value->ref_count++;
	(*list)->next = NULL;
}

static struct value_list *cons_value_list(struct value *head, struct value_list *tail)
{
	struct value_list *list = my_malloc(sizeof(struct value_list));
	list->ref_count = 0;
	list->value = head;
	head->ref_count++;
	list->next = tail;
	if (tail)
		tail->ref_count++;
	return list;
}

static struct value_list *concat_value_lists(struct value_list *first, struct value_list *second)
{
	if (first)
		return cons_value_list(first->value, concat_value_lists(first->next, second));
	else
		return second;
}

static struct value *new_literal_value(struct literal_bit_list *literal)
{
	struct value *value = my_malloc(sizeof(struct value));
	value->ref_count = 0;
	if (!literal) {
		value->head = NIL;
		value->tail = NULL;
		value->value_type = VALUE_FORCED;
	} else {
		assert(literal->bit == ZERO || literal->bit == ONE);
		value->head = literal->bit;
		value->tail = NULL;
		value->value_type = VALUE_LITERAL;
		value->promise.literal = literal;
	}
	return value;
}

static struct value *new_funcall_value(struct function *function, struct value_list *args)
{
	struct value *value = my_malloc(sizeof(struct value));
	value->ref_count = 0;
	value->head = DELAYED;
	value->tail = NULL;
	value->value_type = VALUE_FUNCALL;
	value->promise.funcall.function = function;
	value->promise.funcall.args = args;
	if (args)
		args->ref_count++;
	return value;
}

static struct value *new_concat_value(struct value_list *concat)
{
	struct value *value = my_malloc(sizeof(struct value));
	value->ref_count = 0;
	value->head = DELAYED;
	value->tail = NULL;
	value->value_type = VALUE_CONCAT;
	value->promise.concat = concat;
	concat->ref_count++;
	return value;
}

static struct value *new_file_value(FILE *stream, char byte, int bit_index)
{
	struct value *value = my_malloc(sizeof(struct value));
	value->ref_count = 0;
	if (bit_index < 0)
		value->head = DELAYED;
	else if (little_endian)
		value->head = (byte & (1 << (7 - bit_index))) ? ONE : ZERO;
	else
		value->head = (byte & (1 << bit_index)) ? ONE : ZERO;
	value->tail = NULL;
	value->value_type = VALUE_FILE;
	value->promise.file.stream = stream;
	value->promise.file.byte = byte;
	value->promise.file.bit_index = bit_index;
	return value;
}

static void unreference_value(struct value *value);

static void unreference_value_list(struct value_list *value_list)
{
	while (value_list) {
		struct value_list *next = value_list->next;
		assert(value_list->ref_count > 0);
		if (--value_list->ref_count > 0)
			return;
		unreference_value(value_list->value);
		my_free(value_list);
		value_list = next;
	}
}

static void unreference_value(struct value *value)
{
	while (value) {
		struct value *tail = value->tail;
		assert(value->ref_count > 0);
		if (--value->ref_count > 0)
			return;
		switch (value->value_type) {
		case VALUE_FUNCALL:
			unreference_value_list(value->promise.funcall.args);
			break;
		case VALUE_CONCAT:
			unreference_value_list(value->promise.concat);
			break;
		case VALUE_FILE_COPY:
			unreference_value(value->promise.file_copy);
			break;
		default:
			break;
		}
		my_free(value);
		value = tail;
	}
}

static void copy_value(struct value *dest, struct value *src)
{
	if (src->tail)
		src->tail->ref_count++;
	switch (src->value_type) {
	case VALUE_FORCED:
		break;
	case VALUE_LITERAL:
		break;
	case VALUE_FUNCALL:
		if (src->promise.funcall.args)
			src->promise.funcall.args->ref_count++;
		break;
	case VALUE_CONCAT:
		if (src->promise.concat)
			src->promise.concat->ref_count++;
		break;
	case VALUE_FILE:
		src->ref_count++;
		break;
	case VALUE_FILE_COPY:
		assert(src->promise.file_copy);
		src->promise.file_copy->ref_count++;
		break;
	}
	if (dest->tail)
		unreference_value(dest->tail);
	switch (dest->value_type) {
	case VALUE_FORCED:
		break;
	case VALUE_LITERAL:
		break;
	case VALUE_FUNCALL:
		if (dest->promise.funcall.args)
			unreference_value_list(dest->promise.funcall.args);
		break;
	case VALUE_CONCAT:
		if (dest->promise.concat)
			unreference_value_list(dest->promise.concat);
		break;
	case VALUE_FILE:
		break;
	case VALUE_FILE_COPY:
		assert(dest->promise.file_copy);
		unreference_value(dest->promise.file_copy);
		break;
	}
	if (src->value_type != VALUE_FILE) {
		dest->head = src->head;
		dest->tail = src->tail;
		dest->value_type = src->value_type;
		memcpy(&dest->promise, &src->promise, sizeof(union promise));
	} else {
		dest->head = src->head;
		dest->tail = src->tail;
		dest->value_type = VALUE_FILE_COPY;
		dest->promise.file_copy = src;
	}
}

static void force_head_one_step(struct value *value);

static int try_funcall_subst(struct value *value, struct def *def);

static void force_head_funcall(struct value *value)
{
	struct def *def;
	assert(value->value_type == VALUE_FUNCALL);
	assert(value->head == DELAYED);
	def = value->promise.funcall.function->defs;
	while (def) {
		if (try_funcall_subst(value, def))
			return;
		def = def->next_def;
	}
	fprintf(stderr, "No matching pattern found for %s\n", value->promise.funcall.function->name);
	exit(1);
}

static void force_head_concat(struct value *value)
{
	assert(value->value_type == VALUE_CONCAT);
	if (!value->promise.concat) {
		value->head = NIL;
		value->value_type = VALUE_FORCED;
	} else if (value->promise.concat->value->value_type == VALUE_CONCAT) {
		struct value_list *new_concat;
		new_concat = concat_value_lists(value->promise.concat->value->promise.concat, value->promise.concat->next);
		new_concat->ref_count++;
		unreference_value_list(value->promise.concat);
		value->promise.concat = new_concat;
	} else {
		force_head_one_step(value->promise.concat->value);
		if (value->promise.concat->value->head != NIL) {
			value->head = value->promise.concat->value->head;
		} else {
			struct value_list *next_concat;
			next_concat = value->promise.concat->next;
			if (next_concat)
				next_concat->ref_count++;
			if (next_concat->next) {
				unreference_value_list(value->promise.concat);
				value->promise.concat = next_concat;
			} else {
				copy_value(value, next_concat->value);
				unreference_value_list(next_concat);
			}
		}
	}
}

static void force_head_file(struct value *value)
{
	assert(value->value_type == VALUE_FILE);
	if (value->head != DELAYED)
		return;
	assert(value->promise.file.bit_index < 0);
	if (fread(&value->promise.file.byte, 1, 1, value->promise.file.stream) < 1) {
		if (!feof(value->promise.file.stream)) {
			perror("fread");
			exit(1);
		}
		fclose(value->promise.file.stream);
		value->head = NIL;
		value->value_type = VALUE_FORCED;
	} else {
		value->promise.file.bit_index = 7;
		if (little_endian)
			value->head = (value->promise.file.byte & (1 << (7 - value->promise.file.bit_index))) ? ONE : ZERO;
		else
			value->head = (value->promise.file.byte & (1 << value->promise.file.bit_index)) ? ONE : ZERO;
		
	}
}

static void force_head_one_step(struct value *value)
{
	switch (value->value_type) {
	case VALUE_FORCED:
		return;
	case VALUE_LITERAL:
		return;
	case VALUE_FUNCALL:
		force_head_funcall(value);
		return;
	case VALUE_CONCAT:
		force_head_concat(value);
		return;
	case VALUE_FILE:
		force_head_file(value);
		return;
	case VALUE_FILE_COPY:
		force_head_file(value->promise.file_copy);
		value->head = value->promise.file_copy->head;
		if (value->promise.file_copy->value_type == VALUE_FORCED) {
			struct value *file_copy = value->promise.file_copy;
			file_copy->ref_count++;
			copy_value(value, file_copy);
			unreference_value(file_copy);
		}
		return;
	}
}

static void force_head(struct value *value)
{
	while (value->head == DELAYED)
		force_head_one_step(value);
}

static void force_tail(struct value *value)
{
	assert(value->head != DELAYED);
	if (value->head == NIL || value->tail)
		return;
	switch (value->value_type) {
	case VALUE_FORCED:
		assert(0);
		break;
	case VALUE_LITERAL:
		value->tail = new_literal_value(value->promise.literal->next_bit);
		value->tail->ref_count++;
		value->value_type = VALUE_FORCED;
		break;
	case VALUE_FUNCALL:
		assert(0);
		break;
	case VALUE_CONCAT:
		assert(value->head == value->promise.concat->value->head);
		force_tail(value->promise.concat->value);
		value->tail = new_concat_value(cons_value_list(value->promise.concat->value->tail, value->promise.concat->next));
		value->tail->ref_count++;
		unreference_value_list(value->promise.concat);
		value->value_type = VALUE_FORCED;
		break;
	case VALUE_FILE:
		value->tail = new_file_value(value->promise.file.stream, value->promise.file.byte, value->promise.file.bit_index - 1);
		value->tail->ref_count++;
		value->value_type = VALUE_FORCED;
		break;
	case VALUE_FILE_COPY:
		force_tail(value->promise.file_copy);
		if (value->promise.file_copy->value_type == VALUE_FORCED) {
			struct value *file_copy = value->promise.file_copy;
			file_copy->ref_count++;
			copy_value(value, file_copy);
			unreference_value(file_copy);
		}
		break;
	}
}

/* funcalls */

static struct value *get_expr_value(struct value_list *bindings, struct expr *expr)
{
	int i;
	struct value_list *args;
	struct expr *params;
	struct value *value;

	switch (expr->expr_type) {
	case EXPR_BINDING:
		assert(bindings);
		for (i = 0; i < expr->data.binding_index; i++) {
			bindings = bindings->next;
			assert(bindings);
		}
		return bindings->value;

	case EXPR_LITERAL:
		return new_literal_value(expr->data.literal_bits);

	case EXPR_FUNCTION:
		args = NULL;
		params = expr->data.f.params;
		while (params) {
			append_value_list(&args, get_expr_value(bindings, params));
			params = params->next_expr;
		}
		value = new_funcall_value(expr->data.f.function, args);
		if (args) {
			assert(args->ref_count > 1);
			unreference_value_list(args);
		}
		return value;

	case EXPR_CONCAT:
		args = NULL;
		params = expr->data.concat_exprs;
		while (params) {
			append_value_list(&args, get_expr_value(bindings, params));
			params = params->next_expr;
		}
		value = new_concat_value(args);
		if (args) {
			assert(args->ref_count > 1);
			unreference_value_list(args);
		}
		return value;
	}
	assert(0);
	return NULL;
}

static void do_funcall_subst(struct value *value, struct value_list *bindings, struct expr *expr)
{
	int i;
	struct value_list *args;
	struct expr *params;

	assert(value->value_type == VALUE_FUNCALL);
	assert(value->head == DELAYED);
	assert(!value->tail);

	if (value->promise.funcall.args) {
		unreference_value_list(value->promise.funcall.args);
		value->promise.funcall.args = NULL;
	}

	switch (expr->expr_type) {
	case EXPR_BINDING:
		assert(bindings);
		for (i = 0; i < expr->data.binding_index; i++) {
			bindings = bindings->next;
			assert(bindings);
		}
		copy_value(value, bindings->value);
		break;

	case EXPR_LITERAL:
		value->head = expr->data.literal_bits ? expr->data.literal_bits->bit : NIL;
		value->tail = NULL;
		value->value_type = VALUE_LITERAL;
		value->promise.literal = expr->data.literal_bits;
		break;

	case EXPR_FUNCTION:
		args = NULL;
		params = expr->data.f.params;
		while (params) {
			append_value_list(&args, get_expr_value(bindings, params));
			params = params->next_expr;
		}
		value->head = DELAYED;
		value->tail = NULL;
		value->value_type = VALUE_FUNCALL;
		value->promise.funcall.function = expr->data.f.function;
		value->promise.funcall.args = args;
		break;

	case EXPR_CONCAT:
		args = NULL;
		params = expr->data.concat_exprs;
		while (params) {
			append_value_list(&args, get_expr_value(bindings, params));
			params = params->next_expr;
		}
		assert(args);
		value->head = DELAYED;
		value->tail = NULL;
		value->value_type = VALUE_CONCAT;
		value->promise.concat = args;
		break;
	}
}

static int try_funcall_subst(struct value *value, struct def *def)
{
	struct value_list *args;
	struct pattern *patterns;
	struct value_list *bindings;
	int status;

	assert(value->value_type == VALUE_FUNCALL);
	assert(value->head == DELAYED);
	assert(!value->tail);

	args = value->promise.funcall.args;
	patterns = def->patterns;
	bindings = NULL;
	status = 1;
	while (status && args && patterns) {
		struct literal_bit_list *bits;
		struct value *arg;

		bits = patterns->bits;
		arg = args->value;
		while (bits) {
			assert(bits->bit == ZERO || bits->bit == ONE);
			force_head(arg);
			if (arg->head != bits->bit) {
				status = 0;
				break;
			}
			force_tail(arg);
			arg = arg->tail;
			bits = bits->next_bit;
		}
		if (!status)
			break;

		switch (patterns->pattern_type) {
		case PATTERN_NIL:
			force_head(arg);
			if (arg->head != NIL)
				status = 0;
			break;
		case PATTERN_WILD:
			break;
		case PATTERN_BOUND:
			append_value_list(&bindings, arg);
			break;
		}
		if (!status)
			break;

		args = args->next;
		patterns = patterns->next_pattern;
	}
	if (status) {
		assert(!patterns && !args);
		do_funcall_subst(value, bindings, def->body);
	} else {
		assert(patterns && args);
	}
	if (bindings) {
		assert(bindings->ref_count == 1);
		unreference_value_list(bindings);
	}
	return status;
}

/* top level */

static char *basename(char *filename)
{
	char *f = filename;
	while (*f) {
		if (*f == '/')
			filename = f + 1;
		f++;
	}
	return filename;
}

static int main_function_name_length(char *arg)
{
	int len;
	for (len = 0; arg[len]; len++) {
		switch (arg[len]) {
		case ' ': case '\t': case '\r': case '\n':
		case '0': case '1': case '.': case '_': case '=':
			return len;
		}
	}
	return len;
}

static void usage(FILE *stream, char *argv0)
{
	fprintf(stream, "Usage: %s FILENAME ... [- FUNCTION [FILENAME ...]]\n", argv0);
}

static void init_nil_value(struct value *nil_value)
{
	nil_value->ref_count = 1;
	nil_value->head = NIL;
	nil_value->tail = NULL;
	nil_value->value_type = VALUE_FORCED;
}

static void init_root_function_table(struct function_table *function_table)
{
	function_table->ch = 0;
	function_table->lt = NULL;
	function_table->gt = NULL;
	function_table->eq = NULL;
	function_table->function.name = NULL;
	function_table->function.arity = -1;
	function_table->function.defs = NULL;
}

static void write_value(FILE *stream, struct value *value)
{
	char byte;
	int bit_index;

	value->ref_count++;
	byte = 0;
	bit_index = 7;
	for (;;) {
		struct value *tail;

		force_head(value);
		if (value->head == NIL)
			break;
		assert(value->head == ZERO || value->head == ONE);
		if (value->head == ONE)
			byte |= 1 << (little_endian ? 7 - bit_index : bit_index);
		force_tail(value);
		tail = value->tail;
		tail->ref_count++;
		unreference_value(value);
		value = tail;

		if (--bit_index < 0) {
			bit_index = 7;
			if (fwrite(&byte, 1, 1, stream) < 1) {
				perror("fwrite");
				exit(1);
			}
			byte = 0;
		}
	}
	unreference_value(value);
}

int main(int argc, char **argv)
{
	int i;
	struct value nil_value;
	struct function_table function_table;
	char *main_function_name;
	struct function *main_function;
	struct value_list *main_args;

	init_nil_value(&nil_value);

	init_root_function_table(&function_table);

	i = 0;
	if (argc < 2) {
		usage(stdout, *argv);
		exit(0);
	}
	while (++i < argc) {
		if (!strcmp(argv[i], "-"))
			break;
		parse_file(argv[i], &function_table);
	}

	parse_bodies(&function_table);

	if (i >= argc) {
		main_function_name = basename(argv[argc - 1]);
	} else if (++i >= argc) {
		usage(stderr, *argv);
		exit(1);
	} else {
		main_function_name = argv[i++];
	}

	main_function = lookup_function(main_function_name, main_function_name_length(main_function_name), &function_table);
	if (!main_function || !main_function->defs) {
		fprintf(stderr, "Unknown function: %.*s\n", main_function_name_length(main_function_name), main_function_name);
		exit(1);
	}

	main_args = NULL;
	{
		int ii;
		struct value *stdin_value = NULL;
		for (ii = 0; ii < main_function->arity; ii++) {
			if (i + ii >= argc) {
				if (stdin_value) {
					append_value_list(&main_args, &nil_value);
				} else {
					stdin_value = new_file_value(stdin, 0, -1);
					append_value_list(&main_args, stdin_value);
				}
			} else if (!strcmp("-", argv[i + ii])) {
				if (!stdin_value)
					stdin_value = new_file_value(stdin, 0, -1);
				append_value_list(&main_args, stdin_value);
			} else {
				FILE *file = fopen(argv[i + ii], "r");
				if (!file) {
					perror(argv[i + ii]);
					exit(1);
				}
				append_value_list(&main_args, new_file_value(file, 0, -1));
			}
		}
	}

	write_value(stdout, new_funcall_value(main_function, main_args));
#ifndef NDEBUG
	unreference_value_list(main_args);
#endif
	return 0;
}
