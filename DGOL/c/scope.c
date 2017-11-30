#include <assert.h>
#include <stdlib.h>
#include <strings.h>

#include "mymalloc.h"
#include "scope.h"
#include "vars.h"

struct scope_var_list {
	struct var *var;
	struct scope_var_list *next_scope_var_list;
};

struct scope_var_list_stack {
	struct scope_var_list *scope_var_list;
	struct scope_var_list_stack *next_scope_var_list_stack;
};

struct scope {
	struct vars *vars;
	struct scope *parent_scope;
	int scope_var_count;
	struct var **scope_vars;
	struct scope_var_list_stack *scope_var_list_stack;
};

void scope_free(struct scope *scope)
{
	while (scope) {
		scope = scope_pop(scope);
	}
}

struct scope *scope_push(struct scope *scope, int var_count)
{
	struct scope *new_scope = mymalloc(sizeof(struct scope));

	if (scope) {
		new_scope->vars = scope->vars;
	} else {
		new_scope->vars = vars_new_vars();
	}
	new_scope->parent_scope = scope;
	new_scope->scope_var_count = var_count;
	new_scope->scope_vars = mymalloc(var_count*sizeof(struct var *));

	return new_scope;
}

struct scope *scope_pop(struct scope *scope)
{
	struct scope *parent_scope = scope->parent_scope;
	struct vars *vars = scope->vars;

	while (scope->scope_var_list_stack) {
		scope_pop_do_edges(scope);
	}
	myfree(scope->scope_vars);
	if (!parent_scope) {
		vars_free_vars(vars);
	}
	return parent_scope;
}

struct var *scope_new_var(struct scope *scope)
{
	assert(scope);
	return vars_new_var(scope->vars);
}

struct var *scope_get(struct scope *scope, int var_index)
{
	assert(scope);
	assert(var_index < scope->scope_var_count);

	if (!scope->scope_vars[var_index]) {
		scope->scope_vars[var_index] = vars_new_var(scope->vars);
	}

	return scope->scope_vars[var_index];
}

void scope_set(struct scope *scope, int var_index, struct var *var)
{
	assert(scope);
	assert(var_index < scope->scope_var_count);

	if (!scope->scope_vars[var_index]) {
		scope->scope_vars[var_index] = vars_set_new_var(scope->vars, var);
	} else {
		vars_set_var(scope->scope_vars[var_index], var);
	}
}

void scope_push_do_edges(struct scope *scope, struct var *var)
{
	assert(scope);
	assert(var);

	struct scope_var_list_stack *scope_var_list_stack = mymalloc(sizeof(struct scope_var_list_stack));
	scope_var_list_stack->next_scope_var_list_stack = scope->scope_var_list_stack;
	scope->scope_var_list_stack = scope_var_list_stack;

	struct var_edge_iterator *var_edge_iterator = vars_new_edge_iterator(scope->vars, var);
	for (struct var *edge = vars_edge_iterate(var_edge_iterator); edge; edge = vars_edge_iterate(var_edge_iterator)) {
		struct scope_var_list *scope_var_list = mymalloc(sizeof(struct scope_var_list));
		scope_var_list->var = edge;
		scope_var_list->next_scope_var_list = scope_var_list_stack->scope_var_list;
		scope_var_list_stack->scope_var_list = scope_var_list;
	}
	vars_free_edge_iterator(var_edge_iterator);
}

struct var *scope_pop_do_edge(struct scope *scope)
{
	assert(scope);
	assert(scope->scope_var_list_stack);

	struct scope_var_list *scope_var_list = scope->scope_var_list_stack->scope_var_list;
	if (!scope_var_list) {
		return 0;
	}

	struct var *var = scope_var_list->var;
	scope->scope_var_list_stack->scope_var_list = scope_var_list->next_scope_var_list;
	myfree(scope_var_list);
	return var;
}

void scope_pop_do_edges(struct scope *scope)
{
	assert(scope);
	assert(scope->scope_var_list_stack);

	struct scope_var_list_stack *scope_var_list_stack = scope->scope_var_list_stack;
	scope->scope_var_list_stack = scope_var_list_stack->next_scope_var_list_stack;

	while (scope_var_list_stack->scope_var_list) {
		struct scope_var_list *scope_var_list = scope_var_list_stack->scope_var_list;
		scope_var_list_stack->scope_var_list = scope_var_list->next_scope_var_list;
		myfree(scope_var_list);
	}
	myfree(scope_var_list_stack);
}

struct scope_gc_root_iterator {
	struct scope *scope;
	int scope_var_index;
	struct scope_var_list_stack *current_scope_var_list_stack;
	struct scope_var_list *current_scope_var_list;
};

static void scope_init_gc_root_iterator(struct scope *scope, struct scope_gc_root_iterator *scope_gc_root_iterator)
{
	scope_gc_root_iterator->scope = scope;
	scope_gc_root_iterator->scope_var_index = 0;
	if (!scope) {
		scope_gc_root_iterator->current_scope_var_list_stack = 0;
		scope_gc_root_iterator->current_scope_var_list = 0;
	} else {
		scope_gc_root_iterator->current_scope_var_list_stack = scope->scope_var_list_stack;
		if (scope_gc_root_iterator->current_scope_var_list_stack) {
			scope_gc_root_iterator->current_scope_var_list = scope_gc_root_iterator->current_scope_var_list_stack->scope_var_list;
		} else {
			scope_gc_root_iterator->current_scope_var_list = 0;
		}
	}
}

static struct var *scope_gc_root_iterate(void *root_iterator) {
	struct scope_gc_root_iterator *scope_gc_root_iterator = (struct scope_gc_root_iterator *) root_iterator;
	while (scope_gc_root_iterator->scope) {
		struct scope *scope = scope_gc_root_iterator->scope;
		while (scope_gc_root_iterator->scope_var_index < scope->scope_var_count) {
			struct var *var = scope->scope_vars[scope_gc_root_iterator->scope_var_index];
			scope_gc_root_iterator->scope_var_index++;
			if (var) {
				return var;
			}
		}
		while (scope_gc_root_iterator->current_scope_var_list_stack) {
			struct scope_var_list *scope_var_list = scope_gc_root_iterator->current_scope_var_list;
			if (scope_var_list) {
				scope_gc_root_iterator->current_scope_var_list = scope_var_list->next_scope_var_list;
				return scope_var_list->var;
			}
			struct scope_var_list_stack *scope_var_list_stack = scope_gc_root_iterator->current_scope_var_list_stack->next_scope_var_list_stack;
			scope_gc_root_iterator->current_scope_var_list_stack = scope_var_list_stack;
			if (scope_var_list_stack) {
				scope_gc_root_iterator->current_scope_var_list = scope_var_list_stack->scope_var_list;
			}
		}
		scope_init_gc_root_iterator(scope->parent_scope, scope_gc_root_iterator);
	}
	return 0;
}

void scope_gc(struct scope *scope)
{
	struct scope_gc_root_iterator scope_gc_root_iterator;
	scope_init_gc_root_iterator(scope, &scope_gc_root_iterator);
	vars_gc(scope->vars, &scope_gc_root_iterator, scope_gc_root_iterate);
}
