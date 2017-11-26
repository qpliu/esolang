#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "mymalloc.h"
#include "nodes.h"
#include "vars.h"

struct vars {
	int gc_mark;
	struct node_pool *node_pool;
	struct var *first_var;
};

struct var {
	int gc_mark;
	struct var *next_var;
	struct node *node;
};

struct var_edge_iterator {
	struct vars *vars;
	struct edge_iterator *edge_iterator;
};

struct vars *vars_new_vars()
{
	struct vars *vars = mymalloc(sizeof(struct vars));
	assert(vars);
	memset(vars, 0, sizeof(struct vars));
	vars->node_pool = nodes_new_node_pool();
	return vars;
}

void vars_free_vars(struct vars *vars)
{
	assert(vars);
	for (struct var *var = vars->first_var; var; ) {
		struct var *next_var = var->next_var;
		myfree(var);
		var = next_var;
	}
	nodes_free_node_pool(vars->node_pool);
	myfree(vars);
}

static struct var *vars_new_var_with_node(struct vars *vars, struct node *node)
{
	assert(vars);
	struct var *var = mymalloc(sizeof(struct var));
	assert(var);
	memset(var, 0, sizeof(struct var));
	var->next_var = vars->first_var;
	vars->first_var = var;
	var->node = node;
	return var;
}

struct var *vars_new_var(struct vars *vars)
{
	return vars_new_var_with_node(vars, nodes_new_node(vars->node_pool));
}

struct var *vars_set_new_var(struct vars *vars, struct var *var)
{
	assert(var);
	return vars_new_var_with_node(vars, var->node);
}

void vars_set_var(struct var *var1, struct var *var2)
{
	assert(var1 && var2);
	var1->node = var2->node;
}

int vars_eq(struct var *var1, struct var *var2)
{
	assert(var1 && var2);
	return var1->node == var2->node;
}

int vars_has_edge(struct var *var1, struct var *var2)
{
	assert(var1 && var2);
	return nodes_has_edge(var1->node, var2->node);
}

void vars_add_edge(struct var *var1, struct var *var2)
{
	assert(var1 && var2);
	nodes_add_edge(var1->node, var2->node);
}

void vars_remove_edge(struct var *var1, struct var *var2)
{
	assert(var1 && var2);
	nodes_remove_edge(var1->node, var2->node);
}

struct var_edge_iterator *vars_new_edge_iterator(struct vars *vars, struct var *var)
{
	assert(vars);
	assert(var);
	struct var_edge_iterator *var_edge_iterator = mymalloc(sizeof(struct var_edge_iterator));
	memset(var_edge_iterator, 0, sizeof(struct var_edge_iterator));
	var_edge_iterator->vars = vars;
	var_edge_iterator->edge_iterator = nodes_new_edge_iterator(var->node);
	return var_edge_iterator;
}

struct var *vars_edge_iterate(struct var_edge_iterator *var_edge_iterator)
{
	struct node *node = nodes_edge_iterate(var_edge_iterator->edge_iterator);
	if (!node) {
		return 0;
	}
	return vars_new_var_with_node(var_edge_iterator->vars, node);
}

void vars_free_edge_iterator(struct var_edge_iterator *var_edge_iterator)
{
	nodes_free_edge_iterator(var_edge_iterator->edge_iterator);
	myfree(var_edge_iterator);
}

static struct node *vars_gc_node_iterate(void *iterator)
{
	struct var **var_ptr = (struct var **) iterator;
	struct var *var = *var_ptr;
	if (var) {
		*var_ptr = var->next_var;
		return var->node;
	}
	return 0;
};

void vars_gc(struct vars *vars, void *root_iterator, struct var *root_iterate(void *))
{
	vars->gc_mark++;
	int gc_mark = vars->gc_mark;
	for (struct var *var = root_iterate(root_iterator); var; var = root_iterate(root_iterator)) {
		var->gc_mark = gc_mark;
	}
	struct var **last_var_ptr = &vars->first_var;
	for (struct var *var = vars->first_var; var; ) {
		if (var->gc_mark == gc_mark) {
			last_var_ptr = &var->next_var;
			var = var->next_var;
		} else {
			*last_var_ptr = var->next_var;
			myfree(var);
			var = *last_var_ptr;
		}
	}
	struct var *var_iterator = vars->first_var;
	nodes_gc(vars->node_pool, &var_iterator, vars_gc_node_iterate);
}
