#ifndef VARS_H
#define VARS_H

struct vars;

struct var;

struct var_edge_iterator;

struct vars *vars_new_vars();

void vars_free_vars(struct vars *vars);

struct var *vars_new_var(struct vars *vars);

struct var *vars_set_new_var(struct vars *vars, struct var *var);

void vars_set_var(struct var *var1, struct var *var2);

int vars_eq(struct var *var1, struct var *var2);

int vars_has_edge(struct var *var1, struct var *var2);

void vars_add_edge(struct var *var1, struct var *var2);

void vars_remove_edge(struct var *var1, struct var *var2);

struct var_edge_iterator *vars_new_edge_iterator(struct vars *vars, struct var *var);

struct var *vars_edge_iterate(struct var_edge_iterator *var_edge_iterator);

void vars_free_edge_iterator(struct var_edge_iterator *var_edge_iterator);

void vars_gc(struct vars *vars, void *root_iterator, struct var *root_iterate(void *));

#endif /* VARS_H */
