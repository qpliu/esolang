#ifndef SCOPE_H
#define SCOPE_H

#include "vars.h"

struct scope;

void scope_free(struct scope *scope);

struct scope *scope_push(struct scope *scope, int var_count);

struct scope *scope_pop(struct scope *scope);

struct var *scope_new_var(struct scope *scope);

struct var *scope_get(struct scope *scope, int var_index);

void scope_set(struct scope *scope, int var_index, struct var *var);

void scope_refer(struct scope *scope, int var_index, struct var *var);

void scope_push_do_edges(struct scope *scope, struct var *var);

struct var *scope_pop_do_edge(struct scope *scope);

void scope_pop_do_edges(struct scope *scope);

void scope_gc(struct scope *scope);

#endif /* SCOPE_H */
