#ifndef PARSE_H
#define PARSE_H

#include <stdio.h>

#include "ast.h"

struct ast_module *parse(FILE *file);

void parse_free_ast_modules(struct ast_module *ast_module);

#endif /* PARSE_H */
