#ifndef NODES_H
#define NODES_H

struct node_pool;

struct node;

struct edge;

struct edge_iterator;

struct node_pool *nodes_new_node_pool();

void nodes_free_node_pool(struct node_pool *node_pool);

struct node *nodes_new_node(struct node_pool *node_pool);

int nodes_has_edge(struct node *node1, struct node *node2);

void nodes_add_edge(struct node *node1, struct node *node2);

void nodes_remove_edge(struct node *node1, struct node *node2);

struct edge_iterator *nodes_new_edge_iterator(struct node *node);

struct node *nodes_edge_iterate(struct edge_iterator *edge_iterator);

void nodes_free_edge_iterator(struct edge_iterator *edge_iterator);

void nodes_gc(struct node_pool *node_pool, void *root_iterator, struct node *root_iterate(void *));

#endif /* NODES_H */
