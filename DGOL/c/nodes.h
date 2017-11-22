#ifndef NODES_H
#define NODES_H

struct node_pool {
	int gc_mark;
	struct node *first_node;
};

struct node {
	int gc_mark;
	struct node *next_node;
	struct edge *first_edge;
};

struct edge {
	struct node *node;
	struct edge *next_edge;
};

struct node_pool *nodes_new_node_pool();

void nodes_free_node_pool(struct node_pool *node_pool);

struct node *nodes_new_node(struct node_pool *node_pool);

int nodes_has_edge(struct node *node1, struct node *node2);

void nodes_add_edge(struct node *node1, struct node *node2);

void nodes_remove_edge(struct node *node1, struct node *node2);

void nodes_gc(struct node_pool *node_pool, void *root_iterator_state, struct node *root_iterator(void *));

#endif /* NODES_H */
