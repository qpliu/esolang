#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "nodes.h"

static void nodes_free_node(struct node *node)
{
	assert(node);
	for (struct edge *edge = node->first_edge; edge; edge = edge->next_edge) {
		free(edge);
	}
	free(node);
}

struct node_pool *nodes_new_node_pool()
{
	struct node_pool *node_pool = malloc(sizeof(struct node_pool));
	assert(node_pool);
	memset(node_pool, 0, sizeof(struct node_pool));
	return node_pool;
}

void nodes_free_node_pool(struct node_pool *node_pool)
{
	for (struct node *node = node_pool->first_node; node; node = node->next_node) {
		nodes_free_node(node);
	}
	free(node_pool);
}

struct node *nodes_new_node(struct node_pool *node_pool)
{
	assert(node_pool);
	struct node *node = malloc(sizeof(struct node));
	assert(node);
	memset(node, 0, sizeof(struct node));
	node->next_node = node_pool->first_node;
	node_pool->first_node = node;
	return node;
}

int nodes_has_edge(struct node *node1, struct node *node2)
{
	assert(node1 && node2);
	for (struct edge *edge = node1->first_edge; edge; edge = edge->next_edge) {
		if (edge->node == node2) {
			return 1;
		}
	}
	return 0;
}

void nodes_add_edge(struct node *node1, struct node *node2)
{
	assert(node1 && node2);
	if (nodes_has_edge(node1, node2)) {
		return;
	}

	struct edge *first_edge = node1->first_edge;
	node1->first_edge = malloc(sizeof(struct edge));
	node1->first_edge->node = node2;
	node1->first_edge->next_edge = first_edge;
}

void nodes_remove_edge(struct node *node1, struct node *node2)
{
	assert(node1 && node2);
	if (!node1->first_edge) {
		return;
	}

	struct edge **last_edge_ptr = &node1->first_edge;
	for (struct edge *edge = node1->first_edge; edge; edge = edge->next_edge) {
		if (edge->node == node2) {
			*last_edge_ptr = edge->next_edge;
			free(edge);
			return;
		}
		last_edge_ptr = &edge->next_edge;
	}
}

static void nodes_gc_mark_node(struct node *node, int gc_mark)
{
	if (node->gc_mark == gc_mark) {
		return;
	}

	node->gc_mark = gc_mark;
	for (struct edge *edge = node->first_edge; edge; edge = edge->next_edge) {
		nodes_gc_mark_node(edge->node, gc_mark);
	}
}

void nodes_gc(struct node_pool *node_pool, void *root_iterator_state, struct node *root_iterator(void *))
{
	assert(node_pool);
	if (!node_pool->first_node) {
		return;
	}

	// mark
	node_pool->gc_mark++;
	int gc_mark = node_pool->gc_mark;
	for (struct node *node = root_iterator(root_iterator_state); node; node = root_iterator(root_iterator_state)) {
		nodes_gc_mark_node(node, gc_mark);
	}

	// sweep
	struct node **last_node_ptr = &node_pool->first_node;
	struct node *node = node_pool->first_node;
	while (node) {
		if (node->gc_mark == node_pool->gc_mark) {
			last_node_ptr = &node->next_node;
			node = node->next_node;
		} else {
			*last_node_ptr = node->next_node;
			nodes_free_node(node);
			node = *last_node_ptr;
		}
	}
}
