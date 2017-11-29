#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "mymalloc.h"
#include "nodes.h"

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

struct edge_iterator {
	int node_count;
	int node_index;
	struct node **nodes;
};

static void nodes_free_node(struct node *node)
{
	assert(node);
	for (struct edge *edge = node->first_edge; edge; ) {
		struct edge *next_edge = edge->next_edge;
		myfree(edge);
		edge = next_edge;
	}
	myfree(node);
}

struct node_pool *nodes_new_node_pool()
{
	return mymalloc(sizeof(struct node_pool));
}

void nodes_free_node_pool(struct node_pool *node_pool)
{
	assert(node_pool);
	for (struct node *node = node_pool->first_node; node; ) {
		struct node *next_node = node->next_node;
		nodes_free_node(node);
		node = next_node;
	}
	myfree(node_pool);
}

struct node *nodes_new_node(struct node_pool *node_pool)
{
	assert(node_pool);
	struct node *node = mymalloc(sizeof(struct node));
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
	node1->first_edge = mymalloc(sizeof(struct edge));
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
			myfree(edge);
			return;
		}
		last_edge_ptr = &edge->next_edge;
	}
}

struct edge_iterator *nodes_new_edge_iterator(struct node *node)
{
	assert(node);
	if (!node->first_edge) {
		return 0;
	}

	struct edge_iterator *edge_iterator = mymalloc(sizeof(struct edge_iterator));
	for (struct edge *edge = node->first_edge; edge; edge = edge->next_edge) {
		edge_iterator->node_count++;
	}
	edge_iterator->nodes = mymalloc(edge_iterator->node_count*sizeof(struct node *));
	int i = 0;
	for (struct edge *edge = node->first_edge; edge; edge = edge->next_edge) {
		edge_iterator->nodes[i++] = edge->node;
	}
	return edge_iterator;
}

struct node *nodes_edge_iterate(struct edge_iterator *edge_iterator)
{
	if (!edge_iterator) {
		return 0;
	}
	if (edge_iterator->node_index >= edge_iterator->node_count) {
		return 0;
	}
	return edge_iterator->nodes[edge_iterator->node_index++];
}

void nodes_free_edge_iterator(struct edge_iterator *edge_iterator)
{
	if (!edge_iterator) {
		return;
	}
	myfree(edge_iterator->nodes);
	myfree(edge_iterator);
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

void nodes_gc(struct node_pool *node_pool, void *root_iterator, struct node *root_iterate(void *))
{
	assert(node_pool);
	if (!node_pool->first_node) {
		return;
	}

	// mark
	node_pool->gc_mark++;
	int gc_mark = node_pool->gc_mark;
	for (struct node *node = root_iterate(root_iterator); node; node = root_iterate(root_iterator)) {
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
