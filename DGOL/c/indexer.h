#ifndef INDEXER_H
#define INDEXER_H

struct indexer;

struct indexer *indexer_new();

void indexer_free(struct indexer *indexer);

int indexer_index(struct indexer *indexer, char *string);

int indexer_index_unique(struct indexer *indexer, char *string);

int indexer_find_index(struct indexer *indexer, char *string);

int indexer_count(struct indexer *indexer);

#endif /* INDEXER_H */
