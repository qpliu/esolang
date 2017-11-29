#include <assert.h>
#include <string.h>

#include "indexer.h"
#include "mymalloc.h"

struct indexer {
	char *string;
	struct indexer *next;
};

static struct indexer *indexer_new_with_string(char *string)
{
	struct indexer *indexer = mymalloc(sizeof(struct indexer));
	indexer->string = string;
	indexer->next = 0;
	return indexer;
}

struct indexer *indexer_new()
{
	return indexer_new_with_string(0);
}

void indexer_free(struct indexer *indexer)
{
	assert(indexer);
	while (indexer) {
		struct indexer *last = indexer;
		indexer = indexer->next;
		myfree(last);
	}
}

int indexer_index(struct indexer *indexer, char *string)
{
	assert(indexer);
	assert(string);
	for (int index = 0;; index++) {
		if (!indexer->string) {
			indexer->string = string;
			return index;
		}
		if (!strcmp(indexer->string, string)) {
			return index;
		}
		if (indexer->next) {
			indexer = indexer->next;
		} else {
			indexer->next = indexer_new_with_string(string);
			return index+1;
		}
	}
}

int indexer_index_unique(struct indexer *indexer, char *string)
{
	assert(indexer);
	assert(string);
	for (int index = 0;; index++) {
		if (!indexer->string) {
			indexer->string = string;
			return index;
		}
		if (!strcmp(indexer->string, string)) {
			return -1;
		}
		if (indexer->next) {
			indexer = indexer->next;
		} else {
			indexer->next = indexer_new_with_string(string);
			return index+1;
		}
	}
}

int indexer_find_index(struct indexer *indexer, char *string)
{
	assert(indexer);
	assert(string);
	for (int index = 0;; index++) {
		if (!indexer->string) {
			indexer->string = string;
			return index;
		}
		if (!strcmp(indexer->string, string)) {
			return index;
		}
		if (indexer->next) {
			indexer = indexer->next;
		} else {
			return -1;
		}
	}
}

int indexer_count(struct indexer *indexer)
{
	assert(indexer);
	for (int index = 0;; index++) {
		if (!indexer->string) {
			return index;
		} else if (!indexer->next) {
			return index+1;
		} else {
			indexer = indexer->next;
		}
	}
}
