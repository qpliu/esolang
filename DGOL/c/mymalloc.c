#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "mymalloc.h"

static int malloc_count = 0;
static int free_count = 0;

void *_mymalloc(size_t size, char *file, int line)
{
	malloc_count++;
	void *ptr = malloc(size);
	assert(ptr);
	memset(ptr, 0, size);
	return ptr;
}

void _myfree(void *ptr, char *file, int line)
{
	assert(ptr);
	free_count++;
	free(ptr);
}

void mymalloc_done()
{
}
