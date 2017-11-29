#include <assert.h>
#include <string.h>

#include "mymalloc.h"

void *mymalloc(size_t size)
{
	void *ptr = malloc(size);
	assert(ptr);
	memset(ptr, 0, size);
	return ptr;
}

void myfree(void *ptr)
{
	assert(ptr);
	free(ptr);
}
