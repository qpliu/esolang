#include <assert.h>
#include <string.h>

#include "mymalloc.h"

void *_mymalloc(size_t size, char *file, int line)
{
	void *ptr = malloc(size);
	assert(ptr);
	memset(ptr, 0, size);
	return ptr;
}

void _myfree(void *ptr, char *file, int line)
{
	assert(ptr);
	free(ptr);
}
