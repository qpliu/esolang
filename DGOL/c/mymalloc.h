#ifndef MYMALLOC_H
#define MYMALLOC_H

#include <stdlib.h>

#define mymalloc(sz) _mymalloc(sz,__FILE__,__LINE__)
#define myfree(ptr) _myfree(ptr,__FILE__,__LINE__)

void *_mymalloc(size_t,char *,int);
void _myfree(void *,char *,int);

void mymalloc_done();

#endif /* MYMALLOC_H */
