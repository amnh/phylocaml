
#ifndef BV_HEADER
#include "bv.h"
#endif

#define BV_HAS_MALLOC
#define BV_HAS_FITCH
#define BV_HAS_ALIGNLENGTH

unsigned long bv_fitch( vect *c, const vect *a, const vect *b);

unsigned long bv_alignment_length(const long cs);

#define bv_malloc(x,y); if(0!=posix_memalign((void*)&x->data,16,y)){printf("bv_malloc_sse:Allocation failed on %d",__LINE__);}
