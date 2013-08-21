/** This file requires the value WIDTH defined to be 8,16,32,64. See below. */
/** This file uses the value ARCH64 defined or not .*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#include "bv_sse.h"

/* remove the bit width to avoid confusion; these apply in general */
#define _mm_load(a) _mm_load_si128(a)
#define _mm_and(a,b) _mm_and_si128(a,b)
#define _mm_or(a,b) _mm_or_si128(a,b)
#define _mm_setzero() _mm_setzero_si128()

/** Determine the header to include based on compiler options. This does not
 * guarentee that the application can be run on the system. TODO: Run-time
 * checks may be used else "Illegal Instruction" error will occur at run-time.
 *
 * -blendv is in SSE4.1; use combination [or] and [and] for earlier versions.
 * -stream is in SSE4.1; this allows faster performance on non-temporal data,
 *                       use a basic store for earlier versions. */
#ifdef __SSE4_1__
 #include <smmintrin.h>
 #define _mm_blendv(a,b,c) _mm_blendv_epi8(a,b,c)
 #define _mm_store(a,b) _mm_stream_si128(a,b)

/* cmpeg_epi64 is not available in SSE2 */
#elif __SSE2__
 #include <emmintrin.h> 
 #define _mm_blendv(a,b,c) _mm_or(a, _mm_and(b, c))
 #define _mm_store(a,b) _mm_store_si128(a,b)
#else
 #error "SSE requires at least version 4"
#endif

/** WIDTH specific declarations. */

#if WIDTH == 8
 #define BLOCKS 16
 #define _mm_cmpeq(a,b) _mm_cmpeq_epi8(a,b)
 #define _mm_extract(a,i) _mm_extract_epi8(a,i)

#elif WIDTH == 16
 #define BLOCKS 8
 #define _mm_cmpeq(a,b) _mm_cmpeq_epi16(a,b)
 #define _mm_extract(a,i) _mm_extract_epi16(a,i)

#elif WIDTH == 32
 #define BLOCKS 4
 #define _mm_cmpeq(a,b) _mm_cmpeq_epi32(a,b)
 #define _mm_extract(a,i) _mm_extract_epi32(a,i)

#elif WIDTH == 64
 #define BLOCKS 2
 #define _mm_cmpeq(a,b) _mm_cmpeq_epi64(a,b)
 #define _mm_extract(a,i) _mm_extract_epi64(a,i)

#else
 #error "Unrecognized Character Size."
#endif


/* Define a union to approach the values in the SSE registers easily
 *
 * nrlucaroni: Does this affect performance? My guess is that it does as the
 * data needs to be unloaded from the SSE register and processed accordingly.
 * This should be used for debugging only. More testing needs to be done here.*/
#define utype union {__m128i v; CTYPE a[BLOCKS];}

unsigned long bv_alignment_length(const long cs)
{
  return cs + (16 - (cs % 16));
}

/* using printf with %vhd is an alternative printing mechanism */
void bv_print_m128i(__m128i x)
{
  int i;
  utype u;
  u.v = x;
  for( i = 0; i < BLOCKS; ++i )
    printf(" %03d |", (int)u.a[i]);
}

/* From Hackers Delight; places count in each 16bit place. a horizontal add can
 * finish the entire summation. 16 routines.
 * nrlucaroni: TODO translate to other widths?
 * nrlucaroni: TODO test
__m128i _mm_popcnt_epi16( __m128i v)
{
  v = _mm_add_epi16(_mm_and_si128(v, _mm_set1_epi16(0x5555)), _mm_and_si128(_mm_srli_epi16(v, 1), _mm_set1_epi16(0x5555)));
  v = _mm_add_epi16(_mm_and_si128(v, _mm_set1_epi16(0x3333)), _mm_and_si128(_mm_srli_epi16(v, 2), _mm_set1_epi16(0x3333)));
  v = _mm_add_epi16(_mm_and_si128(v, _mm_set1_epi16(0x0f0f)), _mm_and_si128(_mm_srli_epi16(v, 4), _mm_set1_epi16(0x0f0f)));
  v = _mm_add_epi16(_mm_and_si128(v, _mm_set1_epi16(0x00ff)), _mm_and_si128(_mm_srli_epi16(v, 8), _mm_set1_epi16(0x00ff)));
  return v;
} */

unsigned long bv_fitch(vect *c, const vect *a, const vect *b)
{
  unsigned long i,j,r;
  __m128i unions,inters,zeros,vb_i;
  /* nrl: TODO, will the union breaking SSE pipelining? */
  utype va_i;

  zeros = _mm_setzero();
  r = -(a->padding);
  for(i = 0; i < a->length; i++){
    /* 0 - load data to SSE registers */
    va_i.v = _mm_load((__m128i*)&a->data[i*BLOCKS]);
    vb_i   = _mm_load((__m128i*)&b->data[i*BLOCKS]);
    /* 1 - take full union */
    unions = _mm_or(va_i.v,vb_i);
    /* 2 - take full intersection */
    inters = _mm_and(va_i.v,vb_i);
    /* 3 - set 1s to empty locations in intersection; 0s otherwise */
    va_i.v = _mm_cmpeq(inters, zeros);
    /* 4 - blend union and intersection based on empty values from cmpeq */
    vb_i   = _mm_blendv(inters, unions, va_i.v);
    /* 5 - store the result to [c] */
    _mm_store((__m128i*)&c->data[i*BLOCKS], vb_i);
    /* 6 - calculate added cost from median; SUM va_i>0 */
    /* nrl: TODO, find a native SSE function to do this; popcnt? */
    for( j = 0; j<BLOCKS; ++j) if(va_i.a[j] > 0){++r;};
  }
  return r;
}

/*void bv_fitch3(vect d, const vect a, const vect b, const vect c) { }  */
/*void bv_sankoff_downpass { } */
/*void bv_sankoff_uppass(vect c, const vect a, const vect b) { } */
