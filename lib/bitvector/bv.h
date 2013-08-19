/** Pre-processing arguments for file:
 *      ARCH64 - defined if the archecture is 64bit
 *      WIDTH  - defines the size of each container (in bits) */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#include <caml/mlvalues.h>

#include "../phyloc.h"

#define BV_HEADER

/** Set-up definitions for the bit-size of data that the data-type instructions
 * will process. This is defined by the WIDTH argument. It is the number of bits
 * taken up by a single interpreted value. Along with definitions for defining
 * certain functions, this value sets a number of other things:
 *   CTYPE - the type of values being processed in the pipeline
 *           i.e. 8:char, 16:short, 32:int, 64:long.
 *   BLOCKS- the number of values stored in a 128 register = 128/WIDTH
 *   RET_STATE_SUPPORT - defined if states fit into OCaml integers natively
 * 
 * This file should be processed/renamed into a number of separate compilation
 * units for individual use of WIDTH. Because of this, all functions begin with
 * bv_, and used exclusively, so that can be scripted to bv_8 or whatever. */
#if WIDTH == 8
 #define BLOCKS 16
 #define CTYPE uint8_t
 #define Elt_val(x) Int_val(x)
 #define Val_elt(x) Val_int(x)
 #define RET_STATE_SUPPORT

#elif WIDTH == 16
 #define BLOCKS 8
 #define CTYPE uint16_t
 #define Elt_val(x) Int_val(x)
 #define Val_elt(x) Val_int(x)
 #define RET_STATE_SUPPORT

#elif WIDTH == 32
 #define BLOCKS 4
 #define CTYPE uint32_t
 #ifdef ARCH64
  #define Elt_val(x) Int32_val(x)
  #define Val_elt(x) Val_int32(x)
  #define RET_STATE_SUPPORT
 #else
  #define Elt_val(x) Int_val(x)
  #define Val_elt(x) Val_int(x)
  #undef RET_STATE_SUPPORT
 #endif

#elif WIDTH == 64
 #define BLOCKS 2
 #define CTYPE uint64_t
 #define Elt_val(x) Int64_val(x)
 #define Val_elt(x) Val_int64(x)
 #define BLOCKS 2
 #undef RET_STATE_SUPPORT

#else
 #error "Unrecognized Character Size."

#endif

struct vect_t {
  unsigned long length;  /* length of elements in packed vector          */
  unsigned long chars;   /* number of characters; excludes buffer space  */
  unsigned long padding; /* padding at end of data to fill 128bit vector */
  unsigned int code;     /* automatically generated code for debugging   */
  CTYPE *data;           /* bit-data                                     */
};
typedef struct vect_t vect;

/* custom allocated values */
void bv_CAML_free(value v);
long bv_CAML_hash(value v);
int bv_CAML_compare_values( value vbv1, value vbv2 );
void bv_CAML_serialize(value v,unsigned long *wsize_32,unsigned long *wsize_w64);
unsigned long bv_CAML_deserialize(void* dst);

/* Basic Functions for OCaml Interface */
value bv_CAML_code( value vbv );
value bv_CAML_create( value vlen );
value bv_CAML_setelt( value vbv, value vi, value vs );
value bv_CAML_compare( value vb1, value vb2 );
value bv_CAML_eltstates( value vbv, value vi );
value bv_CAML_eltint( value vbv, value vi);
value bv_CAML_cardinal( value vbv );

value bv_CAML_popcount( value vbv );

value bv_CAML_union( value vbv1, value vbv2 );
value bv_CAML_inter( value vbv1, value vbv2 );

value bv_CAML_saturation( value vbv, value n);
value bv_CAML_poly_saturation( value vb0, value n );

/* Phylogenetic Specific Functions */

value bv_CAML_fitch_median2( value vb1, value vb2 );
/* value bv_CAML_fitch_median3( value vb0, value vb1, value vb2, value vb3 ); */
value bv_CAML_distance2( value vb1, value vb2 );
/* value bv_CAML_distance3( value vb1, value vb2, value vb3 ); */
/* value bv_CAML_sankoff_median2_downpass(value vb1, value vb2); */
/* value bv_CAML_sankoff_median2_uppass( value vb1, value vb2 ); */
