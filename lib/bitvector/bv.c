/** This file requires the value WIDTH defined to be 8,16,32,64. See below. */
/** This file uses the value ARCH64 defined or not .*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#include <caml/alloc.h>     /* copy_double, et cetera */
#include <caml/memory.h>    /* caml_param, et cetera */
#include <caml/custom.h>    /* abstract types */
#include <caml/intext.h>    /* serialization */

#include "bv.h"

#ifdef SSE
#include "bv_sse.h"
#elif AVX
#include "bv_avx.h"
#elif ALTIVEX
#include "bv_altivec.h"
#elif NEON
#include "bv_neon.h"
#endif

/** deconstruct OCaml values; y=Val_vect(x) will be Vect_val(y)=x, see above. */
#define Vect_val(v) (*((vect**)Data_custom_val(v)))

/** these are the same; a constant */
#define Val_none Val_int(0)
#define None_val Val_none

#define Some_val(v) Field(v,0)
value Val_some( value v )
{
    value some;
    some = caml_alloc(1, 0);
    Store_field(some, 0, v);
    return some;
}

/** code generation global */
long magic_number = 0;
long get_next_code() { return magic_number++; }

void bv_print(const vect *a)
{
  int i;
  printf("%d :", a->code);
  for(i=0; i<a->chars;++i)
    printf("%03d |", a->data[i]);
  printf("\n");
}


/** Native Implementations for functions */

#ifndef BV_HAS_ALIGNLENGTH
long bv_alignment_length(const long cs)
{
    return cs;
}
#endif

#ifndef BV_HAS_DISTANCE
long bv_distance(const vect* a, const vect* b)
{
  long i, res;
  res = 0L;
  for(i=0; i<a->chars;++i){
    if(0 == (a->data[i] & b->data[i]) )
      res++;
  }
  return res;
}
#endif

#ifndef BV_HAS_ELTCOUNT
int bv_eltcount(const vect *a, const int i)
{
  int x = 0;
  CTYPE y = a->data[i];
  while( y != 0 ){
    y = y & (y-1);
    ++x;
  }
  return x;
}
#endif

#ifndef BV_HAS_COMPARE
int bv_compare( vect *a, vect *b )
{
  long i;
  int ret = 0;
  if(a->chars > b->chars){
    ret = 1;
  } else if( a->chars < b->chars ){
    ret = -1;
  } else {
    for( i=0; i < a->chars; ++i ){
      if( a->data[i] != b->data[i] ){
        ret = (a->data[i] > b->data[i]) ? 1 : -1;
        break;
      }
    }
  }
  return ret;
}
#endif

#ifndef BV_HAS_UNION
void bv_union(vect* c, vect*a, vect*b)
{
  long i;
  for(i=0; i<a->chars;++i)
    c->data[i] = a->data[i] | b->data[i];
}
#endif

#ifndef BV_HAS_INTER
void bv_inter(vect* c, vect*a, vect*b)
{
  long i;
  for(i=0; i<a->chars;++i)
    c->data[i] = a->data[i] & b->data[i];
}
#endif

#ifndef BV_HAS_SATURATION
long bv_saturation( const vect*a, const int n)
{
  long i,count;
  count = 0;
  if( n <= (1 << WIDTH) ){
    for(i=0; i < a->chars;++i){
      if (n & a->data[i]) { ++count; }
    }
  }
  return count;
}
#endif

#ifndef BV_HAS_PSATURATION
long bv_poly_saturation( const vect *a, int n )
{
  long i,count;
  count = 0;
  if( n <= WIDTH ){
    for(i=0; i < a->chars;++i){
      if( bv_eltcount(a,i) == n )
        ++count;
    }
  }
  return count;
}
#endif

vect* bv_copy( const vect* a )
{
  vect *x;
  CTYPE *data;
  x->length = a->length;
  x->chars  = a->chars;
  x->padding= a->padding;
  x->code   = get_next_code();
  bv_malloc( x->data, a->length * BLOCKS * sizeof(CTYPE));
  return x;
}



/** OCaml Abstract Type Functions */

void bv_CAML_free(value v)
{
    vect* x;
    x = Vect_val(v);
    free( x->data );
    free( x );
}

int bv_CAML_compare_values( value vbv1, value vbv2 )
{
  return( bv_compare(Vect_val(vbv1), Vect_val(vbv2) ) );
}

void bv_CAML_serialize(value v,unsigned long *wsize_32,unsigned long *wsize_w64){}
unsigned long bv_CAML_deserialize(void* dst) { return 0L;}

long bv_CAML_hash(value v){}

static struct custom_operations bv_custom_ops  = {
    "AMNH/bitvector/0.1",       /* identifier */
    (&bv_CAML_free),            /* finalize */
    (&bv_CAML_compare_values),  /* compare */
    (&bv_CAML_hash),            /* hash */
    (&bv_CAML_serialize),       /* serialize */
    (&bv_CAML_deserialize)      /* deserialize */
};

value bv_CAML_register (value u)
{
    CAMLparam1(u);
    register_custom_operations(&bv_custom_ops);
    CAMLreturn (Val_unit);
}



/** OCaml Interface **/

int freq = 10000; /* # of nodes to alloc before GC kicks in */
value bv_CAML_custom_max( value n )
{
    CAMLparam1( n );
    freq = Int_val( n );
    CAMLreturn( Val_unit );
}

value bv_CAML_code( value vbv )
{
  CAMLparam1( vbv );
  CAMLreturn( Val_int(Vect_val(vbv)->code) );
}

value bv_CAML_compare( value vbv1, value vbv2 )
{
    CAMLparam2( vbv1, vbv2 );
    int res;
    res = bv_compare( Vect_val(vbv1), Vect_val(vbv2) );
    CAMLreturn( Val_int(res) );
}

value bv_CAML_create( value vchars )
{
  CAMLparam1(vchars);
  CAMLlocal1(res);
  long chars,len,i;
  vect* v;
  v = (vect*) malloc(sizeof(vect));
  chars = Int_val( vchars );
  len = bv_alignment_length( chars );
  bv_alloc_val(res,v);
  v->code = get_next_code();
  v->chars = chars;
  v->length = len;
  bv_malloc( v->data, len * BLOCKS * sizeof(CTYPE) );
  for( i=0; i < len * BLOCKS; ++i)
    v->data[i] = 0;
  CAMLreturn( res );
}

value bv_CAML_ofarray( value vray )
{
  CAMLparam1( vray );
  CAMLlocal1( vres );
  long i,len,chrs;
  vect* res;
  chrs = Wosize_val( vray );
  res = (vect*) malloc(sizeof(vect));
  len = bv_alignment_length( chrs );
  res->code = get_next_code();
  res->chars = chrs;
  res->length = len;
  bv_malloc( res->data, len * BLOCKS * sizeof(CTYPE) );
  bv_alloc_val(vres,res);
  for( i=0; i < res->chars; ++i)
    res->data[i] = Int_val(Field(vray,i));
  for( i = res->chars; i < len * BLOCKS; ++i )
    res->data[i] = 0;
  CAMLreturn( vres );
}

value bv_CAML_setelt( value vbv, value vi, value vs )
{
  CAMLparam3( vbv, vi, vs );
  vect *bv;
  CTYPE mask;
  bv= Vect_val(vbv);
  mask = 1 << Int_val(vs);
  bv->data[ Int_val(vi) ] |= mask;
  CAMLreturn( Val_unit );
}

CAMLprim value bv_CAML_eltint( value vbv, value vi ) /** BROKEN **/
{
  CAMLparam2( vbv, vi );
  int res;
  vect* v;
#ifndef RET_STATE_SUPPORT
  CAMLreturn( None_val );
#else
  v = Vect_val(vbv);
  res = v->data[Int_val(vi)];
  CAMLreturn(Val_some(Val_int(res)));
#endif
}

CAMLprim value bv_CAML_eltstates( value vbv, value vi)
{
  CAMLparam2(vbv, vi);
  CAMLlocal2(cli,cons);
  vect *bv;
  long i;
  int j;
  CTYPE iter;
  bv  = Vect_val(vbv);
  cli = Val_emptylist;
  i = bv->data[Int_val(vi)];
  iter = 1;
  for(j=0;j<WIDTH;++j){
    if( i & iter ){
      cons = caml_alloc(2,0);
      Store_field( cons, 0, Val_int(j));
      Store_field( cons, 1, cli );
      cli = cons;
    }
    iter = iter << 1;
  }
  CAMLreturn( cli );
}


value bv_CAML_cardinal( value vbv )
{
  CAMLparam1( vbv );
  CAMLreturn( Val_long( Vect_val(vbv)->chars ) );
}

value bv_CAML_popcnt( value vbv )
{
  CAMLparam1( vbv );
  long i;
  i = bv_popcnt( Vect_val( vbv ) );
  CAMLreturn(Int_val(i));
}

value bv_CAML_union( value vbv1, value vbv2 )
{
  CAMLparam2( vbv1, vbv2 );
  CAMLlocal1( vres );
  long i;
  vect *res, *bv1, *bv2;
  bv1 = Vect_val(vbv1);
  bv2 = Vect_val(vbv2);
  res = bv_copy(bv1);
  bv_alloc_val(vres,res);
  bv_union(res, bv1, bv2);
  CAMLreturn(vres);
}

value bv_CAML_inter( value vbv1, value vbv2 )
{
  CAMLparam2( vbv1, vbv2 );
  CAMLlocal1( vres );
  long i;
  vect *res, *bv1, *bv2;
  bv1 = Vect_val(vbv1);
  bv2 = Vect_val(vbv2);
  res = bv_copy(bv1);
  bv_alloc_val(vres,res);
  bv_inter(res, bv1, bv2);
  CAMLreturn(vres);
}

value bv_CAML_saturation( value vbv, value vn)
{
  CAMLparam2( vbv, vn );
  long c;
  c = bv_saturation(Vect_val(vbv),Int_val(vn));
  CAMLreturn(Val_int(c));
}

value bv_CAML_poly_saturation( value vbv, value vn )
{
  CAMLparam2( vbv, vn );
  long count;
  count = bv_poly_saturation( Vect_val(vbv),Int_val(vn));
  CAMLreturn(Val_int(count));
}

value bv_CAML_distance2( value vbv1, value vbv2 )
{
  CAMLparam2( vbv1, vbv2 );
  long i;
  i = bv_distance(Vect_val(vbv1), Vect_val(vbv2));
  CAMLreturn(Val_int(i));
}

value bv_CAML_fitch_median2( value vbv1, value vbv2 )
{
  CAMLparam2( vbv1, vbv2 );
  CAMLlocal1( vres );
  vect* res,*bv1,*bv2;
  bv1 = Vect_val( vbv1 );
  bv2 = Vect_val( vbv2 );
  res = bv_copy(bv1);
  bv_alloc_val(vres,res);
  bv_fitch( res, bv1, bv2 );
  CAMLreturn( vres );
}

/* value bv_CAML_fitch_median3( value vb0, value vb1, value vb2, value vb3 ); */
/* value bv_CAML_distance3( value vb1, value vb2, value vb3 ); */
/* value bv_CAML_sankoff_median2_downpass(value vb1, value vb2); */
/* value bv_CAML_sankoff_median2_uppass( value vb1, value vb2 ); */
