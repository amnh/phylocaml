#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#include "seq.h"

//used for garbage collection
#define SEQ_UNUSED_MEMORY 1000000


/** return the capacity of the seq-data */
int seq_get_cap (const seqt a) {
  return a->cap;
}
value seq_CAML_get_cap (value s) {
  CAMLparam1(s);
  seqt tmp;
  Seq_custom_val(tmp,s);
  CAMLreturn(Val_int(tmp->cap));
}

/** get the active length of the seq-data */
int seq_get_len (const seqt a) {
  return a->len;
}
value seq_CAML_length (value v) {
  CAMLparam1(v);
  seqt tmp;
  Seq_custom_val(tmp,v);
  CAMLreturn(Val_int(seq_get_len(tmp)));
}


SEQT* seq_get_begin (const seqt a) { return (a->begin); }

SEQT* seq_get_head (const seqt a) { return (a->head); }

SEQT* seq_get_end (const seqt a) { return (a->end); }

int seq_begin (int cap, int len) { return (cap - len); }

SEQT* seq_get_ptr (const seqt a, int p) {
  assert (p < a->len);
  assert (p >= 0);
  return (a->begin + p);
}
    
SEQT seq_get (const seqt a, int p) {
  assert (p < a->len);
  assert (p >= 0);
  return (*(seq_get_ptr (a, p)));
}
value seq_CAML_get (value s, value p) {
  CAMLparam2(s, p);
  seqt cs;
  int cp;
  Seq_custom_val(cs,s);
  cp = Int_val(p);
  CAMLreturn (Val_int((int) (seq_get (cs, cp))));
}

void seq_print (const seqt a) {
  int i;
  printf("len=%d,cap=%d,[",a->len,a->cap);
  for(i=0;i<a->len; i++) {
    printf("%d ",seq_get(a,i));
  }
  printf("]\n"); fflush(stdout);
}

/* set element in a sequence; destructive */
void seq_set (seqt a, int p, SEQT v) {
  SEQT *tmp;
  if (a->len == 0) {
    assert (p == 0);
    a->len++;
  } else {
    assert (p < a->len);
    assert (p >= 0);
  }
  tmp = seq_get_ptr (a, p);
  *tmp = v;
  return;
}
value seq_CAML_set (value s, value p, value v) {
  CAMLparam3(s, p, v);
  seqt cs;
  Seq_custom_val(cs,s);
  seq_set (cs, Int_val(p), Int_val(v)); 
  CAMLreturn (Val_unit);
}



/** reverse the seq in place */
void seq_reverse_ip (seqt cs) { 
  SEQT *a, *b, tmp;
  a = seq_get_begin (cs);
  b = seq_get_end (cs);
  while (b > a) {
    tmp = *b;
    *b = *a;
    *a = tmp;
     b--;
     a++;
  }
  return;
}
value seq_CAML_reverse_ip (value s) {
  CAMLparam1(s);
  seqt cs;
  Seq_custom_val(cs,s);
  seq_reverse_ip (cs);
  CAMLreturn(Val_unit);
}



/** prepend the sequence with value [v]; destructive */
void seq_prepend (seqt a, SEQT v) {
  assert(a->cap > a->len);
  a->begin = a->begin - 1;
  *(a->begin) = v;
  a->len = a->len + 1;
  return;
}
value seq_CAML_prepend (value s, value v) {
  CAMLparam2(s, v);
  seqt tmp;
  Seq_custom_val(tmp,s);
  seq_prepend (tmp, Int_val(v));
  CAMLreturn(Val_unit);
}

/** reverse the sequence from [src] to [tgt] */
void seq_reverse (seqt src, seqt tgt) {
  int i;
  tgt->len = src->len;
  tgt->begin = tgt->head + (tgt->cap - tgt->len);
  for (i = 0; i < src->len; i++) 
    *(tgt->begin + i) = *(src->end - i);
  return;
}
value seq_CAML_reverse (value src, value tgt) {
  CAMLparam2(src, tgt);
  seqt csrc, ctgt;
  Seq_custom_val(csrc,src);
  Seq_custom_val(ctgt,tgt);
  seq_reverse (csrc, ctgt);
  CAMLreturn(Val_unit);
}


/** Clear the seq by re-setting begin and length */
void seq_clear (seqt s) {
  s->len = 0;
  s->begin = s->end + 1;
  return;
}
value seq_CAML_clear (value s) {
  CAMLparam1(s);
  seqt cs;
  Seq_custom_val(cs,s);
  seq_clear (cs);
  CAMLreturn(Val_unit);
}

void seq_CAML_free_seq (value v) {
  seqt s;
  Seq_custom_val(s,v);
  free (s->head);
  free (s);
  return;
}

/** Compare two sequence */
int seq_compare (seqt a, seqt b) {
  int i;
  int la, lb;
  SEQT ca, cb;
  la = seq_get_len (a);
  lb = seq_get_len (b);
  if (lb != la) {
    if (la > lb) return 1;
    else return -1;
  }
  for (i = 0; i < la; i++) {
    ca = seq_get (a, i);
    cb = seq_get (b, i);
    if (ca != cb) {
      if (ca > cb) return 1;
      else return -1;
    }
  }
  return 0;
}
int seq_CAML_compare (value a, value b) {
  CAMLparam2(a,b);
  int cmp;
  seqt ap, bp;
  Seq_custom_val(ap,a);
  Seq_custom_val(bp,b);
  cmp = seq_compare (ap, bp);
  CAMLreturn(Val_int(cmp));
}

/** OCAML CUSTOM VALUES */
unsigned long seq_CAML_deserialize (void *v) {
  seqt n;
  SEQT *head;
  n = (seqt) v;
  head = (SEQT *) ((seqt) n + 1);
  n->cap = deserialize_sint_4();
  n->len = deserialize_sint_4();
  n->head = head;
  n->begin = head;
  n->end = n->head + n->cap - 1;
  assert (n->len > 0);
  DESERIALIZE_SEQT(n->begin,n->len);
  return ((n->len * sizeof(SEQT)) + sizeof(struct seq));
}

void seq_CAML_serialize (value vo, unsigned long *wsize_32, unsigned long *wsize_64) 
{
  seqt v;
  SEQT *tmp;
  Seq_custom_val(v,vo);
  serialize_int_4(v->len);
  serialize_int_4(v->len);
  tmp = v->begin;
  SERIALIZE_SEQT(tmp,v->len);
  *wsize_64 = *wsize_32 = sizeof(struct seq) + ((sizeof(SEQT) * (v -> len)));
  return;
}

long seq_CAML_hash (value v)
{
  long x = 0;
  seqt s;
  int i;
  Seq_custom_val(s,v);
  for (i = (seq_get_len(s)) - 1; i >= 0; i--)
    x ^= (((x << 5) + (x >> 2)) ^ seq_get(s,i));
  return x;
}

static struct custom_operations sequence_custom_operations  = {
    "http://www.amnh.org/poy/seq/seq.0.2",
    custom_finalize_default,
    (&seq_CAML_compare), 
    (&seq_CAML_hash), 
    (&seq_CAML_serialize),
    (&seq_CAML_deserialize)
};

value seq_CAML_register (value u) {
  CAMLparam1(u);
  register_custom_operations (&sequence_custom_operations);
  CAMLreturn (Val_unit);
}

value seq_CAML_create (value cap) {
  CAMLparam1(cap);
  CAMLlocal1(res);
  seqt tmp2;
  int len;
  size_t s;
  len = Int_val(cap);
  s = sizeof (SEQT) * len;
  res = caml_alloc_custom 
    (&sequence_custom_operations, (sizeof(struct seq)+s), len, SEQ_UNUSED_MEMORY);
  tmp2 = Seq_pointer(res);
  if (NULL == tmp2) failwith ("Memory error");
  tmp2->cap = len;
  tmp2->len = 0;
  tmp2->head = (SEQT *) ((seqt) (tmp2 + 1));
  if (tmp2->head == NULL) failwith ("Memory error.");
  tmp2->end = tmp2->begin = tmp2->head + len - 1;
  tmp2->begin++;
  assert (tmp2 == Seq_pointer(res));
  CAMLreturn(res);
}

value 
seq_CAML_copy (value from, value to) {
  CAMLparam2(from, to);
  seqt cto, cfrom;
  int i;
  Seq_custom_val(cto,to);
  Seq_custom_val(cfrom,from);
  assert (cto->cap >= cfrom->len);
  cto->len = 0;
  cto->begin = cto->end + 1;
  for (i = cfrom->len - 1; i > -1; i--)
    seq_prepend (cto, seq_get (cfrom, i));
  CAMLreturn(Val_unit);
}

value seq_CAML_count (value gap, value startNO, value seq) {
  CAMLparam3(gap, seq, startNO);
  seqt sc;
  int i, cnt = 0;
  SEQT cgap;
  int start;
  Seq_custom_val (sc,seq);
  cgap = Int_val(gap);
  start = Int_val(startNO);
  int tmp;
  for (i = 0; i < sc->len; i++){
    tmp = seq_get (sc, i); 
    if(start>0) {
      if ( (tmp>=start) || (tmp==cgap))
        cnt++;
    } else {
      if ( (tmp>=cgap) &&  (0 != (cgap & (seq_get (sc, i)))) )
        cnt++;
    }
  }
  CAMLreturn(Val_int(cnt));
}
