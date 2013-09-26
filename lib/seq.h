#include <stdio.h>
#include <assert.h>

#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

/* Macro to retrieve and cast a pointer to a seq structure from the Ocaml custom
 * type. */
#define Seq_pointer(a) ((struct seq *) Data_custom_val(a))
#define Seq_custom_val(to_asgn,a)  to_asgn = Seq_pointer(a)

/* #define Seq_custom_val(to_asgn,a)  to_asgn = Seq_pointer(a) */
/*  to_asgn->head = (SEQT *) ((seqt) (to_asgn + 1)); */
/*  to_asgn->end = to_asgn->head + to_asgn->cap - 1; */
/*  to_asgn->begin = to_asgn->end - to_asgn->len + 1 */

/** This will allow us to later create more efficient implementations by
 * scripting the indexes of the data (INDEXSIZE) and width of data (SEQT). */
#define SEQT unsigned char
#define DESERIALIZE_SEQT(a,b) caml_deserialize_block_1((a),(b))
#define SERIALIZE_SEQT(a,b) caml_serialize_block_1((a),(b))
#define INDEXSIZE int

/* Sequence structure to be used inside ocaml custom types. */
struct seq {
    int cap;        /* Capacity of the sequence memory structure. */
    int len;        /* Total length of the sequence stored. */
    SEQT *head;
    SEQT *begin;    /* Offset of the position where the first element of 
                       the sequence is actually stored. */
    SEQT *end;      /* The last element in the seq; begin=end when len=0 */
};
typedef struct seq * seqt;

/** returns the capacity of the data */
int seq_get_cap (const seqt a);

/** prepends the seq with value v */
void seq_prepend (seqt a, SEQT v);

/** returns the length of the seq data */
int seq_get_len (const seqt a);

/** returns the starting location of the sequence data */
SEQT* seq_get_begin (const seqt a);

/** clears the sequence; only resets pointer locations and size values */
void seq_clear (seqt s);

/** Gets pointer to the beginning where the sequence is stored. begin != head. */
SEQT* seq_get_head (const seqt a);

/** Gets pointer where the last element of the sequence a is stored. */
SEQT* seq_get_end (const seqt a);

/* Gets value of the sequence a in the position p, a starting in position 0 */
SEQT seq_get (const seqt a, int p);

/** Stores the value v in the position p of sequence a. */
void seq_set (seqt a, int p, SEQT v);
