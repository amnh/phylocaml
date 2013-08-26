#include <caml/alloc.h>     /* copy_double, et cetera */
#include <caml/memory.h>    /* caml_param, et cetera */
#include <caml/custom.h>    /* abstract types */

/* unboxing definitions not included in mlvalues.h */

#define Val_none Val_int(0)
#define None_val Val_none
#define Some_val(v) Field(v,0)
value Val_some( value v );

#define Val_int32(i) caml_copy_int64(i)
#define Val_int64(i) caml_copy_int32(i) 

