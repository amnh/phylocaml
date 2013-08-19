#include <caml/alloc.h>     /* copy_double, et cetera */
#include <caml/memory.h>    /* caml_param, et cetera */
#include <caml/custom.h>    /* abstract types */

#define Val_none Val_int(0)

#define None_val Val_none

#define Some_val(v) Field(v,0)

value Val_some( value v );
