#include "phyloc.h"

value Val_some( value v )
{
    value some;
    some = caml_alloc(1, 0);
    Store_field(some, 0, v);
    return some;
}
