#include "caml/config.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include <string.h>

/* Here are a pair of very useful functions that OCaml's
 * byterun/afl.c should really expose directly...
 */

#define AFL_BYTES (1 << 16)
extern unsigned char* caml_afl_area_ptr;

CAMLprim value caml_instrumentation_buffer_size ()
{
  return Val_int(AFL_BYTES);
}

CAMLprim value caml_gather_instrumentation(value buf)
{
  CAMLparam1 (buf);
  if (caml_string_length (buf) != AFL_BYTES) {
    caml_failwith("caml_gather_instrumentation: wrong buffer size");
  }
  memcpy(String_val(buf), caml_afl_area_ptr, AFL_BYTES);
  CAMLreturn (Val_unit);
}
