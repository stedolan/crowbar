#define _GNU_SOURCE
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




#include <stdio.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>
#include <sys/ucontext.h>
#include <caml/alloc.h>

static void* volatile pc;
static void handler(int sig, siginfo_t* info, void* context) {
  if (!pc) pc = (void*)(((ucontext_t*)context)->uc_mcontext.gregs[REG_RIP]);
}

static int watch_byte(volatile void* p) {
#if !defined(__linux__) || !defined(__x86_64__)
  errno = ENOSYS;
  return -1;
#else
  pid_t self = getpid(), child;
  int err;

  if (p) {
    pc = 0;
    struct sigaction sa;
    sa.sa_sigaction = handler;
    sa.sa_flags = SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGTRAP, &sa, 0);
  } else {
    //signal(SIGTRAP, SIG_DFL);
  }



  if ((child = fork()) == 0) {
    int status;
    err = ptrace(PTRACE_ATTACH, self, 0L, 0L);
    if (err) _exit(errno);

    if (waitpid(self, &status, __WALL) != self) _exit(errno);

    if (p) {
      long dr7 =
           0x1        /* global (not reset at task switch) */
        | (0x1 << 16) /* trigger on write */
        | (0x0 << 18) /* one byte */
        | (0x7 << 8)  /* weird flags (LE, GE, ?) */;
      err = ptrace(PTRACE_POKEUSER, self, offsetof(struct user, u_debugreg) + 0 * sizeof(long), p);
      if (err) _exit(errno);
      err = ptrace(PTRACE_POKEUSER, self, offsetof(struct user, u_debugreg) + 7 * sizeof(long), dr7);
      if (err) _exit(errno);
    } else {
      err = ptrace(PTRACE_POKEUSER, self, offsetof(struct user, u_debugreg) + 7 * sizeof(long), 0L);
      if (err) _exit(errno);
    }
    err = ptrace(PTRACE_POKEUSER, self, offsetof(struct user, u_debugreg) + 6 * sizeof(long), 0L);
    if (err) _exit(errno);
    
    err = ptrace(PTRACE_CONT, self, 0L, 0L);
    if (err) _exit(errno);

    _exit(0);
  } else {
    pid_t wres;
    int wstat;
    if (child < 0) return -1;
    while (waitpid(child, &wstat, 0) != child) { if (errno != EINTR) return -1; }
    if (!WIFEXITED(wstat)) { errno = EIO; return -1; }
    if (WEXITSTATUS(wstat)) { errno = WEXITSTATUS(wstat); return -1; }
    return 0;
  }
#endif
}


CAMLprim value caml_instrumentation_set_watch(value bit)
{
  int b = Int_val(bit);
  void* p;
  if (b == -1) p = 0;
  else if (0 <= b && b < AFL_BYTES) p = caml_afl_area_ptr + b;
  else caml_invalid_argument("instrumentation bit outside valid range");

  if (watch_byte(p) < 0) {
    char msg[256];
    sprintf(msg, "setting watch failed: %s", strerror(errno));
    caml_failwith(msg);
  }
  return Val_unit;
}

CAMLprim value caml_instrumentation_get_pc(value unit)
{
  void* r = pc;
  pc = 0;
  return caml_copy_nativeint((intnat)r);
}
