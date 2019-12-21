#define _GNU_SOURCE

#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>

#include "jintcode.h"
#include "tables.h"

static int64_t extend_xmem(
        unsigned char (**xmem)[1 << XSHIFT], int64_t sz, int64_t new_sz) {
    if (*xmem) {
        mprotect(*xmem, sz << XSHIFT, PROT_READ|PROT_WRITE);
        *xmem = mremap(
                *xmem, sz << XSHIFT, new_sz << XSHIFT, MREMAP_MAYMOVE, NULL);
    } else {
        *xmem = mmap(NULL, new_sz << XSHIFT,
                PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    }
    for (int64_t i = sz; i < new_sz; i++) {
        memcpy((*xmem)[i], deopt_jit.data, deopt_jit.size);
    }
    mprotect(*xmem, new_sz << XSHIFT, PROT_READ|PROT_EXEC);
    return new_sz;
}

int jintcode_run(
        struct jintcode_env *env, int64_t **const mem, size_t *const mem_sz) {
    struct jintcode_context ctxt = {
        .env = *env,
        .base = 0,
        .imem_size = *mem_sz,
        .xmem_size = 0,
    };
    unsigned char (*xmem)[1 << XSHIFT] = NULL;
    int64_t rc;

    fprintf(stderr, "deopt_jit = %p:%d\n", deopt_jit.data, deopt_jit.size);
    fprintf(stderr, "prologue = %p:%d:%d\n",
            prologue.data, prologue.id, prologue.size);
    for (int i = 0; i < operations_size; i++) {
        fprintf(stderr, "operation[%d] = %p:%d:%d:%d:%d\n", i,
                operations[i].data, operations[i].id, operations[i].inargs,
                operations[i].outargs, operations[i].size);
    }
    for (int i = 0; i < get_args_size; i++) {
        fprintf(stderr, "get[%d] = %p:%d:%d\n", i,
                get_args[i].data, get_args[i].id, get_args[i].size);
    }
    for (int i = 0; i < set_args_size; i++) {
        fprintf(stderr, "set[%d] = %p:%d:%d\n", i,
                set_args[i].data, set_args[i].id, set_args[i].size);
    }

    for (int64_t ip = 0;;) {
        int64_t a;
        if (ip >= ctxt.xmem_size) {
            ctxt.xmem_size = extend_xmem(&xmem, ctxt.xmem_size, ip + 1);
        }
        fprintf(stderr, "imem=%p xmem=%p ip=%"PRId64"\n", *mem, xmem, ip);
        rc = jintcode_enter(*mem, xmem, &ip, &ctxt, &a);
        fprintf(stderr, "rc = %"PRId64" ip=%"PRId64" a=%"PRId64"\n", rc, ip, a);
        break;
    }

    if (xmem) munmap(xmem, ctxt.xmem_size << XSHIFT);

    return rc != 0;
}
