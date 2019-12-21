#pragma once

#define ABEND -1
#define TERMINATE 0
#define DEOPT_JIT 1
#define DEOPT_MEM 2

#define XSHIFT 8

#ifndef __ASSEMBLER__

#include <stdint.h>

#include "jintcode.h"

struct jintcode_context {
    struct jintcode_env env;
    uint64_t base;
    uint64_t imem_size;
    uint64_t xmem_size;
};

int64_t jintcode_enter(void *imem, void *xmem, int64_t *ip,
        struct jintcode_context *ctxt, int64_t *a) __asm__("_jintcode_enter");

__attribute__((aligned(8)))
extern const struct {
    void *data;
    uint8_t id;
    uint8_t inargs;
    uint8_t outargs;
    uint8_t size;
} deopt_jit __asm__("_deopt_jit"),
    prologue __asm__("_prologue"),
    operations[] __asm__("_operations"),
    get_args[] __asm__("_get_args"),
    set_args[] __asm__("_set_args");
extern const int operations_size __asm__("_operations_size");
extern const int get_args_size __asm__("_get_args_size");
extern const int set_args_size __asm__("_set_args_size");

#endif
