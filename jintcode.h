#pragma once

#ifndef __ASSEMBLER__

#include <stdint.h>

struct jintcode_env {
    int64_t (*read_fn)(void);
    void (*write_fn)(int64_t);
};

int jintcode_run(struct jintcode_env *, int64_t **, size_t *);

#endif
