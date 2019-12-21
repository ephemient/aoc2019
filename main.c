#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "jintcode.h"

static int64_t read_fn(void) {
    int64_t i;
    scanf(" %"SCNd64, &i);
    return i;
}

static void write_fn(int64_t i) {
    printf("%"PRId64"\n", i);
}

static struct jintcode_env env = {
    .read_fn = read_fn,
    .write_fn = write_fn,
};

static void getints(char *line, int64_t **ints, size_t *n) {
    size_t sz = *n;
    *n = 0;
    for (char *s = line, *end;; s = end) {
        while (*s == ',') s++;
        if (!*s) break;
        int64_t i = strtoll(s, &end, 10);
        if (s == end) break;
        if (*n >= sz) {
            size_t new_sz = *n > 0 ? 2 * *n : 16;
            *ints = realloc(*ints, new_sz * sizeof(int64_t));
            memset(*ints + sz, 0, new_sz - sz);
            sz = new_sz;
        }
        (*ints)[(*n)++] = i;
    }
}

int main(int argc, char **argv) {
    int64_t *ints = NULL;
    char *line = NULL;
    size_t n = 0;
    FILE *f = fopen("input.txt", "r");
    getline(&line, &n, f);
    fclose(f);
    n = 0;
    getints(line, &ints, &n);
    free(line);
    printf("%lu:", n);
    for (int i = 0; i < n; i++) {
        printf("%c%"PRId64, i ? ',' : ' ', ints[i]);
    }
    printf("\n");
    jintcode_run(&env, &ints, &n);
    free(ints);
}
