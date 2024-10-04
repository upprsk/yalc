#include "fs.h"

#include <stdio.h>

#include "alloc/allocator.h"

char* read_entire_file(char const* filename, allocator_t alloc, size_t* len) {
    FILE* f = fopen(filename, "rb");
    if (!f) return NULL;

    char* buf = NULL;

    fseek(f, 0, SEEK_END);
    long tell_len = ftell(f);
    if (tell_len < 0) goto error;
    fseek(f, 0, SEEK_SET);

    buf = allocator_alloc(alloc, tell_len + 1);
    if (!buf) goto error;

    long read = fread(buf, sizeof(char), tell_len, f);
    if (read != tell_len) goto error;

    *len = tell_len;
    buf[tell_len] = 0;

    return buf;

error:
    if (f) fclose(f);
    if (buf) allocator_free(alloc, buf);

    return NULL;
}
