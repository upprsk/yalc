#include "fs.h"

#include <stdio.h>

#include "alloc/allocator.h"
#include "slice/slice.h"

fs_result_t read_entire_file(char const* filename, allocator_t alloc,
                             slice_char_t* c) {
    FILE* f = fopen(filename, "rb");
    if (!f) return FSR_NOT_OPEN;

    char* buf = NULL;

    fseek(f, 0, SEEK_END);
    long tell_len = ftell(f);
    if (tell_len < 0) goto error;
    fseek(f, 0, SEEK_SET);

    buf = allocator_alloc(alloc, tell_len + 1);
    if (!buf) goto error;

    long read = fread(buf, sizeof(char), tell_len, f);
    if (read != tell_len) goto error;

    fclose(f);

    buf[tell_len] = 0;

    *c = (slice_char_t){.ptr = buf, .len = tell_len};

    return FSR_OK;

error:
    if (f) fclose(f);
    if (buf) allocator_free(alloc, buf);

    return FSR_OK;
}
