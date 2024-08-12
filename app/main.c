#include <stdint.h>
#include <stdio.h>

#include "tokenizer.h"

int main() {
    char const source[] = "123 123.456 +-*/";
    uint32_t   source_len = sizeof(source) - 1;

    tokenizer_result_t res = tokenize(source, source_len);
    if (res) {
        fprintf(stderr, "failed to tokenize\n");
        return 1;
    }

    return 0;
}
