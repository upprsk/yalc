#include <stdio.h>

#include "da.h"
#include "tokenizer.h"

int main() {
    char const source[] = "123 123.456 +-*/";

    token_t* tokens = tokenize(source);
    if (!tokens) {
        fprintf(stderr, "failed to tokenize\n");
        return 1;
    }

    for (size_t i = 0; i < da_get_size(tokens); ++i) {
        printf("token (%d) [%d, %d]\n", tokens[i].type, tokens[i].span.start,
               tokens[i].span.end);
    }

    return 0;
}
