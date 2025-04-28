#include <stdio.h>

void print_int(int v) { printf("%d\n", v); }
void print_str(char const *s, int len) { printf("%.*s\n", len, s); }
void print_cstr(char const *s) { printf("%s\n", s); }
