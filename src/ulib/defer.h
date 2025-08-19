// https://gustedt.wordpress.com/2025/01/06/simple-defer-ready-to-use/
#pragma once

#ifdef __CLANGD__

// clangd does not understand this, so make it all go away
#define defer

#else

#define __DEFER__(F, V)                                                        \
    auto void               F(int *);                                          \
    [[gnu::cleanup(F)]] int V;                                                 \
    auto void               F(int *)

#define defer       __DEFER(__COUNTER__)
#define __DEFER(N)  __DEFER_(N)
#define __DEFER_(N) __DEFER__(__DEFER_FUNCTION_##N, __DEFER_VARIABLE_##N)

#endif
