#pragma once

/// Define the linked list type.
///
/// ```c
/// typedef lst_t(int) lst_int_t;
/// ```
#define lst_t(T)                     \
    struct lst_##__LINE__ {          \
        struct lst_##__LINE__* next; \
        T                      data; \
    }

/// Calculate the length of the list by iterating all elements.
#define lst_len(_lst)                                                     \
    ({                                                                    \
        size_t len = 0;                                                   \
        for (typeof((_lst)->next) node = _lst; node; node = node->next) { \
            len += 1;                                                     \
        }                                                                 \
        len;                                                              \
    })

/// Prepend to the start of the list, returning a new list with the item in the
/// front.
#define lst_prepend(_alloc, _lst, ...)                      \
    ({                                                      \
        typeof((_lst)->next) node =                         \
            allocator_alloc(_alloc, sizeof(*(_lst)->next)); \
        assert_not_null(node);                              \
        node->next = _lst;                                  \
        node->data = __VA_ARGS__;                           \
        node;                                               \
    })

/// Pop the front element from the list. Nothing is done if the list is empty.
/// The front element will also be modified to not point to the rest of the
/// list. This function is meant to be used where you want to keep the removed
/// element (use it as the head of a new list for example), so it is not
/// deallocated.
#define lst_pop_front(_lst)               \
    ({                                    \
        typeof((_lst)->next) next = NULL; \
        if (_lst) {                       \
            next = (_lst)->next;          \
            (_lst)->next = NULL;          \
        }                                 \
        next;                             \
    })

/// Pop and free the front item of the list.
#define lst_pop_free_front(_alloc, _lst)  \
    ({                                    \
        typeof((_lst)->next) next = NULL; \
        if (_lst) {                       \
            next = (_lst)->next;          \
            allocator_free(_alloc, _lst); \
        }                                 \
        next;                             \
    })

/// Free the linked list. Note that this frees the entire list. Make sure that
/// there are no other references to the nodes.
#define lst_free(_alloc, _lst)                    \
    do {                                          \
        typeof((_lst)->next) it = _lst;           \
        while (it) {                              \
            typeof((_lst)->next) next = it->next; \
            allocator_free(_alloc, it);           \
            it = next;                            \
        }                                         \
    } while (0)

/// A for-each construct for linked lists.
///
/// ```c
/// lst_for_each(lst, node) {
///     printf("%d\n", node->data);
/// }
#define lst_for_each(_lst, _it) \
    for (typeof((_lst)->next) _it = _lst; _it; _it = (_it)->next)

typedef lst_t(int) lst_int_t;
