#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "utils/fnv1a.h"
#include "utils/macros.h"

#define MAP_TYPENAME(line) MERGE(map_, line)

/// Define the map type.
///
/// ```c
/// typedef map_t(int, int) map_int_int_t;
/// ```
#define map_t(K, V)                           \
    struct MAP_TYPENAME(__LINE__) {           \
        struct MAP_TYPENAME(__LINE__) * next; \
        K key;                                \
        V value;                              \
    }

/// Helper to get the map node type in the macros.
#define map_node_type(_map) typeof((_map)->next)

/// Get the length of the map by iterating all elements.
#define map_len(_map)                  \
    ({                                 \
        size_t len = 0;                \
        map_foreach(_map, node) len++; \
        len;                           \
    })

/// Put a new item in the map, duplicate keys are allowed and it just means that
/// the new key will be returned by `map_get` because it will be found first.
/// Adding a new key does not modify the current map, and old references are
/// still valid.
///
/// ```c
/// map_int_int_t* map = NULL;
/// assert_size(map_len(map), ==, 0);
///
/// map_int_int_t* newmap = map_put(alloc, map, 69, 420);
/// assert_size(map_len(map), ==, 0);
/// assert_size(map_len(newmap), ==, 1);
/// ```
#define map_put(_alloc, _map, _key, ...)                                   \
    ({                                                                     \
        map_node_type(_map) node = allocator_alloc(_alloc, sizeof(*node)); \
        assert_not_null(node);                                             \
        node->next = _map;                                                 \
        node->key = _key;                                                  \
        node->value = __VA_ARGS__;                                         \
        node;                                                              \
    })

/// Get a value in the map by key. This will search the map until the key is
/// found. If there is more than one entry with the given key the first one is
/// returned.
///
/// `_cmp` is a compare function for the keys. Check if there is already a
/// `map_cmp_*` function for your key type. There are also variants of get that
/// use a default comparison for commom keys (like `map_put_kint` for integer
/// keys).
///
/// If the key is not found, returns NULL.
#define map_get(_map, _key, _cmp)        \
    ({                                   \
        map_node_type(_map) res = NULL;  \
        map_foreach(_map, node) {        \
            if (_cmp(_key, node->key)) { \
                res = node;              \
                break;                   \
            }                            \
        }                                \
        res;                             \
    })

/// Remove the entry with the given key from the map. This will search the map
/// and then remove the entry with the given key. This function **MODIFIES** the
/// map. In case there is more than one entry with the given key, onny the first
/// one is removed.
///
/// `_cmp` is a compare function for the keys. Check if there is already a
/// `map_cmp_*` function for your key type. There are also variants of get that
/// use a default comparison for commom keys (like `map_del_kint` for integer
/// keys).
///
/// If the key is not present in the map, nothing will be done.
#define map_del(_map, _key, _out, _cmp)                                     \
    do {                                                                    \
        map_node_type(_map) node = _map;                                    \
        if (node) {                                                         \
            map_node_type(_map) prev = node;                                \
            for (node = node->next; node; prev = node, node = node->next) { \
                if (_cmp(_key, node->key)) {                                \
                    prev->next = node->next;                                \
                    if (_out) *(_out) = node;                               \
                    break;                                                  \
                }                                                           \
            }                                                               \
        };                                                                  \
    } while (0)

/// Check if the map contains an entry with the given key. This will search the
/// map until the key is found.
///
/// `_cmp` is a compare function for the keys. Check if there is already a
/// `map_cmp_*` function for your key type. There are also variants of get that
/// use a default comparison for commom keys (like `map_put_kint` for integer
/// keys).
#define map_contains(_map, _key, _cmp)   \
    ({                                   \
        bool contains = false;           \
        map_foreach(_map, node) {        \
            if (_cmp(_key, node->key)) { \
                contains = true;         \
                break;                   \
            }                            \
        }                                \
        contains;                        \
    })

/// Free all of the used memory by the map. This function **MODIFIES** the map.
/// After this function the map will be set to null.
#define map_free(_alloc, _map)                     \
    do {                                           \
        map_node_type(_map) node = _map;           \
        while (node) {                             \
            map_node_type(_map) next = node->next; \
            allocator_free(_alloc, node);          \
            node = next;                           \
        }                                          \
        _map = NULL;                               \
    } while (0)

/// Helper to iterate all entries of the map.
#define map_foreach(_map, _node) \
    for (map_node_type(_map) _node = _map; _node; _node = (_node)->next)

/// Compare helper for integer keys
#define map_cmp_int(_lhs, _rhs) ((_lhs) == (_rhs))

/// Compare helper for c-string keys
static inline bool map_cmp_cstr(char const* lhs, char const* rhs) {
    return strcmp(lhs, rhs) == 0;
}

#define map_get_kint(_map, _key)  map_get(_map, _key, map_cmp_int)
#define map_get_kcstr(_map, _key) map_get(_map, _key, map_cmp_cstr)

#define map_del_kint(_map, _key)  map_del(_map, _key, map_cmp_int)
#define map_del_kcstr(_map, _key) map_del(_map, _key, map_cmp_cstr)

#define map_contains_kint(_map, _key)  map_contains(_map, _key, map_cmp_int)
#define map_contains_kcstr(_map, _key) map_contains(_map, _key, map_cmp_cstr)

typedef map_t(int, int) map_int_int_t;
typedef map_t(char const*, int) map_cstr_int_t;

/// A better string key, with a length and pre-computed hash.
typedef struct strkey {
    char const* s;
    uint32_t    len;
    uint32_t    hash;
} strkey_t;

/// Create a new string key from the given string (calculates the hash).
static inline strkey_t strkey(char const* s, size_t len) {
    uint32_t hash = fnv1a32(s, len);

    return (strkey_t){.s = s, .len = len, .hash = hash};
}

/// Create a new string key from a c-string, calculating the length and hash.
static inline strkey_t mkstrkey(char const* cstr) {
    uint32_t len = strlen(cstr);
    uint32_t hash = fnv1a32(cstr, len);

    return (strkey_t){.s = cstr, .len = len, .hash = hash};
}

/// If two string keys are equal.
static inline bool strkey_eq(strkey_t lhs, strkey_t rhs) {
    if (lhs.len != rhs.len || lhs.hash != rhs.hash) return false;

    return memcmp(lhs.s, rhs.s, lhs.len) == 0;
}

#define map_get_kstr(_map, _key)      map_get(_map, _key, strkey_eq)
#define map_del_kstr(_map, _key)      map_del(_map, _key, strkey_eq)
#define map_contains_kstr(_map, _key) map_contains(_map, _key, strkey_eq)

/// Helper to defining a string map.
#define map_str_t(V) map_t(strkey_t, V)

typedef map_str_t(int) map_str_int_t;
