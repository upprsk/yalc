# Random Notes

## Remove a key from JSON object

[https://stackoverflow.com/a/48529690](https://stackoverflow.com/a/48529690)

```jq
walk(if type == "object" then del(.maxHeight) else . end)
```

Remove all `"loc"` fields from AST:

```jq
walk(if type == "object" then del(.loc) else . end)
```

## Join various lines into array

```jq
jq -s .
```

## CMake

```bash
cmake -B build -S . -GNinja -DCMAKE_BUILD_TYPE=Debug
cmake --build build
```

### Get AST

Get the AST and remove `.loc` and declaration store from json objects.

```bash
cmake --build build && ./build/cmd/yalc test.yal --dump-ast-json --single-file | jq 'walk(if type == "object" then del(.loc, .ds) else . end)' | wl-copy
```
