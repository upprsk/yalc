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
