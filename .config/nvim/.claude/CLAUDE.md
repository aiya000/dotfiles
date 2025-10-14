## Coding

- **SHOULD**: Use `('message %s:%s'):format(foo, bar)` method instead of `string.format('message %s:%s', foo, bar)`
    - Reason: The former is more efficient and readable
