# AGENTS.md for Neovim Configuration

## Coding

- **SHOULD**: Use `('message %s:%s'):format(foo, bar)` method instead of `string.format('message %s:%s', foo, bar)`
    - Reason: The former is more efficient and readable
- **SHOULD**: Use explicit nil checks (`x == nil` or `x ~= nil`) instead of truthy checks (`if x then`)
    - Reserve `if x then` for boolean values only
    - Example: `if tsdk == nil then` instead of `if not tsdk then`
