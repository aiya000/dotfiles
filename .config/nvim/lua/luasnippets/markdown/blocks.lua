local fmt = require('luasnip.extras.fmt').fmt
local i = require('luasnip').insert_node
local list = require('utils.list')
local sm = require('utils.luasnip').sm

local blocks_snippets = list.concat(
  sm(
    { 'block_haskell', 'blhs' },
    fmt(
      [[
```haskell
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_shell_session', 'blsh' },
    fmt(
      [[
```shell-session
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_vim', 'blvim' },
    fmt(
      [[
```vim
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_typescript', 'blts' },
    fmt(
      [[
```typescript
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_javascript', 'bljs' },
    fmt(
      [[
```javascript
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_vue', 'blvue', 'blv' },
    fmt(
      [[
```vue
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_lua', 'bllua', 'bll' },
    fmt(
      [[
```lua
{}
```]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_markdown', 'blmd' },
    fmt(
      [[
```markdown
{}
```]],
      {
        i(1, ''),
      }
    )
  )
)

return blocks_snippets
