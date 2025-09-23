local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

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

return { snippets = blocks_snippets, autosnippets = {} }
