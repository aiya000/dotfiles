local fmt = require('luasnip.extras.fmt').fmt
local i = require('luasnip').insert_node
local list = require('utils.list')
local sm = require('utils.luasnip').sm

return list.concat(
  sm(
    { 'html_attribute', 'attribute', 'attr', 'att' },
    fmt('{}="{}"', {
      i(1, 'name'),
      i(2, 'here'),
    })
  ),

  sm(
    { 'colspan' },
    fmt('colspan="{}"', {
      i(1, 'here'),
    })
  ),

  sm(
    { 'class', 'cl', 'cla', 'html_class' },
    fmt('class="{}"', {
      i(1, 'name'),
    })
  ),

  sm(
    { 'id' },
    fmt('id="{}"', {
      i(1, 'id'),
    })
  ),

  sm(
    { 'height' },
    fmt('height="{}"', {
      i(1, '64'),
    })
  ),

  sm(
    { 'width' },
    fmt('width="{}"', {
      i(1, '64'),
    })
  ),

  sm(
    { 'name' },
    fmt('name="{}"', {
      i(1, 'here'),
    })
  ),

  sm(
    { 'type' },
    fmt('type="{}"', {
      i(1, 'here'),
    })
  ),

  sm(
    { 'rel' },
    fmt('rel="{}"', {
      i(1, 'here'),
    })
  ),

  sm(
    { 'alt' },
    fmt('alt="{}"', {
      i(1, 'here'),
    })
  ),

  sm(
    { 'href' },
    fmt('href="{}"', {
      i(1, 'here'),
    })
  ),

  sm(
    { 'style' },
    fmt('style="{}"', {
      i(1, 'here'),
    })
  )
)
