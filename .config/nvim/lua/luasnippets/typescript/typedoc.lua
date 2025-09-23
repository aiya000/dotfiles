local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm(
    { 'typedoc_param', 'param' },
    fmt(
      [[
    @param {varName} {description}
  ]],
      {
        varName = i(1, 'varName'),
        description = i(2, 'description'),
      }
    )
  ),

  sm(
    { 'typedoc_type_param', 'type_param' },
    fmt(
      [[
    @typeParam
  ]],
      {}
    )
  ),

  sm(
    { 'typedoc_returns', 'returns' },
    fmt(
      [[
    @returns
  ]],
      {}
    )
  ),

  {
    s(
      'typedoc_event',
      fmt(
        [[
      @event
    ]],
        {}
      )
    ),
  },

  sm(
    { 'typedoc_hidden', 'hidden' },
    fmt(
      [[
    @hidden
  ]],
      {}
    )
  ),

  sm(
    { 'typedoc_ignore', 'ignore' },
    fmt(
      [[
    @ignore
  ]],
      {}
    )
  ),

  sm(
    { 'typedoc_internal', 'internal' },
    fmt(
      [[
    @internal
  ]],
      {}
    )
  ),

  sm(
    { 'typedoc_category', 'category' },
    fmt(
      [[
    @category
  ]],
      {}
    )
  ),

  sm(
    { 'typedoc_package_documentation', 'package_documentation' },
    fmt(
      [[
    @packageDocumentation
  ]],
      {}
    )
  ),

  {
    s(
      'see',
      fmt(
        [[
      @see
    ]],
        {}
      )
    ),

    s(
      'link',
      fmt(
        [[
      {{@link {stuff}}}
    ]],
        {
          stuff = i(1, 'stuff'),
        }
      )
    ),

    s(
      'throws',
      fmt(
        [[
      @throws
    ]],
        {}
      )
    ),
  },

  sm(
    { 'document_comment', 'doc' },
    fmt(
      [[
    /**
     * {}
     */
  ]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_markdown', 'blm' },
    fmt(
      [[
    ```
    * {}
    * ```
  ]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'block_vue', 'blvue' },
    fmt(
      [[
    ```vue
    {}
    ```
  ]],
      {
        i(1, ''),
      }
    )
  ),

  sm(
    { 'typedoc_example', 'example' },
    fmt(
      [[
    @example
  ]],
      {}
    )
  ),

  sm(
    { 'tsdoc_testify_block', 'testify' },
    fmt(
      [[
   * @example
   * ```
    import {{ {deepEqual} }} from 'assert'
    {}

    {deepEqual}(
     {actual},
     {expected},
    )
   * ```
  ]],
      {
        deepEqual = i(1, 'deepEqual'),
        i(2, ''),
        actual = i(3, 'actual'),
        expected = i(4, 'expected'),
      }
    )
  ),

  {
    s(
      'tsdoc_testify_block_dummy',
      fmt(
        [[
     * @example
     * ```
     * // dummy
     * ```
    ]],
        {}
      )
    ),
  }
)
