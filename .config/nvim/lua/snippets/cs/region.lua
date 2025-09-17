local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local region_snippets = {
  s("region_private_static_field", fmt([[
#region private static field

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_public_static_method", fmt([[
#region public static method

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_private_static_method", fmt([[
#region private static method

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_public_constructor", fmt([[
#region public constructor

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_private_constructor", fmt([[
#region private constructor

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_public_field", fmt([[
#region public field

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_private_field", fmt([[
#region private field

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_public_method", fmt([[
#region public method

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_protected_method", fmt([[
#region protected method

    {}

#endregion]], {
    i(1, "")
  })),

  s("region_private_method", fmt([[
#region private method

    {}

#endregion]], {
    i(1, "")
  }))
}

return region_snippets