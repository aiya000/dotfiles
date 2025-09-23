local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.extend_decorator.apply(s, {}, {
  common = {},
  condition = function()
    return true
  end,
})

return {
  -- haskell package.yaml template
  sm(
    {
      { trig = 'haskell_package_yaml', dscr = 'haskell package.yaml' },
      { trig = 'package_yaml', dscr = 'haskell package.yaml' },
      { trig = 'stack_package_yaml', dscr = 'haskell package.yaml' },
    },
    fmt(
      [[
name: {}
version: 0.1.0.0
category: Simple
author: aiya000
maintainer: aiya000 <aiya000.develop@gmail.com>
copyright: aiya000
license: MIT
homepage: https://github.com/aiya000/hs-{}
description: {}
synopsis: {}

ghc-options:
    - -Wall
    - -Wno-name-shadowing
    - -Wno-unused-do-bind
    - -Wno-orphans
    - -fprint-potential-instances
    - -fprint-explicit-kinds

dependencies:
    - base >= 4.7 && < 5

library:
  source-dirs: src

executables:
  {}:
    main: Main.hs
    source-dirs: app
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies: {}

tests:
  doctest:
    main: DocTest.hs
    source-dirs:
      - test/doctest
      - src
    dependencies:
      - doctest
  tasty-test:
    main: Tasty.hs
    source-dirs:
      - test/tasty
      - src
    dependencies:
      - tasty
      - tasty-discover]],
      {
        i(1, 'package-name'),
        i(2),
        i(3, 'subject'),
        i(4, 'what-do-this'),
        i(5),
        i(0),
      }
    )
  ),
}
