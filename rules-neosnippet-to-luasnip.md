次のルールにしたがって
~/.dotfiles/.vim/neosnippets
配下の.snip（neosnippet形式）を
~/.dotfiles/.config/nvim/snippets
配下の.lua（LuaSnip形式）に、**全て**変換して！

## ルール

### .snipに書いてあるsnipの順序を、.luaでも維持する

ただしneosnippets配下には`neosnippets/(filetype)/(事柄).snip`
例えば`neosnippets/typescript/eslint.snip`があり、
LuaSnipはサブディレクトリに対応していないため、
以下のルールで1ファイルにまとめる。

- まずは`neosnippets/(filetype)/(filetype).snip`を先頭に、
  `snippets/(filetype).lua`に変換する
- 次の上述のファイルを除いた`neosnippets/(filetype)/(事柄).snip`を、
  `(事柄).snip`のアルファベット順で、`snippets/(filetype).lua`に追記していく
- これらのファイルはセクション`-- {{{`, `-- }}}`で分ける

例: 以下がある場合

neosnippets/typescript/zod.snip
```
snippet zod_interface
abbr const ${1:name} = z.object({${0:#:here})
    const ${1:name} = z.object({${0:#:here}})
```

neosnippets/typescript/typescript.snip
```
snippet for_of
abbr for (const ${1:x} of ${2:xs}) {${0:#:here}}
    for (const ${1:x} of ${2:xs}) {${0:#:here}}
```

neosnippets/typescript/tsdoc.snip
```
snippet typedoc_param
alias param
    @param ${1:varName} ${2:description}
```

を変換すると

snippets/typescript.lua
```lua
-- General {{{

-- neosnippets/typescript/typescript.snipの変換結果

-- }}}
-- tsdoc {{{

-- neosnippets/typescript/tsdoc.snipの…

-- }}}
-- zod {{{

-- neosnippets/typescript/zod.snipの

-- }}}
```

となる。

### includeについて（解決済み）

neosnippets/foo.snip
```
include eslint.snip
include deno-lint.snip
```

**解決方法**: 動的requireを使用

#### include構造の正しい再現

neosnippetのincludeは、言語に依存しない共通スニペットを参照する仕組みです。
例：`neosnippets/typescript/typescript.snip`が`include eslint.snip`している場合、
`eslint.snip`は`neosnippets/`直下にある共通ファイルです。

LuaSnipでは以下の構造で再現：

```
lua/snippets/
├── javascript/              # 共通スニペット用ディレクトリ
│   ├── eslint.lua          # 共通スニペット
│   ├── deno-lint.lua       # 共通スニペット
│   └── reactnative.lua     # JavaScript固有スニペット
├── javascript.lua          # JavaScriptメインファイル
├── typescript/             # TypeScript固有スニペット
│   ├── jest.lua
│   ├── gas.lua
│   └── ...
└── typescript.lua          # TypeScriptメインファイル
```

#### requireパスの設計

**JavaScriptメインファイル** (`javascript.lua`):
```lua
local list = require('utils.list')

return list.concat(
  require('snippets.javascript.eslint'),      -- 共通
  require('snippets.javascript.deno-lint'),   -- 共通
  require('snippets.javascript.reactnative')  -- JavaScript固有
)
```

**TypeScriptメインファイル** (`typescript.lua`):
```lua
local list = require('utils.list')

return list.concat(
  require('snippets.javascript.eslint'),      -- 共通スニペットをJavaScriptから参照
  require('snippets.javascript.deno-lint'),   -- 共通スニペットをJavaScriptから参照
  require('snippets.typescript.jest'),        -- TypeScript固有
  require('snippets.typescript.gas'),         -- TypeScript固有
  -- その他TypeScript固有スニペット
)
```

これにより、neosnippetのinclude構造を正確に再現できます。

#### 特殊言語での追加考慮事項

**ファイル名の命名規則**:
- ドット区切りのファイル名（例: `typescript.tsx`）はLuaのrequireで問題となる
- 解決方法: アンダースコア区切りに変換する（例: `typescript_tsx`）

**複雑なエイリアス処理**:
- neosnippetの`alias`ディレクティブは、LuaSnipでは直接サポートされていない
- 解決方法: `sm({'trigger1', 'trigger2'}, snippet_def)`関数で実装
- s関数は単一トリガーのみサポート、複数トリガーには必ずsm関数を使用する

**言語固有の特殊パターン**:
- **FXML**: JavaFX特有の`fx:controller`、GridPane属性の適切な処理
- **Re:VIEW**: 技術書執筆用マークアップ、キャラクター会話システム（`//talkright`等）
- **XAML**: WPF/UWP、DataBinding、RelativeSource構文の複雑なエスケープ
- **Hamlet**: Yesod/Haskell Webテンプレートの特殊記法

### 変換例

neosnippet
```
# aliasがあるsnip
snippet for_await
alias forawait fora
abbr for await (const ${1:x} of ${2:xs}) {${0:#:here}}
    for await (const ${1:x} of ${2:xs}) {${0:#:here}}

# aliasがないsnip
snippet for_in
abbr for (const ${1:i} in ${2:xs}) {${0:#:here}}
    for (const ${1:i} in ${2:xs}) {${0:#:here}}

# 同じくaliasがないsnip
snippet for_traditional
abbr for (${1:let i = 0}; ${2:i < x}; ${3:i++}) {${0:#:here}}
    for (${1:let i = 0}; ${2:i < x}; ${3:i++}) {${0:#:here}}

# またaliasがあるsnip
snippet throw_new_error
alias throw
abbr throw new Error(${0:#:here})
    throw new Error(${0:#:here})
```

- 必ずfmtを使うこと
- ${0}は最後に{}として`i(last_index, '')`に変換すること
- **重要**: 最終ジャンプポイント（${0}）は位置ベースの`{}`として使用し、名前付きプレースホルダーは使わない
- smはスニペットのリストを返すので、list.concatでまとめること
- **重要**: vim.tbl_extendとsm関数の組み合わせは動作しないため、list.concatを使用する

#### 最終ジャンプポイントの正しい変換

**❌ 間違い（名前付きプレースホルダーを使用）**:
```lua
sm({'const_function', 'cfun'}, fmt('const {name} = ({args}) => {body}', {
  name = i(1, 'name'),
  args = i(2, 'args'),
  body = i(3, ''),  -- 名前付きプレースホルダー
}))
```

**✅ 正しい（位置ベースの{}を使用）**:
```lua
sm({'const_function', 'cfun'}, fmt('const {name} = ({args}) => {}', {
  name = i(1, 'name'),
  args = i(2, 'args'),
  i(3, ''),  -- 位置ベース
}))
```

これは、neosnippetの`${0}`が最終ジャンプポイントとして機能するため、
LuaSnipでも位置ベースの`{}`として実装する必要があるためです。

LuaSnip
```lua
local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm({'for_await', 'forawait', 'fora'}, fmt([[
    for await (const {x} of {xs}) {{
      {}
    }}
  ]], {
    x = i(1, 'x'),
    xs = i(2, 'xs'),
    i(3, ''),
  })),

  {
    s('for_in', fmt([[
      for (const {i} in {xs}) {{
        {}
      }}
    ]], {
      i = i(1, 'i'),
      xs = i(2, 'xs'),
      i(3, ''),
    })),

    s('for_traditional', fmt([[
      for ({let i = 0}; {i < x}; {i++}) {{
        {}
      }}
    ]], {
      ['let i = 0'] = i(1, 'let i = 0'),
      ['i < x'] = i(2, 'i < x'),
      ['i++'] = i(3, 'i++'),
      i(4, ''),
    })),
  },

  sm({'throw_new_error', 'throw'}, fmt([[
    throw new Error({})
  ]], {
    i(1, ''),
  }))
)
```

## 実装詳細

### ディレクトリ構造

```
.config/nvim/lua/snippets/
├── typescript.lua           # メインファイル
├── typescript/
│   ├── eslint.lua          # includeファイル
│   ├── deno-lint.lua
│   ├── gas.lua
│   ├── jest.lua
│   └── ...
├── python.lua              # 他の言語
├── python/
│   └── ...
└── ...
```

### LuaSnip設定

`lua/plugins.lua` に以下を追加:

```lua
{
  'L3MON4D3/LuaSnip',
  version = 'v2.*',
  build = 'make install_jsregexp',
  config = function()
    local ls = require('luasnip')

    -- 基本設定
    ls.config.set_config({
      history = true,
      updateevents = 'TextChanged,TextChangedI',
    })

    -- スニペットロード
    require('luasnip.loaders.from_lua').load({
      paths = vim.fn.stdpath('config') .. '/lua/snippets',
    })
  end,
}
```

### 推奨ヘッダー構造

すべてのスニペットファイルで統一されたヘッダーを使用:

```lua
local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node  -- 単純文字列スニペット用

return list.concat(
  -- スニペット定義
)
```

### 単純文字列スニペットの正しい書き方

LuaSnipでは単純な文字列を返すスニペットでも`t()`関数を使用する必要があります：

```lua
-- ❌ 間違い（ipairsエラーの原因）
{
  s('pub', 'public'),
  s('pri', 'private'),
}

-- ✅ 正しい（t関数を使用）
{
  s('pub', t('public')),
  s('pri', t('private')),
}
```

この違いは`bad argument #1 to 'ipairs' (table expected, got string)`エラーの主要な原因の一つです。

### fmt関数の注意点

#### 正しい使い方

```lua
-- ❌ 間違い
fmt([[
  throw new Error(`unreachable: ${{{}}} satisfies never}`)
]], {
  var = i(1, 'var'),
})

-- ✅ 正しい
fmt([[
  throw new Error(`unreachable: ${{{var} satisfies never}}`)
]], {
  var = i(1, 'var'),
})
```

#### 名前付きプレースホルダー

- `{name}` → `name = i(1, 'default')`
- `{}` → `i(1, 'default')` （位置による）

#### 特殊文字のエスケープ

- `{{` `}}` → リテラル `{` `}`
- `{{{name}}}` → `{` + プレースホルダー + `}`

#### テンプレートリテラル内での複雑なエスケープ

JavaScriptのテンプレートリテラル（`` `${variable}` ``）を含むスニペットでは特に注意が必要：

```lua
-- ❌ 間違い（引数が使われない）
fmt('throw new Error(`unreachable: ${{value}} satisfies never}`)', {
  value = i(1, 'var'),
})

-- ✅ 正しい（[[]]形式と正しいエスケープ）
fmt([[
  throw new Error(`unreachable: ${{{value}}} satisfies never`)
]], {
  value = i(1, 'value'),
})
```

エスケープの説明：
- `` `unreachable: `` → リテラル文字列
- `${{` → リテラル `${`
- `{value}` → プレースホルダー（引数テーブルの`value`を使用）
- `}}` → リテラル `}`
- `` satisfies never` `` → リテラル文字列

結果: `` throw new Error(`unreachable: ${[入力値]} satisfies never`) ``

#### オブジェクト構文での複雑なエスケープ

オブジェクトリテラルを含むスニペットでは、波括弧のエスケープに注意：

```lua
-- ❌ 間違い（エスケープが足りない）
fmt([[
  const {name} = z.object({{{}}})
]], {
  name = i(1, 'name'),
  i(2, ''),
})

-- ✅ 正しい
fmt([[
  const {name} = z.object({{
    {}
  }})
]], {
  name = i(1, 'name'),
  i(2, ''),
})
```

波括弧の意味：
- 最初の `{{` → リテラル `{`（オブジェクト開始）
- 内側の `{}` → プレースホルダー（カーソル位置）
- 最後の `}}` → リテラル `}`（オブジェクト終了）

結果: `const name = z.object({ [カーソル] })`

## コード品質ガイドライン

### fmt文字列の推奨形式

**単行の短いスニペット**:
```lua
-- ❌ 間違い（文字列を直接指定）
s('break', 'break')

-- ✅ 正しい（t関数を使用）
s('break', t('break'))
```

**複行スニペットは[[]]形式と名前付きプレースホルダーを使用**:
```lua
-- ❌ 悪い例
sm({'for_of', 'for'}, fmt('for (const {} of {}) {{{}}}', { i(1, 'x'), i(2, 'xs'), i(3, '') }))

-- ✅ 良い例
sm({'for_of', 'for'}, fmt([[
  for (const {x} of {xs}) {{
    {}
  }}
]], {
  x = i(1, 'x'),
  xs = i(2, 'xs'),
  i(3, ''),
}))
```

### スニペットのグループ化

**連続する単一スニペットは一つの配列にまとめる**:
```lua
-- ❌ 悪い例
{ s('for_in', ...) },
{ s('for_traditional', ...) },
{ s('while', ...) },

-- ✅ 良い例
{
  s('for_in', ...),
  s('for_traditional', ...),
  s('while', ...),
}
```

### スペーシング

スニペット定義間に空行を入れて読みやすくする:
```lua
return list.concat(
  sm({'alias1', 'alias2'}, fmt([[
    template content
  ]], {
    var = i(1, 'default'),
  })),

  {
    s('snippet1', fmt([[
      content1
    ]], {
      i(1, ''),
    })),

    s('snippet2', fmt([[
      content2
    ]], {
      i(1, ''),
    }))
  }
)
```

### エラー対処

#### よくあるエラー

1. **Missing key in format arguments**
   - fmt文字列と引数テーブルの不一致
   - 解決: 名前付きプレースホルダーの対応確認

2. **File not found**
   - requireパスの間違い
   - 解決: `snippets.(filetype).filename` 形式の確認

3. **Syntax error**
   - Lua構文エラー
   - 解決: `luac -p filename.lua` でチェック

4. **bad argument #1 to 'ipairs' (table expected, got string)**
   - `vim.tbl_extend`と`sm`関数の非互換性、または単純な文字列スニペットの誤用
   - 解決: `list.concat`を使用、または単純文字列は`t()`関数を使用

5. **nesting of [[...]] is deprecated**
   - `[[...]]`文字列内に`--[[`コメントがある
   - 解決: `[=[...]=]` や `[==[...]==]` 形式を使用

6. **スニペットが認識されているのに展開されない**
   - 複雑な`vim.tbl_extend`構造が原因
   - 解決: シンプルな配列構造または`list.concat`を使用

7. **Unused argument: args[name]**
   - 引数テーブルで定義されているのにテンプレート文字列で使われていない
   - 解決: 引数テーブルから不要な引数を削除、またはテンプレート文字列で使用
   - よくある原因: テンプレートリテラル内の`${{{value}}}`エスケープミス

#### デバッグ方法

```vim
:lua require'luasnip'.log.open()
```

### sm関数の実装

`lua/utils/luasnip.lua`:

```lua
local s = require('luasnip').snippet

local M = {}

---Creates a snippet with multiple triggers (aliases)
---@param triggers string[]
---@param snip table --A result of `require('luasnip.extras.fmt').fmt()`
---@param opts? table --?
---@return unknown[] --A list of `require('luasnip').snippet()`
---
---Example:
---```lua:snippets/typescript.lua
---return vim.tbl_extend(
---  'force',
---  {},
---  sm({'fun', 'function'}, fmt([[
---    function {name}({args}) {{
---      {}
---    }}
---  ]], {
---    name = i(1, 'name'),
---    args = i(2, 'args'),
---    i(3, ''),
---  })),
---  {}
---)
---```
function M.snip_by_multiple_triggers(triggers, snip, opts)
  return vim.iter(ipairs(triggers))
    :map(function(_, trigger)
      -- Why deepcopy? See: https://www.reddit.com/r/neovim/comments/tzd135/regex_pattern_or_for_luasnip_lua_pattern_matching
      -- Non functional programming is really XXXXXX!
      return vim.deepcopy(s(trigger, snip, opts))
    end)
    :totable()
end

M.sm = M.snip_by_multiple_triggers

return M
```
