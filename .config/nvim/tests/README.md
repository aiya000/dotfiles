# Tests

このディレクトリには、plenary.nvimのbustedテストフレームワークを使用したテストが含まれています。

## 移行されたファイル

以下のファイルのin-source testingをplenary.nvimのbustedテストに移行しました：

- `lua/utils/test.lua` → `tests/utils/test_spec.lua`
- `lua/utils/list.lua` → `tests/utils/list_spec.lua`
- `lua/utils/functions.lua` → `tests/utils/functions_spec.lua`
- `lua/utils/functions/s.lua` → `tests/utils/functions/s_spec.lua`
- `lua/utils/pipe.lua` → `tests/utils/pipe_spec.lua`

## テストの実行方法

### すべてのテストを実行

```bash
cd ~/.config/nvim
nvim --headless -c "PlenaryBustedDirectory tests/ { minimal_init = 'tests/minimal_init.lua' }" -c "qa"
```

または、提供されているスクリプトを使用:

```bash
cd ~/.config/nvim
./run_tests.sh
```

### 特定のテストファイルを実行

```bash
cd ~/.config/nvim
nvim --headless -c "PlenaryBustedFile tests/utils/test_spec.lua { minimal_init = 'tests/minimal_init.lua' }" -c "qa"
```

## テストの書き方

plenary.nvimのbustedテストフレームワークを使用します:

```lua
local MyModule = require('my_module')

describe('my_module', function()
  describe('my_function()', function()
    it('should do something', function()
      assert.are.equal(expected, MyModule.my_function())
    end)
    
    it('should handle edge case', function()
      assert.is_true(MyModule.my_function(edge_case))
    end)
  end)
end)
```

### アサーション

- `assert.are.equal(expected, actual)` - 値の等価性チェック
- `assert.are.same(expected, actual)` - テーブルの深い等価性チェック
- `assert.is_true(value)` - 真偽値チェック
- `assert.is_false(value)` - 偽値チェック
- `assert.is_nil(value)` - nil チェック
- `assert.has_error(function)` - エラーが発生することをチェック

## 移行について

以前は `if vim == nil then` ブロックを使用したin-sourceテストを使用していましたが、
plenary.nvimのbustedテストフレームワークに移行しました。

### 移行理由

1. vim.iterなどのNeovim固有のAPIを使用している関数のテストが可能
2. テストコードとプロダクションコードの分離
3. より標準的なテストフレームワーク
4. より良いテストレポート

### neosnippetテンプレートの更新

新しいbusted形式のテストを書くためのsnippetを追加しました：

- `busted_test` / `plenary_test` - 新しいbustedテストの骨組み
- `busted_describe` / `describe` - describeブロック
- `busted_it` / `it` - itブロック
- `deftest` - 従来の`test()`から`it()`に更新

従来の`in_source_test`スニペットは非推奨としてマークされていますが、後方互換性のために残されています。
