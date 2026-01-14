local fn = require('utils.functions')

describe('utils.functions', function()
  describe('set_vim_dict_field()', function()
    it('should set a sub field', function()
      local dict = {
        field = {},
      }
      fn.set_vim_dict_field(dict, 'field', 'sub_field', 10)
      assert.are.equal(10, dict.field.sub_field)
    end)

    it('should keep another field values', function()
      local dict = {
        another = 10,
        field = {},
      }
      fn.set_vim_dict_field(dict, 'field', 'sub_field', 10)
      assert.are.equal(10, dict.another)
    end)
  end)

  describe('to_pretty_string()', function()
    it('should convert primitives to string', function()
      assert.are.equal('42', fn.to_pretty_string(42))
      assert.are.equal('hello', fn.to_pretty_string('hello'))
      assert.are.equal('true', fn.to_pretty_string(true))
      assert.are.equal('false', fn.to_pretty_string(false))
      assert.are.equal('nil', fn.to_pretty_string(nil))
    end)

    it('should convert simple table to string', function()
      local result = fn.to_pretty_string({ name = 'John', age = 30 })
      -- テーブルの順序は保証されないので、内容をチェック
      assert.is_true(result:find("'name' = 'John'") ~= nil, 'Should contain name field with quotes')
      assert.is_true(result:find("'age' = 30") ~= nil, 'Should contain age field')
      assert.is_true(result:find('^{') ~= nil, 'Should start with {')
      assert.is_true(result:find('}$') ~= nil, 'Should end with }')
    end)

    it('should handle nested tables', function()
      local nested = {
        user = { name = 'Alice' },
        active = true,
      }
      local result = fn.to_pretty_string(nested)
      assert.is_true(result:find("'user' = {") ~= nil, 'Should contain nested table')
      assert.is_true(result:find("'name' = 'Alice'") ~= nil, 'Should contain nested value with quotes')
      assert.is_true(result:find("'active' = true") ~= nil, 'Should contain top-level value')
    end)

    it('should handle empty table', function()
      local result = fn.to_pretty_string({})
      assert.are.equal('{\n}', result)
    end)
  end)

  describe('make_table_to_string()', function()
    it('should work directly', function()
      local result = fn.make_table_to_string({ key = 'value' })
      assert.are.equal("{\n 'key' = 'value',\n}", result)
    end)

    it('should handle numeric keys', function()
      local result = fn.make_table_to_string({ [1] = 'first', [2] = 'second' })
      assert.is_true(result:find("1 = 'first'") ~= nil or result:find("2 = 'second'") ~= nil, 'Should handle numeric keys with quoted strings')
    end)
  end)
end)
