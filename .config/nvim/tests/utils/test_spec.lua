local Test = require('utils.test')

describe('utils.test', function()
  describe('deep_equal()', function()
    it('should compare primitive values correctly', function()
      assert.is_true(Test.deep_equal(1, 1))
      assert.is_false(Test.deep_equal(1, 2))

      assert.is_true(Test.deep_equal('hello', 'hello'))
      assert.is_false(Test.deep_equal('hello', 'world'))

      assert.is_true(Test.deep_equal(true, true))
      assert.is_false(Test.deep_equal(true, false))

      assert.is_true(Test.deep_equal(nil, nil))
      assert.is_false(Test.deep_equal(1, nil))
      assert.is_false(Test.deep_equal(nil, 2))
    end)

    it('should compare tables correctly', function()
      assert.is_true(Test.deep_equal({ a = 1 }, { a = 1 }))
      assert.is_false(Test.deep_equal({ a = 1 }, { a = 2 }))
      assert.is_true(Test.deep_equal({ a = 1, b = { c = 2 } }, { a = 1, b = { c = 2 } }))
    end)

    it('should compare arrays correctly', function()
      assert.is_true(Test.deep_equal({ 1, 2, 3 }, { 1, 2, 3 }))
      assert.is_false(Test.deep_equal({ 1, 2, 3 }, { 10, 20, 30 }))
    end)
  end)

  describe('assert_equal()', function()
    it('should pass when values are equal', function()
      assert.is_true(Test.assert_equal(1 + 1, 2))
    end)

    it('should throw when values are not equal', function()
      assert.has_error(function()
        Test.assert_equal(1 + 1, 3)
      end)
    end)
  end)

  describe('assert_to_throw()', function()
    it('should pass when function throws', function()
      local function to_throw()
        error('error')
      end
      assert.is_true(Test.assert_to_throw(to_throw))
    end)

    it('should throw when function does not throw', function()
      local function not_to_throw()
        return 10
      end
      assert.has_error(function()
        Test.assert_to_throw(not_to_throw)
      end)
    end)
  end)

  describe('reduce()', function()
    it('should reduce an array to a single value', function()
      local sum = Test.reduce({ 1, 2, 3, 4 }, function(acc, item)
        return acc + item
      end, 0)
      assert.are.equal(10, sum)

      local str = Test.reduce({ 'a', 'b', 'c' }, function(acc, item)
        return acc .. item
      end, '')
      assert.are.equal('abc', str)
    end)

    it('should return the initial value if the empty array is taken', function()
      local result_ = Test.reduce({}, function(acc, item)
        error('error')
      end, 'result')
      assert.are.equal('result', result_)
    end)
  end)

  describe('concat_array_including_nil()', function()
    it('should concat an array including nil', function()
      assert.are.equal('1, nil, a', Test.concat_array_including_nil({ 1, ', ', nil, ', ', 'a' }))
      assert.are.equal('', Test.concat_array_including_nil({}))
    end)
  end)

  describe('to_element_string()', function()
    it("should make values to the string of an array's element", function()
      assert.are.equal("'hello'", Test.to_element_string('hello'))
      assert.are.equal('10', Test.to_element_string(10))
      assert.are.equal('true', Test.to_element_string(true))
      assert.are.equal('nil', Test.to_element_string(nil))
    end)
  end)

  describe('make_array_to_string()', function()
    it('should make array to string', function()
      assert.are.equal('{ 1, 2, 3 }', Test.make_array_to_string({ 1, 2, 3 }))
      assert.are.equal('{ 1, nil, 3 }', Test.make_array_to_string({ 1, nil, 3 }))
      assert.are.equal("{ 1, 'a', true }", Test.make_array_to_string({ 1, 'a', true }))
      assert.are.equal('{  }', Test.make_array_to_string({}))
    end)

    it("cannot include trailing nil because Lua's specification", function()
      assert.are.equal('{ 1, 2 }', Test.make_array_to_string({ 1, 2, nil }))
    end)
  end)

  describe('is_array()', function()
    it('should check if a table is an array', function()
      assert.is_true(Test.is_array({ 1, 2, 3 }))
      assert.is_true(Test.is_array({ 1, 'a', true, nil }))
      assert.is_true(Test.is_array({}))
      assert.is_false(Test.is_array({ a = 1, b = 2 }))

      -- Below should be type error
      assert.is_false(Test.is_array(10))
      assert.is_false(Test.is_array(nil))
    end)
  end)

  describe('to_pretty_string()', function()
    it('should convert to a pretty string', function()
      assert.are.equal('{ 1, 2, 3 }', Test.to_pretty_string({ 1, 2, 3 }))
      assert.are.equal("{ 'a', 'b', 'c' }", Test.to_pretty_string({ 'a', 'b', 'c' }))
      assert.are.equal('10', Test.to_pretty_string(10))
    end)
  end)
end)
