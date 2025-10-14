local List = require('utils.list')

describe('utils.list', function()
  describe('char_range()', function()
    it('should generates the char array correctly', function()
      assert.are.same({
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      }, List.char_range('a', 'z'))
    end)

    it('should handle single character range', function()
      assert.are.same({ 'x' }, List.char_range('x', 'x'))
    end)

    it('should handle numbers', function()
      assert.are.same({ '0', '1', '2', '3', '4', '5' }, List.char_range('0', '5'))
    end)
  end)

  describe('equal()', function()
    it('should compare arrays correctly', function()
      -- 同一参照
      local arr = { 1, 2, 3 }
      assert.is_true(List.equal(arr, arr))

      -- 同じ内容
      assert.is_true(List.equal({ 1, 2, 3 }, { 1, 2, 3 }))
      assert.is_true(List.equal({ 'a', 'b' }, { 'a', 'b' }))
      assert.is_true(List.equal({}, {}))

      -- 異なる内容
      assert.is_false(List.equal({ 1, 2, 3 }, { 1, 2, 4 }))
      assert.is_false(List.equal({ 1, 2 }, { 1, 2, 3 }))
      assert.is_false(List.equal({ 1, 2, 3 }, { 1, 2 }))
    end)

    it('should handle nested arrays', function()
      assert.is_true(List.equal({ { 1, 2 }, { 3, 4 } }, { { 1, 2 }, { 3, 4 } }))
      assert.is_false(List.equal({ { 1, 2 }, { 3, 4 } }, { { 1, 2 }, { 3, 5 } }))
      assert.is_true(List.equal({ 1, { 2, 3 }, 4 }, { 1, { 2, 3 }, 4 }))
    end)
  end)

  describe('index_of()', function()
    it('should find correct index', function()
      assert.are.equal(2, List.index_of({ 1, 2, 3 }, 2))
      assert.are.equal(3, List.index_of({ 'a', 'b', 'c' }, 'c'))
      assert.are.equal(1, List.index_of({ 1, 2, 3 }, 1))
      assert.is_nil(List.index_of({}, 1))
      assert.is_nil(List.index_of({ 1, 2, 3 }, 4))
    end)

    it('should return first occurrence', function()
      assert.are.equal(2, List.index_of({ 1, 2, 2, 3 }, 2))
      assert.are.equal(1, List.index_of({ 'a', 'b', 'a' }, 'a'))
    end)
  end)

  describe('has()', function()
    it('should check if value exists', function()
      assert.is_true(List.has({ 1, 2, 3 }, 2))
      assert.is_true(List.has({ 'a', 'b', 'c' }, 'b'))
      assert.is_false(List.has({ 1, 2, 3 }, 4))
      assert.is_false(List.has({}, 1))
    end)

    it('cannot find values after nil due to ipairs() limitation', function()
      assert.is_false(List.has({ 1, nil, 2 }, 2))
    end)
  end)

  describe('slice()', function()
    it('should take a sub array from the taken array', function()
      assert.are.same({ 2, 3, 4 }, List.slice({ 1, 2, 3, 4, 5 }, 2, 4))
    end)
  end)

  describe('concat()', function()
    it('should concatenate multiple arrays', function()
      assert.are.same({ 1, 2, 3, 4, 5 }, List.concat({ 1, 2 }, { 3, 4 }, { 5 }))
      assert.are.same({ 'a', 'b', 'c' }, List.concat({}, { 'a' }, { 'b', 'c' }))
    end)
  end)

  describe('append()', function()
    it('should append an element to the array', function()
      assert.are.same({ 1, 2, 3, 4 }, List.append({ 1, 2, 3 }, 4))
      assert.are.same({ 'a' }, List.append({}, 'a'))
    end)

    it('should not mutate the original array', function()
      local original = { 1, 2, 3 }
      List.append(original, 4)
      assert.are.same({ 1, 2, 3 }, original)
    end)
  end)

  describe('format()', function()
    it('should replace markers with provided values', function()
      assert.are.same({ 1, 2, 3 }, List.format({ 1, List.s(), 3 }, 2)) -- Simple
      assert.are.same({ 'a', 'b', 'c' }, List.format({ List.s(), List.s(), List.s() }, 'a', 'b', 'c')) -- All replaced
      assert.are.same({ 1, 2, 3 }, List.format({ 1, 2, 3 }, 4)) -- No replacement
    end)

    it('should replace multi markers with provided lists', function()
      assert.are.same({ 1, 2, 3, 4 }, List.format({ 1, List.ss(), 4 }, { 2, 3 })) -- Simple
      assert.are.same({ 'a', 'b', 'c', 'd' }, List.format({ List.ss(), List.ss() }, { 'a', 'b' }, { 'c', 'd' })) -- All replaced
      assert.are.same({ 1, 2, 3 }, List.format({ 1, 2, 3 }, { 4, 5 })) -- No replacement
    end)
  end)
end)
