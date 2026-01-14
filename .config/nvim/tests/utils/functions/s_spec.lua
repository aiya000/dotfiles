local s = require('utils.functions.s')

describe('utils.functions.s', function()
  describe('s()', function()
    it('should return the taken string simply if with no embedded expressions', function()
      assert.are.equal('hi', s.s('hi', {}))
      assert.are.equal('hello world', s.s('hello world', {}))
    end)

    it('should embed variables', function()
      assert.are.equal('Hello Konoko', s.s('Hello {name}', { name = 'Konoko' }))
    end)

    it('should embed values', function()
      assert.are.equal('10', s.s('{10}', {}))
      assert.are.equal('8', s.s('{5 + 3}', {}))
      assert.are.equal('1.25', s.s('{1.25}', {}))
      assert.are.equal('hello', s.s('{"hello"}', {}))
      assert.are.equal('nil', s.s('{nil}', {}))
    end)

    it('should embed function and function call', function()
      local function f(x)
        return x
      end
      assert.are.equal('10', s.s('{f(10)}', { f = f }))
    end)

    it('should be error if unknown variable is embedded', function()
      assert.has_error(function()
        s.s('{nonexistent_var}', {})
      end)
    end)

    it('should use variable table values', function()
      assert.are.equal('Konoko', s.s('{name}', { name = 'Konoko' }))
    end)

    it('should work in nested contexts', function()
      local function level1()
        local function level2()
          return s.s('{x}', { x = 10 })
        end
        return level2()
      end
      assert.are.equal('10', level1())
    end)
  end)
end)
