local pipe = require('utils.pipe')

describe('utils.pipe', function()
  describe('get()', function()
    it('should return the inside value simply', function()
      local result = pipe(10):get()
      assert.are.equal(10, result)
    end)
  end)

  describe('apply()', function()
    it('should apply a function to the inside value', function()
      local result = pipe('nayu'):apply(string.upper):get()
      assert.are.equal('NAYU', result)
    end)

    it('should handle occurred error if application failed', function()
      local result = pipe(10):apply(function()
        error('error')
      end, function()
        return -1
      end)
      assert.are.equal(-1, result)
    end)

    it('should be error if application failed and handle is nil', function()
      local ok, _ = pcall(function()
        pipe(10):apply(function()
          error('error')
        end)
      end)
      assert.is_false(ok)
    end)
  end)

  describe('apply_if()', function()
    it('should apply function when predicate is true', function()
      local result = pipe(10)
        :apply_if(function(x)
          return x > 5
        end, function(x)
          return x * 2
        end)
        :get()
      assert.are.equal(20, result)
    end)

    it('should not apply function when predicate is false', function()
      local result = pipe(3)
        :apply_if(function(x)
          return x > 5
        end, function(x)
          return x * 2
        end)
        :get()
      assert.are.equal(3, result)
    end)

    it('should handle error with handle function', function()
      local result = pipe(10):apply_if(function(x)
        return x > 5
      end, function()
        error('error')
      end, function()
        return -1
      end)
      assert.are.equal(-1, result)
    end)
  end)

  describe('apply_conditional()', function()
    it('should apply if_true when predicate is true', function()
      local result = pipe(10)
        :apply_conditional(function(x)
          return x > 5
        end, function(x)
          return x * 2
        end, function(x)
          return x + 100
        end)
        :get()
      assert.are.equal(20, result)
    end)

    it('should apply if_false when predicate is false', function()
      local result = pipe(3)
        :apply_conditional(function(x)
          return x > 5
        end, function(x)
          return x * 2
        end, function(x)
          return x + 100
        end)
        :get()
      assert.are.equal(103, result)
    end)
  end)

  describe('apply_if_not_nil()', function()
    it('should apply function when value is not nil', function()
      local result = pipe('nayu'):apply_if_not_nil(string.upper):get()
      assert.are.equal('NAYU', result)
    end)

    it('should not apply function when value is nil', function()
      local result = pipe('nayu')
        :apply(function()
          return nil
        end)
        :apply_if_not_nil(string.upper)
        :get()
      assert.is_nil(result)
    end)

    it('should handle error with handle function', function()
      local result = pipe('nayu'):apply_if_not_nil(function()
        error('error')
      end, function()
        return 'handled'
      end)
      assert.are.equal('handled', result)
    end)
  end)

  describe('apply_method()', function()
    it('should call method of self.value', function()
      local result = pipe({
          number = 10,
          get_number = function(self)
            return self.number
          end,
        })
        :apply_method('get_number')
        :get()
      assert.are.equal(10, result)
    end)
  end)

  describe('apply_method_if_not_nil()', function()
    it('should call method when value is not nil', function()
      local result = pipe({
          number = 10,
          get_double = function(self)
            return self.number * 2
          end,
        })
        :apply_method_if_not_nil('get_double')
        :get()
      assert.are.equal(20, result)
    end)

    it('should not apply function when value is nil', function()
      local result = pipe({
          number = 10,
          get_double = function(self)
            return self.number * 2
          end,
        })
        :apply(function()
          return nil
        end)
        :apply_method_if_not_nil('get_double')
        :get()
      assert.is_nil(result)
    end)
  end)
end)
