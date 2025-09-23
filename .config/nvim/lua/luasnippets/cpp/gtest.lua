local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

local gtest_snippets = {}

table.insert(
  gtest_snippets,
  s(
    'gtest_test_class',
    fmt(
      [[
class {} : public ::testing::Test {{
protected:
    {}() {{}}
    virtual ~{}() {{}}
    virtual void SetUp() {{}}
    virtual void TearDown() {{}}
public:
    static void SetUpTestCase() {{}}
    static void TearDownTestCase() {{}}
}}]],
      {
        i(1, 'TestName'),
        i(1, ''),
        i(1, ''),
      }
    )
  )
)

table.insert(
  gtest_snippets,
  s(
    'gtest_setup',
    fmt(
      [[
virtual void SetUp() {{
    {}
}}]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  gtest_snippets,
  s(
    'gtest_teardown',
    fmt(
      [[
virtual void TearDown() {{
    {}
}}]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  gtest_snippets,
  s(
    'gtest_setup_testcase',
    fmt(
      [[
static void SetUpTestCase() {{
    {}
}}]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  gtest_snippets,
  s(
    'gtest_teardown_testcase',
    fmt(
      [[
static void TearDownTestCase() {{
    {}
}}]],
      {
        i(1, ''),
      }
    )
  )
)

vim.list_extend(
  gtest_snippets,
  sm(
    { 'gtest_test_f', 'testf' },
    fmt(
      [[
TEST_F({}, {}) {{
    {}
}}]],
      {
        i(1, 'TestName'),
        i(2, 'TestCaseName'),
        i(3, ''),
      }
    )
  )
)

vim.list_extend(gtest_snippets, sm({ 'gtest_succeed', 'succeed' }, t('SUCCEED();')))

vim.list_extend(gtest_snippets, sm({ 'gtest_fail', 'fail' }, t('FAIL();')))

vim.list_extend(
  gtest_snippets,
  sm({ 'gtest_assert_true', 'assert_true' }, {
    t('ASSERT_TRUE('),
    i(1, 'value'),
    t(');'),
  })
)

vim.list_extend(
  gtest_snippets,
  sm({ 'gtest_assert_false', 'assert_false' }, {
    t('ASSERT_FALSE('),
    i(1, 'value'),
    t(');'),
  })
)

vim.list_extend(
  gtest_snippets,
  sm({ 'gtest_assert_eq', 'assert_eq' }, {
    t('ASSERT_EQ('),
    i(1, 'expected'),
    t(', '),
    i(2, 'actual'),
    t(');'),
  })
)

vim.list_extend(
  gtest_snippets,
  sm({ 'gtest_assert_ne', 'assert_ne' }, {
    t('ASSERT_NE('),
    i(1, 'expected'),
    t(', '),
    i(2, 'actual'),
    t(');'),
  })
)

vim.list_extend(
  gtest_snippets,
  sm({ 'gtest_assert_no_throw', 'assert_no_throw' }, {
    t('ASSERT_NO_THROW('),
    i(1, 'syntax'),
    t(');'),
  })
)

vim.list_extend(
  gtest_snippets,
  sm({ 'gtest_assert_any_throw', 'assert_any_throw' }, {
    t('ASSERT_ANY_THROW('),
    i(1, 'syntax'),
    t(');'),
  })
)

return {
  snippets = gtest_snippets,
  autosnippets = {},
}
