---Enables packages installed by `luarocks install --local`

-- 環境によってlua5.1だったりlua5.2だったりするので、全部追加しておく
package.path = ('%s;%s/.luarocks/share/lua/5.1/?.lua'):format(
  package.path,
  vim.env.HOME
)
package.path = ('%s;%s/.luarocks/share/lua/5.2/?.lua'):format(
  package.path,
  vim.env.HOME
)
