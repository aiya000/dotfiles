---Enables packages installed by `luarocks install --local`

local s = require('utils.functions').s

package.path = s('{path};{home}/.luarocks/share/lua/5.1/?.lua', {
  path = package.path,
  home = vim.env.HOME,
})

-- 一応Lua-5.2の環境用にも設定しておく
package.path = s('{path};{home}/.luarocks/share/lua/5.2/?.lua', {
  path = package.path,
  home = vim.env.HOME,
})
