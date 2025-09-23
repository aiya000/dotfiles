-- Enable packages installed by `luarocks install --local`
package.path = package.path .. ';' .. os.getenv('HOME') .. '/.luarocks/share/lua/5.2/?.lua'
package.path = package.path .. ';' .. os.getenv('HOME') .. '/.luarocks/share/lua/5.2/?/init.lua'
