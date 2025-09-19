local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  s('template_electron_main_js', fmt([[
    'use strict';

    var app           = require('app');
    var BrowserWindow = require('browser-window');

    require('crash-reporter').start({{
        companyName: '{company}',
        submitURL:   '{url}'
    }});

    var mainWindow = null;

    app.on('window-all-closed', function () {{
        if (process.platform != 'darwin')
            app.quit();
    }});

    app.on('ready', function () {{
        mainWindow = new BrowserWindow({{width: {width}, height: {height}}});
        mainWindow.loadURL('file://' + __dirname + '/index.html');
        mainWindow.on('closed', function () {{
            mainWindow = null;
        }});
    }});
  ]], {
    company = i(1, 'aiya000'),
    url = i(2, 'dummy'),
    width = i(3, '800'),
    height = i(4, '600'),
  })),

  s('define_prototype_Array#map', fmt([[
    Array.prototype.map = function (f) {{
        for (var i = 0; i < this.length; ++i) {{
            this[i] = f(i);
        }}
        return this;
    }};
  ]], {})),
}