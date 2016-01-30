local os = require 'os'

-- https://github.com/sdegutis/mjolnir/blob/master/README.md#faq
local luarocks = os.getenv('HOME') .. '/.luarocks'
package.path = package.path .. ';' .. luarocks .. '/share/lua/5.2/?.lua'
package.cpath = package.cpath .. ';' .. luarocks .. '/lib/lua/5.2/?.so'

local application = require 'mjolnir.application'
local hotkey = require 'mjolnir.hotkey' -- need to be installed
local window = require 'mjolnir.window'
local fnutils = require 'mjolnir.fnutils'

-- https://github.com/nathankot/mjolnir.tiling
local tiling = require 'mjolnir.tiling' -- need to be installed
local mash = {'ctrl', 'cmd'}

hotkey.bind(mash, 'c', function() tiling.cyclelayout() end)
hotkey.bind(mash, 'j', function() tiling.cycle(1) end)
hotkey.bind(mash, 'k', function() tiling.cycle(-1) end)
hotkey.bind(mash, 'space', function() tiling.promote() end)
hotkey.bind(mash, 'f', function() tiling.gotolayout('fullscreen') end)

-- If you want to set the layouts that are enabled
tiling.set('layouts', {
  'fullscreen', 'main-vertical'
})
