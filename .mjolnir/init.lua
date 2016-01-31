local os = require 'os'

-- https://github.com/sdegutis/mjolnir/blob/master/README.md#faq
local luarocks = os.getenv('HOME') .. '/.luarocks'
package.path = package.path .. ';' .. luarocks .. '/share/lua/5.2/?.lua'
package.cpath = package.cpath .. ';' .. luarocks .. '/lib/lua/5.2/?.so'

local hotkey = require 'mjolnir.hotkey' -- need to be installed
-- local fnutils = require 'mjolnir.fnutils'

-- https://github.com/nathankot/mjolnir.tiling
local tiling = require 'mjolnir.tiling' -- need to be installed
local mash = {'ctrl', 'cmd'}

hotkey.bind(mash, 'c', function() tiling.cyclelayout() end)
hotkey.bind(mash, 'j', function() tiling.cycle(1) end)
hotkey.bind(mash, 'k', function() tiling.cycle(-1) end)
hotkey.bind(mash, 'space', function() tiling.promote() end)
-- hotkey.bind(mash, 'f', function() tiling.gotolayout('fullscreen') end)

-- If you want to set the layouts that are enabled
tiling.set('layouts', {
  'fullscreen', 'main-vertical'
})

-- https://github.com/thomasjachmann/dotfiles/tree/master/mjolnir/.mjolnir
local application = require 'mjolnir.application'
local appfinder = require "mjolnir.cmsj.appfinder"
local window = require 'mjolnir.window'

function launch_app(app, fn)
  return function()
    launchedapp = appfinder.app_from_name(app)
    if (launchedapp) then
      windows = launchedapp:visiblewindows()
      if (#windows > 1) then
        focused = window.focusedwindow()
        for _, window in pairs(windows) do
          if (window == focused) then
            windows[#windows]:focus()
            if (fn) then; fn(); end
            return
          end
        end
      end
    end
    application.launchorfocus(app)
    if (fn) then; fn(); end
  end
end

hotkey.bind(mash, 'i', launch_app('iterm'))
hotkey.bind(mash, 'f', launch_app('FirefoxDeveloperEdition'))
