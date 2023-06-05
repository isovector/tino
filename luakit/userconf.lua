require "adblock"
require "adblock_chrome"
require "go_next_prev"

--------------------------------------------------------------------------------

local debug = require "msg"
_G.print = debug.warn

--------------------------------------------------------------------------------

local follow  = require "follow"
follow.pattern_maker = follow.pattern_styles.match_label

local select = require "select"
select.label_maker = function ()
    local chars = charset("asdfqwerzxcv")
    return trim(sort(reverse(chars)))
end

--------------------------------------------------------------------------------

luakit.enable_spell_checking = true

local modes = require "modes"
modes.remove_binds("normal", { "y" })
modes.remap_binds("normal", { { "x", "ZZ", false }, { "y", "Y", false } })

--------------------------------------------------------------------------------

local window = require("window")
local new_tab = window.methods.new_tab
window.methods.new_tab = function(w, arg, opts)
  if w.tabs:count() == 0 then
    new_tab(w, arg, opts)
  else
    window.new({arg})
  end
end

window.methods.save_session = function() end

--------------------------------------------------------------------------------

local webview = require "webview"
webview.add_signal("init", function(view)
    view:add_signal("navigation-request", function (v, uri)
        if string.match(uri, "twitter.com") then
            v.uri = uri:gsub("twitter.com","nitter.net")
            return false
        end
    end)
end)

local theme = require "theme"
theme.font = "8px monospace"

--------------------------------------------------------------------------------

local style_override = stylesheet{
  source = [===[
    p, li, div {
      max-width: 30em;
    }]===]}

modes.add_binds("normal", {
 {"s", "Toggle user-defined stylesheets",
    function (w)
      local s = w.view.stylesheets;
      if s[style_override] then
        s[style_override] = nil
      else
        s[style_override] = true
      end
    end
  },
})

--------------------------------------------------------------------------------

local settings = require "settings"
local engines = settings.window.search_engines
engines.m = "https://search.marginalia.nu/search?query=%s&profile=default&js=default"

--------------------------------------------------------------------------------

require "lpass"

