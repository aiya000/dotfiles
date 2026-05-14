local targetAppNames = {
  'ovice',
}

local targetFilters = {}

local function raiseTargetWindows()
  for _, targetAppName in ipairs(targetAppNames) do
    local app = hs.application.get(targetAppName)
    if app then
      local win = app:mainWindow()
      if win and win:isVisible() and not win:isMinimized() then
        win:raise()
      end
    end
  end
end

for _, targetAppName in ipairs(targetAppNames) do
  local targetFilter = hs.window.filter.new(false):setAppFilter(targetAppName)
  table.insert(targetFilters, targetFilter)
  targetFilter:subscribe(hs.window.filter.windowCreated, raiseTargetWindows)
  targetFilter:subscribe(hs.window.filter.windowUnminimized, raiseTargetWindows)
end

hs.window.filter.default:subscribe(hs.window.filter.windowFocused, function(win, appName)
  for _, targetAppName in ipairs(targetAppNames) do
    if appName == targetAppName then
      return
    end
  end

  hs.timer.doAfter(0.05, raiseTargetWindows)
end)

hs.hotkey.bind({ 'ctrl', 'alt', 'cmd' }, 'P', raiseTargetWindows)
