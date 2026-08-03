package.preload["luna"] = nil
package.loaded["luna"] = nil
require "luna"
require "chimeraCompat"()
require "balltzeCompat"
local balltze = Balltze
local engine = Engine

DebugMode = true
DebugLuaMemory = true
DebugPerformance = false

if DebugMode then
    -- Registers the frame listeners that draw the Lua memory / profiler overlays.
    require "performance"
end

local weapons = require "helljumper.systems.constants.weapons"
local sounds = require "helljumper.systems.constants.sounds"

balltze.logger.muteDebug(not DebugMode)

local main

function PluginOnGameStart()
    weapons.get()
    sounds.get()
    main = require "helljumper.main"
end

function PluginUnload()
    -- Unload runs during map teardown, where the state main touches may already be gone.
    if main then
        local unloaded, err = pcall(main.unload)
        if not unloaded then
            balltze.logger.error("Failed to unload main: {}", err)
        end
        main = nil
    end
end

function OnError(message)
    print(message)
    print(debug.traceback())
end
