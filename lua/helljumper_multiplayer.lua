package.preload["luna"] = nil
package.loaded["luna"] = nil
require "luna"
local balltze = Balltze
local engine = Engine
local script = require "script"
-- Declared up here rather than beside PluginUnload because the frame listener below closes over it:
-- a `local main` further down would leave that closure reading a global that nothing ever assigns.
local main

DebugMode = false
DebugLuaMemory = true
DebugPerformance = false

local commands = require "helljumper.systems.debug.commands"
-- Required here rather than from helljumper.main because the meter is meant to be counting from the
-- moment the plugin loads: what it was written to catch is a cost in a map's first seconds, which is
-- over before PluginOnGameStart hands main over. Its own flags decide whether anything comes out.
local performanceMeter = require "helljumper.systems.debug.debugPerformanceMeter"

-- Override assert function to print traceback as well
local luaAssert = assert
function assert(...)
    local args = {...}
    local condition = args[1]
    local message = args[2]
    if not condition then
        if message then
            balltze.logger.error(message)
        end
        local err = debug.traceback(message or "Assertion failed!", 2)
        err = err .. "\n--------- ASSERT STACKTRACE ---------"
        luaAssert(condition, err)
    end
end

balltze.logger.muteDebug(not DebugMode)

HelljumperTickListener = balltze.addEventListener("tick", function()
    -- First thing on the tick, before anything it measures runs: what a system costs on this tick
    -- then lands in the window that is opening rather than in the one just reported.
    performanceMeter.tick()
    script.poll()
end)

-- The plugin's one frame listener, the counterpart of the tick one above.
--
-- Balltze keeps a single listener per event name: subscribing again replaces what was there rather
-- than joining a list, and the winner is whoever registered last. Modules therefore do not subscribe
-- for themselves; they expose a function and are called from here, in a known order. Before this,
-- performance.lua's two subscriptions left only its profiler running, and the ADS module's took even
-- that down as soon as the first tick reached it.
--
-- main is guarded because it is not always there: it only exists from PluginOnGameStart onward, and
-- this listener is up from the moment the plugin loads.
HelljumperFrameListener = balltze.addEventListener("frame", function()
    performanceMeter.frame()
    if main then
        main.frame()
    end
end)

function PluginUnload()
    balltze.logger.info("Unloading Helljumper Plugin")
    performanceMeter.unload()
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

function PluginOnGameStart()
    main = require "helljumper.main"
    script.setReferenceContext(main)
end