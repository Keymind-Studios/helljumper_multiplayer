package.preload["luna"] = nil
package.loaded["luna"] = nil
require "luna"
require "chimeraCompat"()
local balltze = Balltze
local engine = Engine
local blam = require "blam2"
local script = require "script"
local performance

DebugMode = true
DebugLuaMemory = true
DebugPerformance = false

if DebugMode then
    -- Registers the frame listeners that draw the Lua memory / profiler overlays.
    performance = require "performance"
end

local commands = require "helljumper.systems.debug.commands"

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

local isSapp = engine.game.getGameConnectionType() == "networkServer" and
                   type(balltze.registerSappCallbacks) == "function"

if not isSapp then
    if balltze.chimera then
        require "chimeraCompat"()
    end
end

balltze.addEventListener("tick", function()
    local tickStart
    if DebugPerformance then
        tickStart = os.clock()
    end
    script.poll()
    if DebugPerformance then
        performance.tick(os.clock() - tickStart)
    end
end)

if not isSapp then
    -- Commands for Alpha Firefight
    for command, data in pairs(commands) do
        -- local command = command:replace("debug_", "")
        balltze.registerCommand(command, data.description, data.help, data.save or false,
                                data.minArgs or 0, data.maxArgs or 0, false, true, function(args)
            -- Balltze.logger.debug("{}", inspect(args))
            if (args and data.minArgs and data.maxArgs) and (#args < data.minArgs) or
                (#args > data.maxArgs) then
                balltze.logger.error("Invalid number of arguments. Usage: {}, Example: {}",
                                     data.help, data.example)
                return true
            end
            -- data.func(table.unpack(args or {}))
            local ok, message = pcall(data.func, table.unpack(args or {}))
            if not ok then
                balltze.logger.error("Error executing command \"{}\": {}", command, message)
            end
            return true
        end)
    end
    balltze.loadSettings()
end

function PluginOnSappLoad()
    if isSapp then
        -- Register all SAPP callbacks now that all subscribers are in place
        balltze.registerSappCallbacks()
        blam.rcon.patch()
    end
end

function PluginUnload()
    balltze.logger.info("Unloading Helljumper Plugin")
end

function OnError(message)
    print(message)
    print(debug.traceback())
end

function PluginOnGameStart()
    script.setReferenceContext(require "helljumper.main")
end