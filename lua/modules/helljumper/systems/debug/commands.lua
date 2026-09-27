-- Lua libraries
local balltze = Balltze
local luna = require "luna"
local input = require "helljumper.systems.core.input"
local performanceMeter = require "helljumper.systems.debug.debugPerformanceMeter"

--- Console commands. Balltze puts the plugin's name in front of each one, so `debug` is typed
--- `helljumper_multiplayer_debug`.
local commands = {}

--- An optional boolean argument; nil means the caller left it out, since luna.bool refuses nil.
---@param value string|nil
---@return boolean|nil
local function optionalBool(value)
    if value == nil then
        return nil
    end
    return luna.bool(value)
end

---@class HelljumperCommand
---@field description string what the command does, printed by Balltze's help
---@field help string its parameters
---@field example string
---@field minArgs integer
---@field maxArgs integer
---@field save boolean|nil persist the last successful arguments in settings.json and replay them on load
---@field func fun(...: string)

---@type table<string, HelljumperCommand>
commands.all = {
    debug = {
        description = "Turn Helljumper's debug logging on or off.",
        help = "<enabled>",
        example = "helljumper_multiplayer_debug true",
        minArgs = 1,
        maxArgs = 1,
        save = true,
        -- Read here rather than cached at load: a module that caches this global never sees it move.
        func = function(enabled)
            DebugMode = luna.bool(enabled)
            balltze.logger.muteDebug(not DebugMode)
            balltze.logger.info("debug mode {}", DebugMode and "on" or "off")
        end
    },
    performance = {
        description = "Turn the performance meter on or off, and whether it writes logs/performance.log.",
        help = "<enabled> [<logFile>]",
        example = "helljumper_multiplayer_performance true false",
        minArgs = 1,
        maxArgs = 2,
        save = true,
        func = function(enabled, logFile)
            DebugPerformance = luna.bool(enabled)
            performanceMeter.setEnabled(DebugPerformance)
            -- Left out, the file keeps whatever it was doing; the meter being off already stops it.
            local toFile = optionalBool(logFile)
            if toFile ~= nil then
                performanceMeter.setFileLogging(toFile)
            end
        end
    },
    inputs = {
        description = "Print the code of every key, mouse button and pad button pressed, to bind it in settings.json.",
        help = "<enabled>",
        example = "helljumper_multiplayer_inputs true",
        minArgs = 1,
        maxArgs = 1,
        -- Not saved: it is a lookup tool, and one left on floods the log on the next session.
        func = function(enabled)
            local isEnabled = luna.bool(enabled)
            input.logInputs(isEnabled)
            balltze.logger.info("input logging {}", isEnabled and "on" or "off")
        end
    }
}

--- Give Balltze every command, then let it replay the arguments it kept for the ones that ask to be
--- saved.
function commands.register()
    for name, command in pairs(commands.all) do
        balltze.registerCommand(name, command.description, command.help, command.save or false,
                                command.minArgs, command.maxArgs, true, true, function(args)
            args = args or {}
            if #args < command.minArgs or #args > command.maxArgs then
                balltze.logger.warning("{}: wrong number of arguments. Usage: {} | Example: {}", name,
                                       command.help, command.example)
                return true
            end

            local ok, err = pcall(command.func, table.unpack(args))
            if not ok then
                balltze.logger.error("{} failed: {}", name, tostring(err))
            end
            return true
        end)
    end

    -- Replays only the commands registered with save = true, and does nothing while none are.
    balltze.loadSettings()
end

return commands
