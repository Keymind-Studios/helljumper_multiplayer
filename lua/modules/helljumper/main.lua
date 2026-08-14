local script = require "script"
local multiplayer = require "helljumper.multiplayer"

local map = {}

script.cleanup()

function map.main()
    multiplayer.load()
    Balltze.logger.info("Welcome to Helljumper Multiplayer!")
end
script.startup(map.main)

function map.loop()
    multiplayer.gameplaySystems()
end
script.continuous(map.loop)

function map.frame()
    multiplayer.frameSystems()
end

function map.unload()
    multiplayer.unload()
end

return map