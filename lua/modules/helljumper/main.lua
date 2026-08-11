local script = require "script"
--local sleep = script.sleep
--local engine = Engine
local multiplayer = require "helljumper.multiplayer"

local map = {}

function map.main()
    multiplayer.load()
    Balltze.logger.info("Welcome to Helljumper Multiplayer!")
end
script.startup(map.main)

function map.loop()
    multiplayer.gameplaySystems()
end
script.continuous(map.loop)

return map