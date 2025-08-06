local balltze = Balltze
local engine = Engine
DebugMode = true
package.preload["luna"] = nil
package.loaded["luna"] = nil
require "luna"
local weapons = require "helljumper.systems.constants.weapons"
local sounds = require "helljumper.systems.constants.sounds"

local objectReferences = {}
-- Get an object of the current game (prevent memory leak by using this wrapper)
---@param handle EngineObjectHandle|integer Handle of the object
---@param objectType EngineTagObjectType? Type of the object
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeBiped): MetaEngineBipedObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeVehicle): MetaEngineVehicleObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeGarbage): MetaEngineGarbageObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeWeapon): MetaEngineWeaponObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeEquipment): MetaEngineEquipmentObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeProjectile): MetaEngineProjectileObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeDeviceMachine): MetaEngineDeviceMachineObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeDeviceControl): MetaEngineDeviceControlObject|nil
---@overload fun(handle: EngineObjectHandle|integer, type: EngineTagObjectTypeDeviceLightFixture): MetaEngineDeviceLightFixtureObject|nil
---@return MetaEngineBaseObject|nil
getObject = function(handle, objectType)
    local handleValue = handle
    if type(handleValue) == "table" or type(handleValue) == "userdata" then
        handleValue = handleValue.value
    end
    local ref = objectReferences[handleValue]
    if get_object(handleValue --[[@as number]] ) and ref then
        -- logger:info("Object ref cache hit!")
        return ref
    end
    -- logger:warning("Object cache miss! {}", handleValue)
    local object = Engine.gameState.getObject(handleValue, objectType)
    objectReferences[handleValue] = object
    return object
end

-- Local main
local loadWhenIn = {
    "treason",
    "bleed_it_out",
    "last_voyage",
    "impasse",
    "aqueduct",
    "penance",
    "behemoth",
    "warehouse"
}

loadWhenIn = table.extend(loadWhenIn, table.map(loadWhenIn, function(map)
    return map .. "_dev"
end))

function PluginMetadata()
    return {
        name = "Helljumper Multiplayer",
        author = "Keymind Dev Team",
        version = "1.0.0",
        targetApi = "1.0.0",
        reloadable = true,
        maps = loadWhenIn
    }
end

local function loadChimeraCompatibility()
    -- Load Chimera compatibility
    for k, v in pairs(balltze.chimera) do
        if not k:includes "timer" and not k:includes "execute_script" and
            not k:includes "set_callback" then
            _G[k] = v
        end
    end
    server_type = engine.netgame.getServerType()

    -- Replace Chimera functions with Balltze functions
    write_bit = balltze.memory.writeBit
    write_byte = balltze.memory.writeInt8
    write_word = balltze.memory.writeInt16
    write_dword = balltze.memory.writeInt32
    write_int = balltze.memory.writeInt32
    write_float = balltze.memory.writeFloat
    write_string = function(address, value)
        for i = 1, #value do
            write_byte(address + i - 1, string.byte(value, i))
        end
        if #value == 0 then
            write_byte(address, 0)
        end
    end
    execute_script = engine.hsc.executeScript
end

local main

function PluginFirstTick()
    weapons.get()
    sounds.get()
    balltze.event.tick.subscribe(function(event)
        if event.time == "before" then
            if not main then
                main = require "helljumper.main"
            end
            for handleValue in pairs(objectReferences) do
                if not get_object(handleValue) then
                    objectReferences[handleValue] = nil
                    collectgarbage("collect")
                end
            end
        end
    end)
end

function PluginLoad()
    logger = balltze.logger.createLogger("Helljumper")
    logger:muteDebug(not DebugMode)
    logger:muteIngame(not DebugMode)
    loadChimeraCompatibility()

    Balltze.event.frame.subscribe(function(ev)
        if ev.time == "before" then
            local font = "small"
            local align = "center"
            local bounds = {left = 0, top = 457, right = 640, bottom = 480}
            local textColor = {1.0, 0.663, 0.776, 0.953}
            local memory = collectgarbage("count")
            local sizeInMb = memory / 1024
            local text = string.format("Lua Memory Usage: %.4f MB", sizeInMb)
            Balltze.chimera.draw_text(text, bounds.left, bounds.top, bounds.right, bounds.bottom, font, align, table.unpack(textColor))
        end
    end)

    return true
end

function PluginUnload()
end
