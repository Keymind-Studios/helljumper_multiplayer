-- Lua libraries
local balltze = Balltze
local json = require "json"
local input = require "helljumper.systems.core.input"

--------------------------------------------------------------------------------------------------
-- What a player can change without touching the code, read out of settings.json at the root of the
-- plugin.
--
-- WHY THAT FILE. It is already there: Balltze keeps the arguments of every command registered with
-- autosave under its "commands" key, and replays them with Balltze.loadSettings. Everything this
-- module owns sits beside that key rather than inside it, and is written back without touching it.
--
-- WHAT IS IN IT. One entry per input action under "controls", named the way the system that reads it
-- asks for it. A binding names its key, mouse button and pad button out of input.key, input.mouse and
-- input.gamepad, or gives the raw code a press was seen with through `helljumper_multiplayer_inputs`,
-- or says "none" to leave that device out:
--
--   "controls": {
--       "aimDownSights": {
--           "cancelInput": false,
--           "gamepadButton": "rightStick",
--           "key": "z",
--           "mouseButton": "middle"
--       }
--   }
--
-- "none" rather than null, because a null reads back as a missing entry, and a missing entry is
-- filled in with the default and written back so the file always shows every option there is.
--------------------------------------------------------------------------------------------------

local settings = {}

-- Relative to the plugin directory, which is what every Balltze.filesystem path is relative to.
local fileName = "settings.json"

-- The value that leaves a device out of a binding.
local noBinding = "none"

-- The decoded file, read once on first use.
---@type table|nil
local document = nil
-- Whether the file can be written back. False when it is there but could not be read as JSON: the
-- defaults are used for this session and the file is left alone, rather than wiping whatever the
-- player had written in it along with the mistake.
local isWritable = false
local isDirty = false

--- Read the file, once
---@return table
local function getDocument()
    if document then
        return document
    end
    document = {}
    if not balltze.filesystem.fileExists(fileName) then
        isWritable = true
        return document
    end

    local text = balltze.filesystem.readFile(fileName)
    if text == nil then
        balltze.logger.warning("settings: {} could not be opened, using the defaults", fileName)
        return document
    end
    -- The byte order mark some Windows editors add is not skipped by the decoder.
    text = text:gsub("^\239\187\191", "")
    if text:match("^%s*$") then
        isWritable = true
        return document
    end

    local isDecoded, decoded = pcall(json.decode, text)
    if not isDecoded or type(decoded) ~= "table" then
        balltze.logger.error("settings: {} is not valid JSON, using the defaults until it is fixed: {}",
                             fileName, tostring(decoded))
        return document
    end
    document = decoded
    isWritable = true
    return document
end

--------------------------------------------------------------------------------------------------
-- Writing it back
--
-- By hand rather than through json.encode, which puts the whole file on one line in whatever order
-- the table hands its keys back: a file meant to be edited wants one entry to a line, indented, and
-- in the same order every time it is written.
--------------------------------------------------------------------------------------------------

local indentUnit = "    "

--- Whether a table is a list rather than an object. An empty one is written as an object, which is
--- what Balltze itself writes for a file with nothing in it.
---@param value table
---@return boolean
local function isList(value)
    local count = #value
    if count == 0 then
        return false
    end
    for key in pairs(value) do
        if math.type(key) ~= "integer" or key < 1 or key > count then
            return false
        end
    end
    return true
end

---@param value any
---@param indent string
---@return string
local function encode(value, indent)
    if type(value) ~= "table" then
        return json.encode(value)
    end
    local inner = indent .. indentUnit
    local lines = {}
    if isList(value) then
        for index = 1, #value do
            lines[index] = inner .. encode(value[index], inner)
        end
        return "[\n" .. table.concat(lines, ",\n") .. "\n" .. indent .. "]"
    end
    local keys = {}
    for key in pairs(value) do
        keys[#keys + 1] = tostring(key)
    end
    if #keys == 0 then
        return "{}"
    end
    table.sort(keys)
    for index = 1, #keys do
        local key = keys[index]
        lines[index] = inner .. json.encode(key) .. ": " .. encode(value[key], inner)
    end
    return "{\n" .. table.concat(lines, ",\n") .. "\n" .. indent .. "}"
end

--- Write the file back, if anything was added to it since it was read
function settings.save()
    if not isDirty or not isWritable then
        return
    end
    local isEncoded, text = pcall(encode, getDocument(), "")
    if not isEncoded then
        balltze.logger.error("settings: could not write {}: {}", fileName, tostring(text))
        return
    end
    balltze.filesystem.writeFile(fileName, text .. "\n")
    isDirty = false
end

--------------------------------------------------------------------------------------------------
-- Bindings
--------------------------------------------------------------------------------------------------

-- Each device a binding can name, the field it is written under in the file, and the field it is
-- handed to input.newAction as.
local bindingDevices = {
    {field = "key", actionField = "keyCode", codes = input.key},
    {field = "mouseButton", actionField = "mouseButton", codes = input.mouse},
    {field = "gamepadButton", actionField = "gamepadButton", codes = input.gamepad}
}

--- A binding's name looked up without caring how it was capitalised
---@param codes table<string, integer>
---@param name string
---@return integer|nil
local function findCode(codes, name)
    local code = codes[name]
    if code then
        return code
    end
    local lowered = name:lower()
    for codeName, codeValue in pairs(codes) do
        if codeName:lower() == lowered then
            return codeValue
        end
    end
    return nil
end

--- What one device of a binding comes to
---@param value any @as it was read out of the file
---@param codes table<string, integer>
---@return boolean isValid
---@return integer|nil code @nil when the device is left out
local function resolveCode(value, codes)
    if value == noBinding or value == false then
        return true, nil
    end
    if type(value) == "number" then
        local code = math.tointeger(value)
        return code ~= nil and code >= 0, code
    end
    if type(value) == "string" then
        local code = findCode(codes, value)
        return code ~= nil, code
    end
    return false, nil
end

--- The binding of an input action, as the player wrote it, ready for input.newAction
---
--- Whatever the file does not say yet is filled in with the default and written back, so a first run
--- leaves every option in the file for the player to find. Whatever it says wrong is warned about and
--- replaced with the default for this session only, so the mistake stays there to be corrected.
---
--- The defaults are names out of input.key, input.mouse and input.gamepad; nil or "none" leaves a
--- device out.
---@param actionName string @its entry under "controls"
---@param defaults {key: string|integer|nil, mouseButton: string|integer|nil, gamepadButton: string|integer|nil, cancelInput: boolean|nil}
---@return InputActionSettings
function settings.binding(actionName, defaults)
    local root = getDocument()
    if type(root.controls) ~= "table" then
        root.controls = {}
        isDirty = true
    end
    local entry = root.controls[actionName]
    if type(entry) ~= "table" then
        entry = {}
        root.controls[actionName] = entry
        isDirty = true
    end

    local binding = {}
    for index = 1, #bindingDevices do
        local device = bindingDevices[index]
        local default = defaults[device.field]
        if default == nil then
            default = noBinding
        end
        if entry[device.field] == nil then
            entry[device.field] = default
            isDirty = true
        end
        local isValid, code = resolveCode(entry[device.field], device.codes)
        if not isValid then
            balltze.logger.warning("settings: controls.{}.{} = {} is not a {} binding, using {}",
                                   actionName, device.field, tostring(entry[device.field]),
                                   device.field, tostring(default))
            isValid, code = resolveCode(default, device.codes)
            assert(isValid, "settings: the default " .. device.field .. " of " .. actionName ..
                       " is not a binding: " .. tostring(default))
        end
        binding[device.actionField] = code
    end

    local defaultCancelInput = defaults.cancelInput == true
    if entry.cancelInput == nil then
        entry.cancelInput = defaultCancelInput
        isDirty = true
    end
    if type(entry.cancelInput) == "boolean" then
        binding.cancelInput = entry.cancelInput
    else
        balltze.logger.warning("settings: controls.{}.cancelInput should be true or false, using {}",
                               actionName, tostring(defaultCancelInput))
        binding.cancelInput = defaultCancelInput
    end

    settings.save()
    return binding
end

return settings
