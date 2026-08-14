-- Lua libraries
local balltze = Balltze
local engine = Engine

--------------------------------------------------------------------------------------------------
-- The one "player_input" listener, and the presses every system reads off it.
--
-- WHY THIS EXISTS. Balltze keeps a single listener per event name: subscribing a second time to an
-- event replaces whatever was on it rather than joining a list, and the one that wins is the one
-- that registered last. Two systems that each put up their own "player_input" listener therefore
-- leave exactly one of them working, and which one comes down to the order their tick functions run
-- in, which reads as one system silently breaking the other from across the project. It was found
-- the way these things are: the aim and the objective ping each worked, and never at the same time.
--
-- So the listener is subscribed here, once, and a system that wants a press asks for an action
-- instead of for a listener. Adding a fourth thing that reacts to a key is a table, not another
-- subscription, and no system can take another one's input away by existing.
--
-- WHAT AN ACTION IS. A binding per device and a flag saying it has been pressed since last asked.
-- The flag is what the tick reads, rather than the system reacting inside the event itself: input
-- arrives whenever the player presses something, gameplay is decided on the tick, and consuming a
-- press on the tick is what keeps one press from being acted on twice.
--
-- HOW IT IS USED. An action is made once, at load, from whatever the system's own configuration
-- says its binding is; the press is read on the tick with :consume(). Neither has to know this
-- module holds a listener at all.
--
--   local input = require "helljumper.systems.core.input"
--   local toggle = input.newAction {mouseButton = 1, gamepadButton = 9}
--
--   function system.onTick()
--       if toggle:consume() then
--           ...
--       end
--   end

--------------------------------------------------------------------------------------------------

local input = {}

--------------------------------------------------------------------------------------------------
-- What the codes mean, so a binding reads as the key it is rather than as the number the engine
-- happens to give it. A system names one of these instead of writing the number out:
--
--   input.newAction {keyCode = input.key.c, mouseButton = input.mouse.right}
--
-- WHERE THESE COME FROM. Halo's own key table, which the v1 module read by hand at
-- keyboard_input_address + 60 and which player_input hands back unchanged. It runs in physical
-- keyboard order rather than in ASCII or scancode order, which is what makes the gaps below
-- predictable but not guessable: the number row runs 17 to 26, so 27 and 28 are the two keys after
-- it, and 29 is the one after those.
--
-- WHAT IS MISSING. Only what has actually been seen is written down. The codes nobody has confirmed
-- are left out rather than filled in by counting along the row: a wrong number here binds a key
-- that is never pressed, and nothing anywhere says so. Any of them takes ten seconds to find with
-- input.logInputs() at the bottom of this file, so a gap is a lookup and not a guess.
--------------------------------------------------------------------------------------------------

-- The digits are the row above the letters, not the numpad, which has codes of its own that nobody
-- has needed yet. backspace is out of ui/core.lua's translateKeycode; the rest are the v1 module's
-- own reference, with its "5" and its "0" put back where the row says they go.
input.key = {
    one = 17,
    two = 18,
    three = 19,
    four = 20,
    five = 21,
    six = 22,
    seven = 23,
    eight = 24,
    nine = 25,
    zero = 26,
    minus = 27,
    equals = 28,
    backspace = 29,
    tab = 30,
    q = 31,
    w = 32,
    e = 33,
    r = 34,
    t = 35,
    y = 36,
    u = 37,
    i = 38,
    o = 39,
    p = 40,
    backslash = 43,
    capsLock = 44,
    a = 45,
    s = 46,
    d = 47,
    f = 48,
    g = 49,
    h = 50,
    j = 51,
    k = 52,
    l = 53,
    enter = 56,
    shift = 57,
    z = 58,
    x = 59,
    c = 60,
    v = 61,
    b = 62,
    n = 63,
    m = 64,
    control = 69,
    alt = 71,
    space = 72
}

-- Numbered from zero, in the order the engine reports them. The five past the usual three are
-- whatever the mouse calls its side buttons, which differ by mouse, so they are named by number.
input.mouse = {
    left = 0,
    middle = 1,
    right = 2,
    button4 = 3,
    button5 = 4,
    button6 = 5,
    button7 = 6,
    button8 = 7
}

-- The standard XInput button order, which is what a modern pad reports through DirectInput and so
-- what Halo sees. Only rightStick is confirmed against this game: it is the 9 the aim was bound to
-- before these names existed, and it lands exactly where this order puts it. The rest are the order
-- itself rather than something anyone has pressed here, so check one with input.logInputs() before
-- shipping a binding on it, and expect a pad that is not XInput to disagree outright.
input.gamepad = {
    a = 0,
    b = 1,
    x = 2,
    y = 3,
    leftBumper = 4,
    rightBumper = 5,
    back = 6,
    start = 7,
    leftStick = 8,
    rightStick = 9
}

--- One thing a player can ask for by pressing something, and whether they have asked for it
---@class InputAction
---@field keyCode integer|nil @out of input.key
---@field mouseButton integer|nil @out of input.mouse
---@field gamepadButton integer|nil @out of input.gamepad
---@field cancelInput boolean @whether the game control this is bound to should still see the press
---@field isRequested boolean @pressed since the last consume
---@field lastInputTick integer @-1 before the first press
local InputAction = {}
InputAction.__index = InputAction

-- Every action asked for, walked on each input. A list rather than a map because nothing looks an
-- action up by name: the event arrives with a code, and what has to be worked out is which of these
-- wanted it, which can be more than one.
---@type InputAction[]
local actions = {}

--- Register that this action's input went down
---
--- The event repeats while an input is held, so a press has to be told apart from the tick it
--- landed on: without this a key held down asks for the action on every tick it is held for.
local function noteInput(action)
    local tick = engine.game.getTickCount()
    local isSamePress = action.lastInputTick >= 0 and tick - action.lastInputTick <= 1
    action.lastInputTick = tick
    if not isSamePress then
        action.isRequested = true
    end
end

--- Whether this action was asked for since the last time it was asked about, clearing it either way
---
--- Cleared whether or not the caller can act on it, which is what keeps a press made in a menu, or
--- with a weapon that cannot use it, from firing the moment the player is back in the game.
---@return boolean
function InputAction:consume()
    local isRequested = self.isRequested
    self.isRequested = false
    return isRequested
end

--- What a system says its binding is. Any device can be left out, and they can all be on at once,
--- so one action covers a key, a mouse button and a pad button asking for the same thing.
---@class InputActionSettings
---@field keyCode integer|nil @out of input.key
---@field mouseButton integer|nil @out of input.mouse
---@field gamepadButton integer|nil @out of input.gamepad
---@field cancelInput boolean|nil @suppresses the press only, never the hold; false when left out

--- Ask for an action. Made once, at load: it is the binding that is being declared, not the press.
---@param settings InputActionSettings
---@return InputAction
function input.newAction(settings)
    local action = setmetatable({
        keyCode = settings.keyCode,
        mouseButton = settings.mouseButton,
        gamepadButton = settings.gamepadButton,
        cancelInput = settings.cancelInput or false,
        isRequested = false,
        lastInputTick = -1
    }, InputAction)
    actions[#actions + 1] = action
    return action
end

-- Which field of an action a device's presses are bound with. Kept as a table so the device decides
-- what to compare against without a branch per action.
local deviceBindingFields = {keyboard = "keyCode", mouse = "mouseButton", gamepad = "gamepadButton"}

--- What was pressed, on whichever device raised the event
---@param event PlayerInputEvent
---@param device string
---@return integer|nil
local function getInputCode(event, device)
    -- getKeyCode, getMouseButton and getGamepadButton each raise on the wrong device, so the device
    -- decides which one is safe to ask. Asked once for the whole list rather than once per action,
    -- which is the other half of why the actions are walked here instead of in each system.
    if device == "keyboard" then
        return event:getKeyCode()
    elseif device == "mouse" then
        return event:getMouseButton()
    elseif device == "gamepad" then
        return event:getGamepadButton()
    end
    return nil
end

-- Whether every press is being said out loud, and what the last one said, so a key held down is one
-- line rather than a line a frame.
local isLoggingInputs = false
local lastLoggedDevice = nil
local lastLoggedCode = nil
local lastLoggedTick = -1

--- Say what is being pressed, for finding the code of a key this module does not name yet
---
--- Turned on from a console command or from a module while a binding is being worked out, and off
--- again afterwards: it is a lookup tool, not something to leave running. Sent as a warning rather
--- than as debug because the plugin's debug output does not reach ringworld's debug.txt.
---@param isEnabled boolean|nil @true when left out
function input.logInputs(isEnabled)
    isLoggingInputs = isEnabled ~= false
end

--- One line per press rather than per frame the press lasts
---@param device string
---@param inputCode integer
local function logInput(device, inputCode)
    local tick = engine.game.getTickCount()
    local isSamePress = lastLoggedDevice == device and lastLoggedCode == inputCode and
                            lastLoggedTick >= 0 and tick - lastLoggedTick <= 1
    lastLoggedDevice = device
    lastLoggedCode = inputCode
    lastLoggedTick = tick
    if not isSamePress then
        balltze.logger.warning("input: {} {}", device, inputCode)
    end
end

---@param event PlayerInputEvent
local function onPlayerInput(event)
    local device = event:getDevice()
    local bindingField = deviceBindingFields[device]
    if not bindingField then
        return
    end
    local inputCode = getInputCode(event, device)
    if inputCode == nil then
        return
    end
    if isLoggingInputs then
        logInput(device, inputCode)
    end
    for index = 1, #actions do
        local action = actions[index]
        local binding = action[bindingField]
        -- Checked against nil before it is compared: an action that leaves a device out has nothing
        -- bound on it, and nil == nil would have every such action answer to an input that somehow
        -- came back without a code.
        if binding ~= nil and binding == inputCode then
            if action.cancelInput then
                event:cancel()
            end
            noteInput(action)
        end
    end
end

-- Whether the listener is up, tracked apart from the handle because addEventListener does not hand
-- one back on every Balltze build.
local isListenerRegistered = false
local listener = nil

--- Put the listener up, once
---
--- Called from the tick rather than at load: subscribing from the bottom of a module as it is
--- required turned out not to take, which is what both of the systems that used to hold their own
--- listener had found separately. The guard is what keeps "from the tick" from meaning a listener a
--- tick, and it belongs here now rather than in each of them.
function input.ensureListener()
    if isListenerRegistered then
        return
    end
    listener = balltze.addEventListener("player_input", onPlayerInput)
    isListenerRegistered = true
end

--- Take the listener down and forget any press that was waiting to be read
---
--- require caches this module, so a reload would otherwise come back believing a listener that the
--- reload took down is still up, and never subscribe again. The actions themselves are left in
--- place: they are declared by the systems as they are required, and a reload declares them afresh.
function input.unload()
    if listener and listener.remove then
        listener:remove()
    end
    listener = nil
    isListenerRegistered = false
    for index = 1, #actions do
        local action = actions[index]
        action.isRequested = false
        action.lastInputTick = -1
    end
end

return input
