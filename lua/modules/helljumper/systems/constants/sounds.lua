local tagClasses = Engine.tag.classes
local findTags = Engine.tag.findTags

local sounds = {}

function sounds.get()
    sounds.soundTag = {
        uiGrenadePlasma = findTags("001_plasma_grenade", tagClasses.sound)[1],
        uiGrenadeFrag = findTags("001_frag_grenade", tagClasses.sound)[1]
    }
    logger:debug("Loaded Sound tags")
end

return sounds