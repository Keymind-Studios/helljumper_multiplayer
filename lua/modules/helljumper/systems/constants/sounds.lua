local sounds = {}

function sounds.get()
    -- Resolved here and not as a module upvalue: the tag API is only guaranteed to be in
    -- place once a map is loaded, and filterTags errors if called before that.
    local filterTags = Engine.tag.filterTags

    sounds.soundTag = {
        uiGrenadePlasma = filterTags("sound", "001_plasma_grenade")[1],
        uiGrenadeFrag = filterTags("sound", "001_frag_grenade")[1],
        humanRifleZoomIn = filterTags("sound", "007_human_rifle_zoom_in")[1],
        humanRifleZoomOut = filterTags("sound", "007_human_rifle_zoom_out")[1]
    }
    Balltze.logger.debug("Loaded Sound tags")
end

return sounds
