-- SPDX-License-Identifier: GPL-3.0-only

------------------------------------------------------------------------------
-- Promethean UI 
-- Module: Font Override
-- Source: https://github.com/JerryBrick/promethean
------------------------------------------------------------------------------

local fontOverrides = {
    {
        family = "Nexa Demo Light",
        tag = "nexa-light-subtitle",
        size = 9,
        weight = 800,
        offset = {
            x = 0,
            y = 3
        },
        shadowOffset = {
            x = 1,
            y = 1
        }
    },
}

local function setupOverrides()
    for _, font in pairs(fontOverrides) do
        local fontTag = Engine.tag.findTags(font.tag, Engine.tag.classes.font)[1]
        if(fontTag) then
            Balltze.chimera.create_font_override(fontTag.handle.value, font.family, font.size, font.weight, font.shadowOffset.x, font.shadowOffset.y, font.offset.x, font.offset.y)
            logger:debug("Font override created for {} with font {}", font.tag, font.family)
        else
            logger:error("Failed to override {} font!", font.tag)
        end
    end
end

return {
    setup = setupOverrides
}
