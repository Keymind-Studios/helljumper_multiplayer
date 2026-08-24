--------------------------------------------------------------------------------------------------
-- Every colour this project draws with, named once.
--
-- Written one way and one way only: the four channels as whole bytes, 0 to 255, alpha first. That
-- is what Guerilla shows, so a colour read off a tag is typed straight in, and it is what a colour
-- picker hands back. No fractions in the source, which is what 120 / 255 sitting in a table was.
--
-- Where a colour ends up is what decides the form it is wanted in, and there are two:
--
--   Engine.interface wants the channels apart and each as a fraction of one. colors.interface.
--   A colour field inside a tag wants the four bytes packed into one number, which the settings
--   tables that write tags do at the moment of the write. Those take the palette as written.
--
-- Both are worked out once, when this module is required, and never per call. Which matters past
-- tidiness for the interface form: core.setText tells "the same colour as last tick" by identity, so
-- a table built afresh on each call would have it take the text down and put it back up thirty times
-- a second.
--------------------------------------------------------------------------------------------------

local colors = {}

--- The four channels as the one number a colour field inside a tag keeps them in
---
--- Here rather than in each module that writes a tag. Three of them carried their own copy of this
--- arithmetic, which is three places for one of them to drift.
---@param color {a: integer, r: integer, g: integer, b: integer} @channels as bytes
---@return integer
function colors.pack(color)
    -- Arithmetic rather than shifts, so this does not depend on which Lua it is running under: the
    -- server side of this project still goes through compat53.
    return color.a * 0x1000000 + color.r * 0x10000 + color.g * 0x100 + color.b
end

--------------------------------------------------------------------------------------------------
-- The palette. One line per colour, named for what it is for rather than for what it looks like: a
-- name that says "the empty gear text" survives being retuned, and one that says "pale blue" does
-- not.
--
-- A colour belongs here when more than one place draws it, or when it is part of what the HUD looks
-- like. A one-off tuning value inside a single weapon's settings does not: it is that weapon's
-- business and reads better beside it, written in this same notation.
--------------------------------------------------------------------------------------------------

local palette = {
    gearText = {a = 255, r = 169, g = 198, b = 243},           --rgba(169,198,243,1)
    gearTextEmpty = {a = 120, r = 200, g = 208, b = 255},      --rgba(200,208,255,0.471)
    adsReadout = {a = 255, r = 181, g = 227, b = 255},         --rgba(181,227,255,1)
    adsReadoutCovenant = {a = 255, r = 227, g = 203, b = 255}, --rgba(227,203,255,1)
    secondaryWeaponIcon = {a = 120, r = 84, g = 99, b = 122},  --rgba(84,99,122,0.471)
    zoomMeterEmpty = {a = 70, r = 70, g = 70, b = 70},         --rgba(70,70,70,0.275)
}

--------------------------------------------------------------------------------------------------
-- The two ways to reach a colour of the palette. Which one a place wants is decided by what it
-- hands the colour to, and nothing here converts on the fly: both are worked out once, when this
-- module is required.
--------------------------------------------------------------------------------------------------

--- The palette as it is written, channels as bytes
---
--- What every settings table that ends up written into a tag takes, since those pack at the moment
--- of the write and want the channels apart until then.
---@type table<string, {a: integer, r: integer, g: integer, b: integer}>
colors.palette = palette

--- The same colours with the channels as fractions of one, which is what Engine.interface reads
--- them as. Hand these round; never copy one, or core.setText stops recognising it.
---@type table<string, {a: number, r: number, g: number, b: number}>
colors.interface = {}

for name, color in pairs(palette) do
    colors.interface[name] = {
        a = color.a / 255,
        r = color.r / 255,
        g = color.g / 255,
        b = color.b / 255
    }
end

--------------------------------------------------------------------------------------------------
-- Reference only, and read by nothing: the set these were picked out of, kept in the notation they
-- were picked in. A colour that comes to be drawn is written into the palette above instead, as
-- bytes, under a name that says what it is for.
--------------------------------------------------------------------------------------------------

colors.reference = {
    ---SULFUR---
    cookies_and_cream = "#d9e1ac",
    misty_moss = "#b3b873",
    oxley = "#74a071",
    mustard_green = "#62703c",
    soldier__green = "#505929",
    ---SILVER---
    opal_white = "#a4c0c1", -- best option
    weldon_blue = "#83a1a6",
    steel_teal = "#578a92", -- best option
    dark_electric_blue = "#507178",
    ---BLUE---
    jordi_blue = "#8bb2ff",
    catalina = "#142d75",
    ---GREEN---
    russian_green = "#77935d",
    japanese_laurel = "#3b701d",
    lincoln_green = "#225004",
    ---PURPLE---
    waterloo = "#82839a",
    indigo = "#261e7b",
    ---GOLDEN---
    sahara = "#b1af14",
    verdun = "#5c4d00",
    ---RED---
    copper = "#9d646b",
    dark_tan = "#6a0d0f",
    tamarind = "#341722",
    ---ORANGE---
    bell = "#e38e15",
    fire = "#a74500",
    beech = "#7e3000",
    ---BLACK---
    space = "#273132",
    black = "#000000",
    ---WHITE---
    white = "#FFFFFF",
    mist = "#a5c5cb",
    ---CYANOTIC---
    mint = "#96edca",
    mulled_wine = "#473e5a",
    ---NIGHTFALL---
    keppel = "#37979d",
    eden = "#125962",
    elephant = "#0c3739",
    ---MATRIX---
    blue_chill = "#147d97",
    meadow = "#1ca569",
    ---SUNSHINE---
    jellyfish = "#5bc4b2",
    camel = "#c2a54d",
    ---CHIMERA---
    blueberry = "#3c379a",
    valentine = "#e35a58",
    ---IRIDESCENT---
    rose = "#fa51aa",
    bush = "#633da9",
    tealish = "#a6b5eb",
    ---COALESCENCE---
    american_blue = "#3e2f76",
    crayola = "#eb1e49",
    berry = "#a01c4b"
}

return colors
