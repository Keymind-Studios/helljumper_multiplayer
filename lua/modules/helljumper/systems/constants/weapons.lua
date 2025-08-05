local tagClasses = Engine.tag.classes
local findTags = Engine.tag.findTags

local weapons = {}

function weapons.get()
    weapons.weaponTag = {
        assaultRifle = findTags("assault_rifle_ma38", tagClasses.weapon)[1],
        assaultRifleEx = findTags("assault_rifle_ma38_ex", tagClasses.weapon)[1],
        vk78Commando = findTags("vk78_commando", tagClasses.weapon)[1],
        battleRifle = findTags("br_65h", tagClasses.weapon)[1],
        dmr = findTags("dmr_392", tagClasses.weapon)[1],
        saw = findTags("lmg_saw", tagClasses.weapon)[1],
        m90Shotgun = findTags("shotgun_m90", tagClasses.weapon)[1],
        smgM7 = findTags("smg_m7", tagClasses.weapon)[1],
        sniperRifle = findTags("sniper_rifle_srs99c", tagClasses.weapon)[1],
        plasmaCaster = findTags("cv_grenade_launcher", tagClasses.weapon)[1],
        stormRifle = findTags("cv_storm_rifle", tagClasses.weapon)[1],
        stalkerRifle = findTags("stalker_rifle", tagClasses.weapon)[1],
        magnumM6s = findTags("magnum_m6s", tagClasses.weapon)[1],
        plasmaPistol = findTags("plasma_pistol", tagClasses.weapon)[1],
        needler = findTags("needler_t54c", tagClasses.weapon)[1],
        disruptor = findTags("proto_arc_zapper", tagClasses.weapon)[1],
        spnkr = findTags("m41_spknr", tagClasses.weapon)[1],
        skewer = findTags("skewer", tagClasses.weapon)[1],
        plasmaCoil = findTags("plasma_fusioncoil", tagClasses.weapon)[1]
    }

    weapons.weaponHudInterfaceTag = {
        assaultRifle = findTags("assault_rifle_ma38", tagClasses.weaponHudInterface)[1],
        assaultRifleEx = findTags("assault_rifle_ma38", tagClasses.weaponHudInterface)[1],
        vk78Commando = findTags("vk78_commando", tagClasses.weaponHudInterface)[1],
        battleRifle = findTags("br_65h", tagClasses.weaponHudInterface)[1],
        dmr = findTags("dmr_392", tagClasses.weaponHudInterface)[1],
        saw = findTags("lmg_saw", tagClasses.weaponHudInterface)[1],
        m90Shotgun = findTags("shotgun_m90", tagClasses.weaponHudInterface)[1],
        smgM7 = findTags("smg_m7", tagClasses.weaponHudInterface)[1],
        sniperRifle = findTags("sniper_rifle", tagClasses.weaponHudInterface)[1],
        plasmaCaster = findTags("cv_grenade_launcher", tagClasses.weaponHudInterface)[1],
        stormRifle = findTags("cv_storm_rifle", tagClasses.weaponHudInterface)[1],
        stalkerRifle = findTags("stalker_rifle", tagClasses.weaponHudInterface)[1],
        magnumM6s = findTags("magnum_m6s", tagClasses.weaponHudInterface)[1],
        plasmaPistol = findTags("plasma_pistol", tagClasses.weaponHudInterface)[1],
        needler = findTags("needler_t54c", tagClasses.weaponHudInterface)[1],
        disruptor = findTags("proto_arc_zapper", tagClasses.weaponHudInterface)[1],
        spnkr = findTags("rocket_launcher_spnkr", tagClasses.weaponHudInterface)[1],
        skewer = findTags("skewer", tagClasses.weaponHudInterface)[1],
        plasmaCoil = findTags("fusioncoil", tagClasses.weaponHudInterface)[1]
    }
    logger:debug("Loaded Weapon and weaponHudInterface tags")
end


return weapons