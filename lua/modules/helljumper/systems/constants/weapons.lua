local weapons = {}

function weapons.get()
    -- Resolved here and not as a module upvalue: the tag API is only guaranteed to be in
    -- place once a map is loaded, and filterTags errors if called before that.
    local filterTags = Engine.tag.filterTags

    weapons.weaponTag = {
        assaultRifle = filterTags("weapon", "assault_rifle_ma38")[1],
        assaultRifleEx = filterTags("weapon", "assault_rifle_ma38_ex")[1],
        vk78Commando = filterTags("weapon", "vk78_commando")[1],
        battleRifle = filterTags("weapon", "br_65h")[1],
        dmr = filterTags("weapon", "dmr_392")[1],
        saw = filterTags("weapon", "lmg_saw")[1],
        m90Shotgun = filterTags("weapon", "shotgun_m90")[1],
        smgM7 = filterTags("weapon", "smg_m7")[1],
        sniperRifle = filterTags("weapon", "sniper_rifle_srs99c")[1],
        plasmaCaster = filterTags("weapon", "cv_grenade_launcher")[1],
        stormRifle = filterTags("weapon", "cv_storm_rifle")[1],
        stalkerRifle = filterTags("weapon", "stalker_rifle")[1],
        magnumM6s = filterTags("weapon", "magnum_m6s")[1],
        plasmaPistol = filterTags("weapon", "plasma_pistol")[1],
        needler = filterTags("weapon", "needler_t54c")[1],
        disruptor = filterTags("weapon", "proto_arc_zapper")[1],
        spnkr = filterTags("weapon", "m41_spknr")[1],
        skewer = filterTags("weapon", "skewer")[1],
        plasmaCoil = filterTags("weapon", "plasma_fusioncoil")[1],
        railGun = filterTags("weapon", "rail_gun")[1],
        plasmaRifle = filterTags("weapon", "plasma_rifle")[1]
    }

    weapons.weaponHudInterfaceTag = {
        assaultRifle = filterTags("weapon_hud_interface", "assault_rifle_ma38")[1],
        assaultRifleADS = filterTags("weapon_hud_interface", "assault_rifle_ma38_ads")[1],
        assaultRifleADSNumbers = filterTags("weapon_hud_interface", "assault_rifle_ma38_ads_small_number")[1],
        assaultRifleEx = filterTags("weapon_hud_interface", "assault_rifle_ma38")[1],
        vk78Commando = filterTags("weapon_hud_interface", "vk78_commando")[1],
        battleRifle = filterTags("weapon_hud_interface", "br_65h")[1],
        dmr = filterTags("weapon_hud_interface", "dmr_392")[1],
        saw = filterTags("weapon_hud_interface", "lmg_saw")[1],
        m90Shotgun = filterTags("weapon_hud_interface", "shotgun_m90")[1],
        smgM7 = filterTags("weapon_hud_interface", "smg_m7")[1],
        sniperRifle = filterTags("weapon_hud_interface", "sniper_rifle")[1],
        plasmaCaster = filterTags("weapon_hud_interface", "cv_grenade_launcher")[1],
        stormRifle = filterTags("weapon_hud_interface", "cv_storm_rifle")[1],
        stalkerRifle = filterTags("weapon_hud_interface", "stalker_rifle")[1],
        magnumM6s = filterTags("weapon_hud_interface", "magnum_m6s")[1],
        plasmaPistol = filterTags("weapon_hud_interface", "plasma_pistol")[1],
        needler = filterTags("weapon_hud_interface", "needler_t54c")[1],
        disruptor = filterTags("weapon_hud_interface", "proto_arc_zapper")[1],
        spnkr = filterTags("weapon_hud_interface", "rocket_launcher_spnkr")[1],
        skewer = filterTags("weapon_hud_interface", "skewer")[1],
        plasmaCoil = filterTags("weapon_hud_interface", "fusioncoil")[1],
        plasmaRifle = filterTags("weapon_hud_interface", "plasma_rifle")[1]
    }
    Balltze.logger.debug("Loaded Weapon and weaponHudInterface tags")
end


return weapons
