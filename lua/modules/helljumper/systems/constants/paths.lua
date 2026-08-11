-- Tag Paths

local tag = {}

tag.biped = {
    human = {
        melissaMckay = "keymind\\helljumper\\objects\\characters\\melissa_mckay\\melissa_mckay"
    },
}

tag.collision = {
    player = {
        singleplayerGrid = "keymind\\helljumper\\objects\\characters\\melissa_mckay\\melissa_mckay",
        multiplayerGrid = ""
    },
}

--Weapon and  weaponHudInterface shares the same tag paths, so we can use the same table for both 
tag.weapon = {
    human = {
        assaultRifleMa38 = "keymind\\helljumper\\objects\\weapons\\rifle\\assault_rifle\\assault_rifle_ma38",
        vk78Commando = "keymind\\helljumper\\objects\\weapons\\rifle\\vk78_commando\\vk78_commando",
        br65h = "keymind\\helljumper\\objects\\weapons\\rifle\\br65h\\br_65h",
        dmr392 = "keymind\\helljumper\\objects\\weapons\\rifle\\dmr\\dmr_392",
        saw = "keymind\\helljumper\\objects\\weapons\\rifle\\lmg_saw\\lmg_saw",
        shotgunM90 = "keymind\\helljumper\\objects\\weapons\\rifle\\shotgun_m90\\shotgun_m90",
        smgM7 = "keymind\\helljumper\\objects\\weapons\\rifle\\smg\\smg_m7",
        sniper = "keymind\\helljumper\\objects\\weapons\\rifle\\sniper_rifle\\sniper_rifle_srs99c",
        magnumM6s = "keymind\\helljumper\\objects\\weapons\\pistol\\magnum_m6s\\magnum_m6s",
        spnkr = "keymind\\helljumper\\objects\\weapons\\support_high\\spnkr_rocket_launcher\\m41_spknr",
        variant = {
            -- Assault Rifle MA38 High Explosive Rounds
            Ma38Explosive = "keymind\\helljumper\\objects\\weapons\\rifle\\assault_rifle\\assault_rifle_ma38_ex",
        },
    },
    covenant = {
        plasmaCaster = "keymind\\helljumper\\objects\\weapons\\rifle\\cv_grenade_launcher\\cv_grenade_launcher",
        stormRifle = "keymind\\helljumper\\objects\\weapons\\rifle\\cv_storm_rifle\\cv_storm_rifle",
        stalkerRifle = "keymind\\helljumper\\objects\\weapons\\rifle\\stalker_rifle\\stalker_rifle",
        plasmaPistol = "keymind\\helljumper\\objects\\weapons\\pistol\\plasma_pistol\\plasma_pistol",
        needler = "keymind\\helljumper\\objects\\weapons\\pistol\\needler\\needler_t54c",
        disruptor = "keymind\\helljumper\\objects\\weapons\\pistol\\proto_arc_zapper\\proto_arc_zapper",
        skewer = "keymind\\helljumper\\objects\\weapons\\support_high\\skewer\\skewer",
        plasmaRifle = "keymind\\helljumper\\objects\\weapons\\pistol\\plasma_rifle\\plasma_rifle",
        fusionCoil = "keymind\\halo_infinite\\weapons\\support\\fusioncoil\\plasma\\plasma_fusioncoil",
        variant = {}
    },
    forerunner = {},
    flood = {}
}

tag.equipment = {
    human = {},
    covenant = {},
    forerunner = {},
    flood = {}
}

tag.deviceControl = {
    ammo = {
        -- Armor Piercing Ammo = 150 rounds
        ap_726_ammo_150 = "keymind\\helljumper\\objects\\devices\\ammo_crates\\ap_726_ammo_150",
        -- Explosive Ammo = 150 rounds
        ex_726_ammo_150 = "keymind\\helljumper\\objects\\devices\\ammo_crates\\ex_726_ammo_150",
        -- Incendiary Ammo = 150 rounds
        ir_726_ammo_150 = "keymind\\helljumper\\objects\\devices\\ammo_crates\\ir_726_ammo_150",
        -- Disruption Ammo = 150 rounds
        ds_726_ammo_150 = "keymind\\helljumper\\objects\\devices\\ammo_crates\\dr_726_ammo_150"
    }
}

tag.sound = {
    ui = {
        hud = {
            grenades = {
                fragSelected = "keymind\\the_flood\\sound\\001_ui\\001_ui_hud\\001_ui_hud_grenades\\001_frag_grenade",
                plasmaSelected = "keymind\\the_flood\\sound\\001_ui\\001_ui_hud\\001_ui_hud_grenades\\001_plasma_grenade"
            },
            weapons = {
                aimingDownSight = {
                    humanAdsIn = "keymind\\the_flood\\sound\\007_weapon\\007_wea_shared\\007_human_rifle_zoom\\007_human_rifle_zoom_in",
                    humanAdsOut = "keymind\\the_flood\\sound\\007_weapon\\007_wea_shared\\007_human_rifle_zoom\\007_human_rifle_zoom_out"
                },
            },
        },
        menus = {}
    },
    sfx = {}
}

return tag