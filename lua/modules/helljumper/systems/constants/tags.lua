local engine = Engine
local balltze = Balltze
local lookupTag = engine.tag.lookupTag
local tag = require "helljumper.systems.constants.paths"

local tags = {}

function tags.get()
    tags.weapon = {
        assaultRifle = lookupTag(tag.weapon.human.assaultRifleMa38, "weapon"),
        battleRifle = lookupTag(tag.weapon.human.br65h, "weapon"),
        magnumM6s = lookupTag(tag.weapon.human.magnumM6s, "weapon"),
        saw = lookupTag(tag.weapon.human.saw, "weapon"),
        shotgun = lookupTag(tag.weapon.human.shotgunM90, "weapon"),
        sniper = lookupTag(tag.weapon.human.sniper, "weapon"),
        vk78Commando = lookupTag(tag.weapon.human.vk78Commando, "weapon"),
        spnkr = lookupTag(tag.weapon.human.spnkr, "weapon"),
        disruptor = lookupTag(tag.weapon.covenant.disruptor, "weapon"),
        fusionCoil = lookupTag(tag.weapon.covenant.fusionCoil, "weapon"),
        needler = lookupTag(tag.weapon.covenant.needler, "weapon"),
        plasmaCaster = lookupTag(tag.weapon.covenant.plasmaCaster, "weapon"),
        plasmaPistol = lookupTag(tag.weapon.covenant.plasmaPistol, "weapon"),
        plasmaRifle = lookupTag(tag.weapon.covenant.plasmaRifle, "weapon"),
        skewer = lookupTag(tag.weapon.covenant.skewer, "weapon"),
        stalkerRifle = lookupTag(tag.weapon.covenant.stalkerRifle, "weapon"),
        stormRifle = lookupTag(tag.weapon.covenant.stormRifle, "weapon"),
    }
    tags.sound = {
        hudGrenades = {
            fragSelected = lookupTag(tag.sound.ui.hud.grenades.fragSelected, "sound"),
            plasmaSelected = lookupTag(tag.sound.ui.hud.grenades.plasmaSelected, "sound"),
        },
        adsHumanZoom = {
            humanAdsIn = lookupTag(tag.sound.ui.hud.weapons.aimingDownSight.humanAdsIn, "sound"),
            humanAdsOut = lookupTag(tag.sound.ui.hud.weapons.aimingDownSight.humanAdsOut, "sound"),
        },
    }
    tags.weaponHudInterface = {
        assaultRifle = lookupTag(tag.weapon.human.assaultRifleMa38, "weapon_hud_interface"),
        battleRifle = lookupTag(tag.weapon.human.br65h, "weapon_hud_interface"),
        magnumM6s = lookupTag(tag.weapon.human.magnumM6s, "weapon_hud_interface"),
        saw = lookupTag(tag.weapon.human.saw, "weapon_hud_interface"),
        shotgun = lookupTag(tag.weapon.human.shotgunM90, "weapon_hud_interface"),
        sniper = lookupTag(tag.weapon.human.sniper, "weapon_hud_interface"),
        vk78Commando = lookupTag(tag.weapon.human.vk78Commando, "weapon_hud_interface"),
        spnkr = lookupTag(tag.weapon.human.spnkr, "weapon_hud_interface"),
        disruptor = lookupTag(tag.weapon.covenant.disruptor, "weapon_hud_interface"),
        fusionCoil = lookupTag(tag.weapon.covenant.fusionCoil, "weapon_hud_interface"),
        needler = lookupTag(tag.weapon.covenant.needler, "weapon_hud_interface"),
        plasmaCaster = lookupTag(tag.weapon.covenant.plasmaCaster, "weapon_hud_interface"),
        plasmaPistol = lookupTag(tag.weapon.covenant.plasmaPistol, "weapon_hud_interface"),
        plasmaRifle = lookupTag(tag.weapon.covenant.plasmaRifle, "weapon_hud_interface"),
        skewer = lookupTag(tag.weapon.covenant.skewer, "weapon_hud_interface"),
        stalkerRifle = lookupTag(tag.weapon.covenant.stalkerRifle, "weapon_hud_interface"),
        stormRifle = lookupTag(tag.weapon.covenant.stormRifle, "weapon_hud_interface"),
        -- HUDs a weapon reaches through another HUD's child hud reference, so they are looked up by
        -- a path of their own rather than by the weapon's the way the ones above are.
        child = {
            sniperRifleTicks = lookupTag(tag.weaponHudInterface.child.sniperRifleTicks,
                                         "weapon_hud_interface"),
            sniperRifleExtMeters = lookupTag(tag.weaponHudInterface.child.sniperRifleExtMeters,
                                             "weapon_hud_interface")
        }
    }
    balltze.logger.debug("Loaded {}, {}, {} Tags", tags.weapon, tags.sound, tags.weaponHudInterface)
end

return tags