-- Lua libraries
local balltze = Balltze
local engine = Engine
local getObject = Engine.object.getObject
local getPlayer = Engine.player.getPlayer
local utils = require "helljumper.utils"
local hsc = require "hsc"
local input = require "helljumper.systems.core.input"
local path = require "helljumper.systems.constants.paths"

local aimingDownSights = {}

local floor = math.floor
local sqrt = math.sqrt
local sin = math.sin
local cos = math.cos
local pi = math.pi
local atan = math.atan
local tan = math.tan
local atan2 = math.atan2 or math.atan

--------------------------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------------------------

local toggleKeyCode = nil
local toggleMouseButton = input.mouse.middle
local toggleGamepadButton = input.gamepad.rightStick

-- Whether the game control this input is bound to should still see it.
local cancelToggleInput = false

local hipFieldOfView = 76
local fieldOfViewTolerance = 0.5

-- How far health or shield has to fall in a single tick to count as a hit taken.
local damagingVitalsDrop = 0.01

-- A vector_font tag carries its own size
local defaultFontPath = path.vectorFont.ui.hud.adsLarger
local smallFontPath = path.vectorFont.ui.hud.adsSmall
local defaultTextColor = {a = 1.0, r = 181 / 255, g = 227 / 255, b = 255/255}
local covenantColor = {a = 1.0, r = 227/255, g = 203/255, b = 255/255}

---@class AdsHudSettings
---@field widthScale number?
---@field heightScale number?
---@field anchorOffsetX integer?
---@field anchorOffsetY integer?
---@field anchorOffsetArc number? how far the path bows out sideways on the way, in pixels, for a piece that should swing across rather than slide straight. 
---@field anchorOrbitX number? a centre to swing round, in place of travelling across. 
---@field anchorOrbitY number? a centre to swing round, in place of travelling across. The distances and angles at both ends fall out of the offsets already named, so an orbit is those same two endpoints read a different way round, and ends at unequal distances from the centre spiral between them.
---@field anchorOrbitTurns number? whole turns added to that swing, for a path that comes back on itself. 
---@field color {a: integer, r: integer, g: integer, b: integer}?
---@field delayTicks number? how long it stays as it was after the aim is taken
---@field openingTicks number? how long it takes to come out; 0 puts it there in one step
---@field openingStartScale number? the fraction of its full scale it comes in at, for what scales
---@field easePower number? how the movement is spread: 1 runs straight, higher lands softer
---@field closingDelayTicks number? how long it stays out after the aim is dropped, which staggers the way out
---@field closingTicks number? how long it takes to go back; the opening's own when not set
---@field crosshairScale number? @whole HUD tags only
---@field crosshairs table<integer, table<integer, AdsHudSettings>>? @whole HUD tags only

---@type AdsHudSettings
local defaultHudElement = {
    delayTicks = 3,
    openingTicks = 7,
    openingStartScale = 0.20,
    closingTicks = 2,
    easePower = 3
}

local readoutDelayTicks = 7

---@class AdsReadoutSettings
---@field text string|nil @what it says, when its string is fixed rather than read off the weapon
---@field format string|nil @formats the number it reads off the weapon
---@field batteryFormat string|nil @what the ammunition readout says on a weapon that runs off a battery
---@field position {x: integer, y: integer}|nil
---@field justification "left"|"right"|"center"|nil
---@field fontPath string|nil
---@field color {a: number, r: number, g: number, b: number}|nil
---@field delayTicks number|nil
---@field closingDelayTicks number|nil
---@type table<string, AdsReadoutSettings>
local defaultReadouts = {
    zoom = {
        position = {x = -134, y = -7},
        justification = "center",
        fontPath = defaultFontPath,
        color = defaultTextColor,
        delayTicks = readoutDelayTicks,
        closingDelayTicks = 1
    },
    ammo = {
        format = "%d",
        batteryFormat = "%d%%",
        position = {x = -291, y = -7},
        justification = "right",
        fontPath = defaultFontPath,
        color = defaultTextColor,
        delayTicks = readoutDelayTicks,
        closingDelayTicks = 1
    },
    reserve = {
        format = "/ %d",
        position = {x = 558, y = -6},
        justification = "left",
        fontPath = smallFontPath,
        color = defaultTextColor,
        delayTicks = readoutDelayTicks,
        closingDelayTicks = 1
    }
}

---@type AdsHudSettings
local ammoMaskOverlay = {
    widthScale = 2.25,
    heightScale = 2.25,
    openingStartScale = 3,
    delayTicks = 1
}

---@type table<string, AdsHudSettings>
local plasmaAdsHudElements = {
    [path.weaponHudInterface.ads.plasmaRifle] = {
        crosshairs = {
            [0] = {[0] = {widthScale = 0.325, heightScale = 0.325}},
            [1] = {[0] = ammoMaskOverlay}
        }
    }
}

-- Every weapon that aims down sights, keyed by tag path the same way hudDynamicCrosshair keys its crosshair animations.
---@class AdsWeaponConfig
---@field fieldOfView number
---@field overheatedHeat number|nil
---@field autoaimRange number|nil
---@field hudElements table<string, AdsHudSettings>|nil
---@field readouts table<string, AdsReadoutSettings>|nil
---@type table<string, AdsWeaponConfig>
local adsWeapons = {
    -- AssaultRifleMA38
    [path.weapon.human.assaultRifleMa38] = {
        fieldOfView = 56,
        autoaimRange = 30,
        hudElements = {
            [path.weaponHudInterface.ads.assaultRifleMa38] = {
                crosshairs = {
                    [0] = { --# Crosshair 1
                        [0] = { -- # Overlay 1
                            widthScale = 0.375,
                            heightScale = 0.375,
                            color = {a = 180, r = 255, g = 255, b = 255}
                        }
                    },
                    [1] = {[0] = ammoMaskOverlay}
                }
            },
        },
        readouts = {
            zoom = {
                text = "1.40x"
            },
            ammo = {},
            reserve = {}
        }
    },
    -- NeedlerT54C
    [path.weapon.covenant.needler] = {
        fieldOfView = 60,
        autoaimRange = 30,
        hudElements = {
            [path.weaponHudInterface.ads.needler] = {
                crosshairs = {
                    [0] = {
                        [0] = {
                            anchorOffsetX = 100,
                            widthScale = 0.275,
                            heightScale = 0.275,
                            color = {a = 180, r = 255, g = 156, b = 255}
                        },
                        [1] = {
                            anchorOffsetX = -100,
                            widthScale = 0.275,
                            heightScale = 0.275,
                            color = {a = 180, r = 255, g = 156, b = 255}
                        }
                    },
                    [1] = {
                        [0] = ammoMaskOverlay
                    }
                }
            }
        },
        readouts = {
            zoom = {
                text = "1.30x",
                position = {
                    x = -100,
                    y = 0
                },
                color = covenantColor
            },
            ammo = {
                position = {
                    x = -320,
                    y = 0
                },
                color = covenantColor,
            },
            reserve = {
                position = {
                    x = 530,
                    y = 1
                },
                color = covenantColor
            }
        }
    },
    -- Plasma Rifle
    [path.weapon.covenant.plasmaRifle] = {
        overheatedHeat = 1,
        fieldOfView = 56,
        hudElements = plasmaAdsHudElements,
        readouts = {
            zoom = {
                text = "1.40x",
                position = {
                    x = -130,
                    y = 0
                },
                color = covenantColor
            },
            ammo = {
                justification = "center",
                position = {
                    x = 130,
                    y = 0
                },
                color = covenantColor
            }
        }
    },
    -- Plasma Pistol
    [path.weapon.covenant.plasmaPistol] = {
        overheatedHeat = 1,
        autoaimRange = 27,
        fieldOfView = 60,
        hudElements = plasmaAdsHudElements,
        readouts = {
            zoom = {
                text = "1.30x",
                position = {
                    x = -130,
                    y = 0
                },
                color = covenantColor
            },
            ammo = {
                justification = "center",
                position = {
                    x = 130,
                    y = 0
                },
                color = covenantColor
            }
        }
    },
    -- Disruptor
    [path.weapon.covenant.disruptor] = {
        fieldOfView = 60,
        hudElements = {
            [path.weaponHudInterface.ads.needler] = {
                crosshairs = {
                    [0] = {
                        [0] = {
                            widthScale = 0.375,
                            heightScale = 0.375
                        }
                    },
                    [1] = {
                        [0] = ammoMaskOverlay
                    }
                }
            },
        },
        readouts = {
            zoom = {
                text = "1.30x",
                position = {
                    x = -140,
                    y = -4
                },
                color = covenantColor
            },
            ammo = {
                position = {
                    x = -290,
                    y = -4
                },
                color = covenantColor
            },
            reserve = {
                position = {
                    x = 560,
                    y = -3
                },
                color = covenantColor
            }
        }
    },
    -- SAW
    [path.weapon.human.saw] = {
        fieldOfView = 56,
        hudElements = {
            [path.weaponHudInterface.ads.saw] = {
                crosshairs = {
                    [0] = {
                        [0] = {
                            widthScale = 0.25,
                            heightScale = 0.25,
                            color = {a = 180, r = 255, g = 255, b = 255} --#FFFFFF
                        },
                        [1] = {
                            widthScale = 1,
                            heightScale = 1,
                            color = {a = 40, r = 0, g = 67, b = 125}
                        },
                        [2] = {
                            widthScale = 1,
                            heightScale = 1,
                            color = {a = 40, r = 80, g = 80, b = 80}
                        }
                    },
                    [1] = {
                        [0] = ammoMaskOverlay
                    }
                }
            },
        },
        readouts = {
            zoom = {
                text = "1.40x",
                position = {
                    x = -45,
                    y = 70
                }
            },
            ammo = {
                position = {
                    x = -382,
                    y = 70
                }
            },
            reserve = {
                position = {
                    x = 468,
                    y = 71
                }
            }
        }
    },
    [path.weapon.human.shotgunM90] = {
        fieldOfView = 57,
        autoaimRange = 30,
        hudElements = {
            [path.weaponHudInterface.ads.shotgunM90] = {
                crosshairs = {
                    [0] = {
                        [0] = {
                            widthScale = 0.25,
                            heightScale = 0.25,
                            anchorOffsetX = 47,
                            anchorOffsetY = 27,
                            color = {a = 180, r = 255, g = 255, b = 255} --#FFFFFF
                        },
                    },
                    [1] = { -- Cage Mid
                        [0] = {
                            widthScale = 0.45,
                            heightScale = 0.45,
                            anchorOffsetX = 67,
                            anchorOffsetY = 60,
                            color = {a = 40, r = 15, g = 50, b = 70}
                        }
                    },
                    [2] = { -- Cage Outer
                        [0] = {
                            widthScale = 0.6,
                            heightScale = 0.6,
                            anchorOffsetX = 67,
                            anchorOffsetY = 90,
                            color = {a = 40, r = 25, g = 33, b = 40}
                        }
                    },
                    [3] = { -- Distance arrow
                        [0] = {
                            delayTicks = 8,
                            widthScale = 0.25,
                            heightScale = 0.25,
                            anchorOffsetX = -62,
                            anchorOffsetY = 44,
                            -- Swings across instead of sliding straight. The run from the tag's own
                            -- (-37, 62) to here is about 31 pixels, so this bows it out by a good
                            -- third of that; negate it to make the swing go the other way round.
                            anchorOffsetArc = -3,
                            color = {a = 50, r = 200, g = 200, b = 200} --#FFFFFF
                        }
                    },
                    [4] = {
                        [0] = ammoMaskOverlay
                    }
                }
            }
        },
        readouts = {
            zoom = {
                delayTicks = 8,
                text = "1.40x",
                position = {
                    x = -74,
                    y = -9
                }
            },
            ammo = {
                delayTicks = 8,
                justification = "center",
                position = {
                    x = 116,
                    y = 37
                }
            },
        }
    }
}

--------------------------------------------------------------------------------------------------
-- End of configuration
--------------------------------------------------------------------------------------------------

-- Ticks are what the settings are written in, milliseconds are what the clock hands back.
local tickMilliseconds = utils.ticksToMillisecs(1)

-- Whether the aim is up, and which weapon it was taken with. The weapon is held by object and not
-- by tag, so swapping between two of the same kind counts as a swap too.
local isAimingDownSights = false
local aimingWeaponHandleValue = nil

-- What is on screen right now, which is not the same as what the aim wants: something on its way
-- out is still on screen, and stays on these lists until it is all the way back.
---@type ShownAdsHudElement[]
local shownHudElements = {}
---@type ShownAdsReadout[]
local shownReadouts = {}
-- The weapon the readouts on show were read off. Unlike the HUD elements, which are keyed by tag
-- and can close while another weapon's open, there is one readout of each name on screen, so
-- swapping weapons clears them outright rather than leaving two counters over each other.
---@type string|nil
local shownReadoutsWeaponPath = nil

--- How far along something is on its way in or out, in the one shape everything here uses
---@class AdsPhase
---@field settings table @whichever settings it was resolved from, for its timings
---@field progress number @0 for as it was before the aim, 1 for all the way out
---@field isClosing boolean @which way that progress is running
---@field phaseTicks number @how long it has been running that way, for the delay to measure

--- Start something off as it was before the aim, on its way in
---@param settings table
---@return AdsPhase
local function newPhase(settings)
    return {settings = settings, progress = 0, isClosing = false, phaseTicks = 0}
end

--- Turn a phase around, if it is not already running that way
---@param phase AdsPhase
---@param isClosing boolean
local function setPhaseClosing(phase, isClosing)
    -- Guarded rather than assigned outright: the aim says what it wants on every tick, and a phase
    -- reset that often would never get anywhere.
    if phase.isClosing ~= isClosing then
        phase.isClosing = isClosing
        phase.phaseTicks = 0
    end
end

--- Move a phase along by a step of time, and say when it is all the way back
---@param phase AdsPhase
---@param elapsedTicks number @fractional: frames do not fall on tick boundaries
---@return boolean isClosed
local function advancePhase(phase, elapsedTicks)
    local settings = phase.settings
    local isClosing = phase.isClosing
    phase.phaseTicks = phase.phaseTicks + elapsedTicks
    local delayTicks
    if isClosing then
        delayTicks = settings.closingDelayTicks or 0
    else
        delayTicks = settings.delayTicks or 0
    end
    -- The opening delay holds the first appearance back, so something caught halfway out and asked
    -- back does not sit through it: it is already on screen, and freezing it where it got to would
    -- read as a hitch rather than as a delay.
    if phase.phaseTicks < delayTicks and (isClosing or phase.progress == 0) then
        return false
    end
    local durationTicks
    if isClosing then
        -- Silence about closing means closing the way it opened, which is what makes the way out
        -- the way in backwards without anything having to say so.
        durationTicks = settings.closingTicks or settings.openingTicks or 0
    else
        durationTicks = settings.openingTicks or 0
    end
    local step = 1
    if durationTicks > 0 then
        step = elapsedTicks / durationTicks
    end
    local progress = phase.progress + (isClosing and -step or step)
    if progress > 1 then
        progress = 1
    elseif progress < 0 then
        progress = 0
    end
    phase.progress = progress
    return isClosing and progress <= 0
end

--- How far out a phase reads once its easing is taken into account, 0 to 1
---@param phase AdsPhase
---@return number
local function getPhaseEase(phase)
    local easePower = phase.settings.easePower or 1
    -- Eased out rather than run straight, so a reticle flies open and settles into its scale instead
    -- of stopping dead. It is a function of progress alone and never of which way progress is
    -- running, which is what makes the closing the opening backwards and, more usefully, what keeps
    -- a piece caught halfway and turned around from jumping: whatever it read on the way out it
    -- reads again on the way back, off the same curve at the same progress.
    return 1 - (1 - phase.progress) ^ easePower
end

--- One table's fields laid over another's, so what a weapon leaves out comes from the defaults
---@param defaults table
---@param settings table|nil
---@return table
local function resolveSettings(defaults, settings)
    -- Worked out once when the aim is taken rather than on every step: none of it changes while the
    -- aim lasts, and what does change, the string a readout says, is not in here.
    local resolved = {}
    for field, value in pairs(defaults) do
        resolved[field] = value
    end
    if settings then
        for field, value in pairs(settings) do
            resolved[field] = value
        end
    end
    return resolved
end

---@param tagPath string
---@return WeaponHudInterface|nil
local function getHudTagData(tagPath)
    -- Looked up on each write instead of held as an upvalue: tag data lives in the loaded map, so a
    -- cached view would dangle the moment the map changes.
    local tagHandle = engine.tag.lookupTag(tagPath, "weapon_hud_interface")
    if not tagHandle then
        balltze.logger.error("ADS HUD tag does not exist: {}", tagPath)
        return nil
    end
    return engine.tag.getTagData(tagHandle, "weapon_hud_interface")
end

--- What is on screen for one of a weapon's HUD tags
---@class ShownAdsHudElement
---@field path string
---@field settings AdsHudSettings
---@field phase AdsPhase @the element's own, and the one its pieces follow unless they said otherwise
---@field overlayPhases table<integer, table<integer, AdsPhase>>|nil @[crosshair][overlay], from zero
---@field phases AdsPhase[] @every phase above, the element's first
---@field originalValues {crosshairOverlays: {widthScale: number, heightScale: number, anchorOffsetX: integer, anchorOffsetY: integer, color: integer}[]|nil}|nil

--- The four channels Guerilla shows, as the one number the tag actually keeps them in
---@param color {a: integer, r: integer, g: integer, b: integer}
---@return integer
local function packColor(color)
    -- Arithmetic rather than shifts, so this does not depend on which Lua it is running under: the
    -- server side of this project still goes through compat53.
    return color.a * 0x1000000 + color.r * 0x10000 + color.g * 0x100 + color.b
end

--- Where a piece sits partway between where the tag put it and where the aim wants it
---
--- Three shapes of path, narrowest first. An orbit centre swings the piece round that point, and is
--- the only one of the three that can bring it back to where it started. An arc bows the straight
--- path out sideways. Neither, and it runs straight. The shape is worked out against the same eased
--- reading the piece travels by, and not against the raw progress, so easing changes how fast the
--- piece runs along its path and never what path it runs along.
---@param settings AdsHudSettings @the piece's own, for the shape of its path
---@param fromX integer
---@param fromY integer
---@param toX integer
---@param toY integer
---@param ease number @0 at the tag's own offset, 1 at the aim's
---@return integer x
---@return integer y
local function getTravelledOffset(settings, fromX, fromY, toX, toY, ease)
    local x, y
    local centreX = settings.anchorOrbitX
    local centreY = settings.anchorOrbitY
    if centreX and centreY then
        -- The two ends said as an angle and a distance from the centre rather than as two points.
        -- Nothing is configured twice over by this: both radii and both angles fall out of the
        -- offsets already named, so an orbit is the same two endpoints read a different way round.
        local fromAngle = atan2(fromY - centreY, fromX - centreX)
        local toAngle = atan2(toY - centreY, toX - centreX)
        local fromRadius = sqrt((fromX - centreX) ^ 2 + (fromY - centreY) ^ 2)
        local toRadius = sqrt((toX - centreX) ^ 2 + (toY - centreY) ^ 2)
        -- Brought inside half a turn either way, so the piece takes the short way round rather than
        -- the long one when the two angles straddle the wrap. Lua's modulo carries the sign of its
        -- right hand side, which lands this in [-pi, pi) with no branching.
        local sweep = (toAngle - fromAngle + pi) % (2 * pi) - pi
        -- Whole turns on top of that. This is what makes a piece that finishes where it began go all
        -- the way round instead of standing still, and the sign of it says which way round.
        sweep = sweep + (settings.anchorOrbitTurns or 0) * 2 * pi
        -- The radius is walked at the same time as the angle, so ends at unequal distances from the
        -- centre spiral between them rather than snapping to a circle and stepping off it.
        local angle = fromAngle + sweep * ease
        local radius = fromRadius + (toRadius - fromRadius) * ease
        x = centreX + cos(angle) * radius
        y = centreY + sin(angle) * radius
    else
        local deltaX = toX - fromX
        local deltaY = toY - fromY
        x = fromX + deltaX * ease
        y = fromY + deltaY * ease
        local arc = settings.anchorOffsetArc
        if arc then
            local length = sqrt(deltaX * deltaX + deltaY * deltaY)
            -- A piece going nowhere has no sideways to bow to, and the length is what would be
            -- divided by to find it. An orbit is what moves a piece that ends where it began.
            if length > 0 then
                -- A sine of the distance travelled, which is nothing at both ends and one in the
                -- middle: however far the path bows, it cannot move where the piece starts or lands.
                local bow = arc * sin(pi * ease)
                -- Pushed along the perpendicular of the straight path, which for a step of (dx, dy)
                -- is (-dy, dx). Sending the bow sideways rather than up or down is what keeps it
                -- reading as one swing whichever way across the screen the piece happens to go.
                x = x - deltaY / length * bow
                y = y + deltaX / length * bow
            end
        end
    end
    -- Rounded because the tag keeps these as whole pixels.
    return floor(x + 0.5), floor(y + 0.5)
end

--- Put a HUD's pieces where the aim wants them, keeping what was there the first time
---@param shownHudElement ShownAdsHudElement
local function writeHudElements(shownHudElement)
    local settings = shownHudElement.settings
    local hudTagData = getHudTagData(shownHudElement.path)
    if not hudTagData then
        return
    end
    ---@cast hudTagData WeaponHudInterface
    local crosshairSettings = settings.crosshairs
    -- A HUD reaches for the crosshairs if it has anything at all to say about them, the one scale
    -- for all of them or a single overlay asking for something of its own. A tag block with nothing
    -- in it comes back as nil rather than as an array of length zero, and a HUD that carries only
    -- numbers is exactly that, so the block is checked before it is walked.
    local crosshairs = (settings.crosshairScale or crosshairSettings) and hudTagData.crosshairs
    local originalValues = shownHudElement.originalValues
    if not originalValues then
        originalValues = {}
        shownHudElement.originalValues = originalValues
    end
    local elementPhase = shownHudElement.phase
    local overlayPhases = shownHudElement.overlayPhases
    if crosshairs then
        local originalOverlays = originalValues.crosshairOverlays
        if not originalOverlays then
            originalOverlays = {}
            originalValues.crosshairOverlays = originalOverlays
        end
        -- Walked nested rather than flattened, since the settings are nested the same way, and the
        -- snapshot is kept against a running count so the restore lines its own walk up with it.
        local overlayCount = 0
        for crosshairIndex = 1, #crosshairs do
            local overlays = crosshairs[crosshairIndex].crosshairOverlays
            -- Both keys are counted from zero, the way Guerilla numbers them, so what is typed into
            -- the table is what is read off the editor rather than off by one from it.
            local crosshairKey = crosshairIndex - 1
            local overlaySettings = crosshairSettings and crosshairSettings[crosshairKey]
            local crosshairPhases = overlayPhases and overlayPhases[crosshairKey]
            if overlays then
                for overlayIndex = 1, #overlays do
                    local overlayKey = overlayIndex - 1
                    overlayCount = overlayCount + 1
                    -- Its own clock where it asked for a timing of its own, the element's where it
                    -- did not. This is the whole of what lets one piece arrive after the rest.
                    local phase = crosshairPhases and crosshairPhases[overlayKey] or elementPhase
                    -- A piece still inside its delay is passed over entirely rather than written at
                    -- the size it starts from, which is what keeps it off the screen until its turn.
                    if phase.progress > 0 then
                        local overlay = overlays[overlayIndex]
                        local colors = overlay.defaultColor.parameters
                        -- Snapshotted the first time this one piece is about to be written, and not
                        -- when the aim was taken: the pieces no longer all start together, and one
                        -- still waiting its turn has not been touched and has nothing to put back.
                        -- Past that first write what the tag holds is this module's own doing, and
                        -- taking it again would save the opening size as if it were the authored one.
                        local original = originalOverlays[overlayCount]
                        if not original then
                            local anchorOffset = overlay.anchorOffset
                            original = {
                                widthScale = overlay.widthScale,
                                heightScale = overlay.heightScale,
                                anchorOffsetX = anchorOffset.x,
                                anchorOffsetY = anchorOffset.y,
                                color = colors.defaultColor
                            }
                            originalOverlays[overlayCount] = original
                        end
                        local overlaySetting = overlaySettings and overlaySettings[overlayKey]
                        local ease = getPhaseEase(phase)
                        -- Off its own clock's settings, so a piece that named a start scale of its
                        -- own grows from there rather than from the one the element handed round.
                        local startScale = phase.settings.openingStartScale or 1
                        local grown = startScale + (1 - startScale) * ease
                        -- A piece with no size named anywhere, its own or the element's, keeps the
                        -- one the tag gave it. That is what lets it be moved or recoloured without
                        -- being resized, and the two axes are asked for apart so one can be left be.
                        local widthScale = overlaySetting and overlaySetting.widthScale or
                                               settings.crosshairScale
                        if widthScale then
                            overlay.widthScale = widthScale * grown
                        end
                        local heightScale = overlaySetting and overlaySetting.heightScale or
                                                settings.crosshairScale
                        if heightScale then
                            overlay.heightScale = heightScale * grown
                        end
                        if overlaySetting then
                            -- Walked from where the tag put the piece to where the aim wants it,
                            -- rather than set down there on the first frame. There is no start of
                            -- its own to name the way the scale has one: the tag's own offset is
                            -- where the piece is sitting when the aim is taken, so starting anywhere
                            -- else would mean jumping before moving.
                            local targetX = overlaySetting.anchorOffsetX
                            local targetY = overlaySetting.anchorOffsetY
                            -- An orbit counts as travelling on its own, without either end being
                            -- named: a piece that goes right round and comes back has nowhere else
                            -- to be, and asking for a destination it already sits at is a strange
                            -- way to have to say so.
                            if targetX or targetY or overlaySetting.anchorOrbitX then
                                -- Both axes travel together even when only one of them was named,
                                -- since a path has two of them and neither an arc nor an orbit
                                -- keeps to one. The one left unnamed finishes where it started.
                                local fromX = original.anchorOffsetX
                                local fromY = original.anchorOffsetY
                                local anchorOffset = overlay.anchorOffset
                                anchorOffset.x, anchorOffset.y =
                                    getTravelledOffset(overlaySetting, fromX, fromY,
                                                       targetX or fromX, targetY or fromY, ease)
                            end
                            if overlaySetting.color then
                                colors.defaultColor = packColor(overlaySetting.color)
                            end
                        end
                    end
                end
            end
        end
    end
end

--- Put back what a HUD held before its ADS elements were written
---@param shownHudElement ShownAdsHudElement
local function restoreHudElements(shownHudElement)
    -- There is nothing to put back when the aim is dropped before the delay runs out: the tag was
    -- never written, so it still holds the values it was authored with.
    local originalValues = shownHudElement.originalValues
    if not originalValues then
        return
    end
    local hudTagData = getHudTagData(shownHudElement.path)
    if not hudTagData then
        return
    end
    ---@cast hudTagData WeaponHudInterface
    -- Each element is only put back if the snapshot has something for it, so a tag that came back
    -- with a different element count restores what it can instead of erroring out.
    local originalOverlays = originalValues.crosshairOverlays
    local crosshairs = originalOverlays and hudTagData.crosshairs
    if crosshairs and originalOverlays then
        -- The same nested walk the writing does, in the same order, which is what makes the running
        -- count line up with the snapshot without either side having to know the shape twice.
        local overlayCount = 0
        for crosshairIndex = 1, #crosshairs do
            local overlays = crosshairs[crosshairIndex].crosshairOverlays
            if overlays then
                for overlayIndex = 1, #overlays do
                    overlayCount = overlayCount + 1
                    local original = originalOverlays[overlayCount]
                    if original then
                        local overlay = overlays[overlayIndex]
                        overlay.widthScale = original.widthScale
                        overlay.heightScale = original.heightScale
                        overlay.anchorOffset.x = original.anchorOffsetX
                        overlay.anchorOffset.y = original.anchorOffsetY
                        overlay.defaultColor.parameters.defaultColor = original.color
                    end
                end
            end
        end
    end
end

--- Move one of a weapon's HUD tags along, and say when it is back to what it was authored as
---@param shownHudElement ShownAdsHudElement
---@param elapsedTicks number
---@return boolean isClosed
local function updateShownHudElement(shownHudElement, elapsedTicks)
    local phases = shownHudElement.phases
    local isClosed = true
    local isMoved = false
    for index = 1, #phases do
        local phase = phases[index]
        local progressBefore = phase.progress
        -- Every clock is stepped, and the HUD only counts as finished once the last of them is: a
        -- piece with a closing delay of its own is still on screen after the rest have gone.
        if not advancePhase(phase, elapsedTicks) then
            isClosed = false
        end
        if phase.progress ~= progressBefore then
            isMoved = true
        end
    end
    if isClosed then
        -- The authored values are put back here rather than written from a phase: "as it was" is the
        -- one state that is not a size this could work out.
        restoreHudElements(shownHudElement)
        return true
    end
    -- Nothing moved, so what the tag holds is already what this frame would put there. Two things
    -- read as nothing moving: a HUD all the way out with the aim still up, which is most of an aim,
    -- and one whose pieces are all still inside their delays, which is what lets an aim taken and
    -- dropped inside that window never reach the screen at all.
    if isMoved then
        writeHudElements(shownHudElement)
    end
    return false
end

--- What is on screen for one of a weapon's readouts
---@class ShownAdsReadout
---@field key string
---@field settings AdsReadoutSettings
---@field phase AdsPhase
---@field wantedText string|nil @what it should say, read off the weapon once a tick
---@field handle InterfaceText|nil @nil even while up, on builds where addText hands nothing back
---@field isUp boolean @tracked apart from the handle, for that same reason
---@field text string|nil @what it says right now
--- Options for a readout. Built per text rather than kept around: they are fixed at creation time
--- anyway, and the font handle belongs to the loaded map, so it is looked up on the spot.
---@param settings AdsReadoutSettings
---@return InterfaceTextOptions
local function getTextOptions(settings)
    local fontPath = settings.fontPath or defaultFontPath
    -- A font that cannot be found is not worth dropping the readout over: addText falls back to the
    -- globals terminal font on a nil handle, so a wrong path costs the look and nothing else. Worth
    -- saying out loud though, since that fallback is easy to mistake for the tag looking wrong.
    local font = engine.tag.lookupTag(fontPath, "vector_font")
    if not font then
        balltze.logger.warning("ADS readout font does not exist: {}", fontPath)
    end
    return {
        color = settings.color or defaultTextColor,
        font = font,
        shadow = false,
        layer = "hud",
        anchor = "center",
        justification = settings.justification
    }
end

--- Take a readout off the screen
---@param shownReadout ShownAdsReadout
local function removeReadoutText(shownReadout)
    if shownReadout.handle and shownReadout.handle.remove then
        shownReadout.handle:remove()
    end
    shownReadout.handle = nil
    shownReadout.isUp = false
    shownReadout.text = nil
end

--- Put a readout on screen, move it along to what it should say now, or take it away
---@param shownReadout ShownAdsReadout
---@param text string|nil @nil takes the readout away
local function setReadoutText(shownReadout, text)
    if not text then
        removeReadoutText(shownReadout)
        return
    end
    if shownReadout.isUp then
        if text == shownReadout.text then
            return
        end
        if shownReadout.handle and shownReadout.handle.setText then
            shownReadout.handle:setText(text)
            shownReadout.text = text
            return
        end
        -- Nothing to rewrite the string with on this build, so the readout is replaced by one that
        -- reads right. Rare enough to be worth the two calls, and it keeps a counter from freezing
        -- on the round count it was added with.
        removeReadoutText(shownReadout)
    end
    local settings = shownReadout.settings
    -- Every readout has been through resolveSettings, so it carries a position; the middle of the
    -- screen only stands in for one that somehow did not.
    local position = settings.position or {x = 0, y = 0}
    shownReadout.handle = engine.interface.addText(text, position.x, position.y, getTextOptions(settings))
    shownReadout.isUp = true
    shownReadout.text = text
end

--- Move one of a weapon's readouts along, and say when it is off the screen for good
---@param shownReadout ShownAdsReadout
---@param elapsedTicks number
---@return boolean isClosed
local function updateShownReadout(shownReadout, elapsedTicks)
    local isClosed = advancePhase(shownReadout.phase, elapsedTicks)
    if isClosed or shownReadout.phase.progress <= 0 then
        removeReadoutText(shownReadout)
        return isClosed
    end
    -- Whatever it was reading is left alone once it is on its way out: a counter that ticked over
    -- while leaving would draw the eye to what is going. Nothing refreshes wantedText past that
    -- point either, since the weapon it was read off is not in hand anymore.
    setReadoutText(shownReadout, shownReadout.wantedText)
    return false
end

--- The magazine a weapon feeds from, or nil when it runs off a battery instead
---@param weaponObject WeaponObject
---@return WeaponObjectMagazine|nil
local function getWeaponMagazine(weaponObject)
    local weaponTagData = engine.tag.getTagData(weaponObject.tagHandle, "weapon")
    ---@cast weaponTagData Weapon
    local magazines = weaponTagData and weaponTagData.magazines
    -- An empty tag block comes back as nil rather than as an array of length zero, so a weapon with
    -- no magazines at all is caught here. Carrying the block is not the same as feeding from it
    -- though: a battery weapon can still have one with no rounds declared in it, and taking that
    -- for an ammunition weapon is what shows a plasma rifle at 0 rounds instead of its charge.
    local tagMagazine = magazines and magazines[1]
    if not tagMagazine or tagMagazine.roundsLoadedMaximum <= 0 then
        return nil
    end
    return weaponObject.magazines[1]
end

--- Read a weapon for what its readouts should say. Which one it gets on the right is not something
--- the settings answer: a weapon that feeds from magazines shows what is loaded and what is in
--- reserve, one that runs off a battery shows the charge, and the weapon tag says which it is.
---@param weaponObject WeaponObject
local function readWeaponReadouts(weaponObject)
    local magazine = getWeaponMagazine(weaponObject)
    for index = 1, #shownReadouts do
        local shownReadout = shownReadouts[index]
        local settings = shownReadout.settings
        if shownReadout.key == "ammo" then
            if magazine then
                shownReadout.wantedText = settings.format:format(magazine.roundsLoaded)
            else
                -- Age is the wear on the battery and not the charge left in it: a plasma rifle the
                -- game's own HUD reads at 77% comes back with an age of 0.225, so what is left is
                -- the other side of it.
                --
                -- Cut off rather than rounded, because that is what the HUD in the corner does: a
                -- weapon a shot into its battery sits at 99.7 left, which that HUD shows as 99 and
                -- rounding would show as 100. A round of plasma is worth a fraction of a percent,
                -- so the two readings would disagree for the shot or two it takes the rounding to
                -- catch up, on the same number, on the same screen. Held inside 0 and 100 as well,
                -- in case the engine lets age wander past either end.
                local batteryPercent = floor((1 - weaponObject.age) * 100)
                if batteryPercent > 100 then
                    batteryPercent = 100
                elseif batteryPercent < 0 then
                    batteryPercent = 0
                end
                shownReadout.wantedText = settings.batteryFormat:format(batteryPercent)
            end
        elseif shownReadout.key == "reserve" then
            -- roundsUnloaded is what the weapon still has to reload from, which is the reserve shown
            -- next to what is in the magazine. A battery weapon has nothing to say here at all.
            shownReadout.wantedText = magazine and settings.format:format(magazine.roundsUnloaded)
        else
            shownReadout.wantedText = settings.text
        end
    end
end

-- The settings a clock reads off itself, as against the ones that only say what a piece is written
-- to. A piece naming any of these is asking for a clock of its own; a piece naming none of them
-- follows the HUD element's, which is the common case and costs nothing.
local timingFields = {
    "delayTicks", "openingTicks", "openingStartScale", "easePower", "closingDelayTicks",
    "closingTicks"
}

--- A clock for each of a HUD element's pieces that asked for one, nested the way the settings are
---@param elementSettings AdsHudSettings @already resolved against the defaults
---@return table<integer, table<integer, AdsPhase>>|nil @nil when no piece asked for one
local function buildOverlayPhases(elementSettings)
    local crosshairSettings = elementSettings.crosshairs
    if not crosshairSettings then
        return nil
    end
    local overlayPhases = nil
    for crosshairKey, overlaySettings in pairs(crosshairSettings) do
        for overlayKey, settings in pairs(overlaySettings) do
            local isOwnClock = false
            for index = 1, #timingFields do
                if settings[timingFields[index]] ~= nil then
                    isOwnClock = true
                    break
                end
            end
            if isOwnClock then
                overlayPhases = overlayPhases or {}
                local crosshairPhases = overlayPhases[crosshairKey]
                if not crosshairPhases then
                    crosshairPhases = {}
                    overlayPhases[crosshairKey] = crosshairPhases
                end
                -- Laid over the element's own settings and not over the bare defaults, so a piece
                -- that names only a delay keeps the opening and the easing the element handed round
                -- rather than falling back past it to what this module ships with.
                crosshairPhases[overlayKey] = newPhase(resolveSettings(elementSettings, settings))
            end
        end
    end
    return overlayPhases
end

--- Say which weapon's ADS presentation should be out. What is not wanted anymore is not taken away
--- there and then, it is turned around and left to close, which is why nothing here writes to a tag
--- or to the screen. Called with nil, everything closes.
---@param weaponTagPath string|nil
---@param adsWeapon AdsWeaponConfig|nil
local function setShownWeapon(weaponTagPath, adsWeapon)
    local wantedHudElements = weaponTagPath and adsWeapon and adsWeapon.hudElements or {}
    local isHudElementShown = {}
    for index = 1, #shownHudElements do
        local shownHudElement = shownHudElements[index]
        isHudElementShown[shownHudElement.path] = true
        local isClosing = wantedHudElements[shownHudElement.path] == nil
        local phases = shownHudElement.phases
        for phaseIndex = 1, #phases do
            setPhaseClosing(phases[phaseIndex], isClosing)
        end
    end
    for tagPath, settings in pairs(wantedHudElements) do
        if not isHudElementShown[tagPath] then
            local resolved = resolveSettings(defaultHudElement, settings)
            local phase = newPhase(resolved)
            local shownHudElement = {
                path = tagPath,
                settings = resolved,
                phase = phase,
                overlayPhases = buildOverlayPhases(resolved),
                phases = {phase}
            }
            -- The element's clock first and the pieces' after it, all in the one list, so stepping
            -- and turning them around never has to walk the nesting the writing looks them up by.
            local overlayPhases = shownHudElement.overlayPhases
            if overlayPhases then
                local phases = shownHudElement.phases
                for _, crosshairPhases in pairs(overlayPhases) do
                    for _, overlayPhase in pairs(crosshairPhases) do
                        phases[#phases + 1] = overlayPhase
                    end
                end
            end
            shownHudElements[#shownHudElements + 1] = shownHudElement
        end
    end
    if weaponTagPath and weaponTagPath ~= shownReadoutsWeaponPath then
        -- A different weapon: whatever is still up belongs to the last one, and there is only one
        -- readout of each name to go round, so it goes at once rather than counting down in front
        -- of the new weapon's.
        for index = 1, #shownReadouts do
            removeReadoutText(shownReadouts[index])
        end
        shownReadouts = {}
        shownReadoutsWeaponPath = weaponTagPath
        for key, settings in pairs(adsWeapon and adsWeapon.readouts or {}) do
            if defaultReadouts[key] then
                local resolved = resolveSettings(defaultReadouts[key], settings)
                shownReadouts[#shownReadouts + 1] = {
                    key = key,
                    settings = resolved,
                    phase = newPhase(resolved),
                    isUp = false
                }
            end
        end
    end
    -- Turned around on every tick rather than on the transition, so aiming again inside the closing
    -- window takes the readouts back instead of letting them go and building them afresh.
    for index = 1, #shownReadouts do
        setPhaseClosing(shownReadouts[index].phase, weaponTagPath == nil)
    end
end

-- The weapon whose aim assist has been widened, and the reach its tag was authored with. One slot
-- and not a list the way the HUD elements are: the reach belongs to the weapon in hand, and there
-- is only ever one of those. The two are kept in the one table rather than side by side because
-- they only ever mean anything together: a path with nothing to hand back, or a reach with nothing
-- to hand it back to, is not a state this can be in.
---@type {path: string, autoaimRange: number}|nil
local shownAutoaim = nil

--- The weapon tag behind a path
---@param tagPath string
---@return Weapon|nil
local function getWeaponTagData(tagPath)
    -- Looked up on each write for the same reason the HUD tags are, and not held from the weapon in
    -- hand: what is written lives in the loaded map, and the reach has to be handed back after the
    -- weapon has been put down, when there is nothing left to read a handle off.
    local tagHandle = engine.tag.lookupTag(tagPath, "weapon")
    if not tagHandle then
        balltze.logger.error("ADS weapon tag does not exist: {}", tagPath)
        return nil
    end
    return engine.tag.getTagData(tagHandle, "weapon")
end

--- Widen the aim assist of the weapon being aimed with, and hand back what the tag was authored
--- with once it is not. Called with nil, or with a weapon that asks for no reach of its own,
--- whatever is out goes back.
---
--- The reach is written on the weapon tag rather than on the player, so it is worth saying where
--- that write lands. This runs inside each player's own game, on each player's own copy of the map,
--- and nothing written to a tag here crosses the network: the camera field of view is the same kind
--- of write and has never pulled anyone else's camera in. Widening the reach does widen it on every
--- one of that weapon in the map, the ones in other players' hands included, but only the local
--- player is ever aim assisted at all: the engine works that out in the player control path, which
--- keeps one entry per local player and so has exactly one. Everyone else's weapon on this screen is
--- moved by what the network says about it, not by aim assist.
---@param weaponTagPath string|nil
---@param adsWeapon AdsWeaponConfig|nil
local function setWeaponAutoaimRange(weaponTagPath, adsWeapon)
    -- Put on and taken off outright, where everything else here is eased: aim assist is felt rather
    -- than seen, and a reach that grew over a few ticks would read as the shot pulling on its own
    -- rather than as the sights coming up.
    ---@type number|nil
    local wantedRange = weaponTagPath and adsWeapon and adsWeapon.autoaimRange or nil
    local wantedPath = wantedRange and weaponTagPath or nil
    if wantedPath == (shownAutoaim and shownAutoaim.path) then
        return
    end
    if shownAutoaim then
        local weaponTagData = getWeaponTagData(shownAutoaim.path)
        if weaponTagData then
            weaponTagData.autoaimRange = shownAutoaim.autoaimRange
        end
        -- Let go of outside that guard, so a tag that cannot be reached anymore, which is what a map
        -- on its way out looks like, is written off rather than asked for again on every tick.
        shownAutoaim = nil
    end
    if wantedPath and wantedRange then
        local weaponTagData = getWeaponTagData(wantedPath)
        if weaponTagData then
            -- Read off the tag at the moment it is written over, so a weapon that is retuned hands
            -- back what it is authored with now and not what it was authored with once.
            shownAutoaim = {path = wantedPath, autoaimRange = weaponTagData.autoaimRange}
            weaponTagData.autoaimRange = wantedRange
        end
    end
end

-- The clock the movements are stepped by, and how far along it the last step left off.
--
-- It is never reset, which is the whole of why this reads right. The clock hands back whole
-- milliseconds, so a clock reset on every frame throws away whatever fraction of one it had not
-- counted yet, every frame. At a thousand frames a second a frame lasts under a millisecond and
-- every one of them reads as no time at all; at sixty, a sixteenth of each frame goes missing. The
-- movement then runs slower the faster the machine does, which is backwards. Read as a running total
-- and stepped by the difference between two readings, that same truncation costs a millisecond once
-- across the whole movement rather than most of one on every frame of it.
---@type BalltzeTimestamp|nil
local stepTimestamp = nil
local steppedMilliseconds = 0

-- The longest step that will be taken in one go. A stretch with no frames drawn in it, a menu or a
-- hitch, comes back as one large difference, and something halfway out would jump straight to the
-- end of itself rather than carry on. Four ticks is long enough never to be reached by a machine
-- drawing at all and short enough that the jump is not seen.
local maximumStepMilliseconds = 4 * (1000 / 30)

--- Move everything on screen along. Stepped once a frame and not once a tick: counting ticks would
--- quantise the movement into the 30 steps a second the game thinks in, which is what makes a four
--- tick opening read as four jumps however smoothly the game is running.
---
--- Public because this module does not subscribe to "frame" itself: Balltze keeps one listener per
--- event name, so a listener here would take down whatever else in the project drew on the frame,
--- and be taken down by whatever subscribed after it. multiplayer.frameSystems calls this instead.
function aimingDownSights.updateShownElements()
    local elapsedTicks = 0
    if stepTimestamp then
        local totalMilliseconds = stepTimestamp:getElapsedMilliseconds()
        local elapsedMilliseconds = totalMilliseconds - steppedMilliseconds
        steppedMilliseconds = totalMilliseconds
        if elapsedMilliseconds > maximumStepMilliseconds then
            elapsedMilliseconds = maximumStepMilliseconds
        end
        elapsedTicks = elapsedMilliseconds / tickMilliseconds
    else
        stepTimestamp = balltze.createTimestamp()
    end
    -- Walked backwards so anything that has finished closing can come off its list without shifting
    -- what is still waiting to be stepped.
    for index = #shownHudElements, 1, -1 do
        if updateShownHudElement(shownHudElements[index], elapsedTicks) then
            table.remove(shownHudElements, index)
        end
    end
    for index = #shownReadouts, 1, -1 do
        if updateShownReadout(shownReadouts[index], elapsedTicks) then
            table.remove(shownReadouts, index)
        end
    end
    if #shownReadouts == 0 then
        -- The last one has gone, so there is nothing left of that weapon to keep track of.
        shownReadoutsWeaponPath = nil
    end
end

-- What flips the aim. The press itself is watched by the shared input module rather than by a
-- listener of this module's own: Balltze keeps one listener per event name, so a second
-- "player_input" subscription anywhere in the project would take this one down, or be taken down by
-- it, depending on which of the two ran first.
local toggleAction = input.newAction {
    keyCode = toggleKeyCode,
    mouseButton = toggleMouseButton,
    gamepadButton = toggleGamepadButton,
    cancelInput = cancelToggleInput
}

--- The aiming settings of the weapon a biped is holding, if that weapon aims at all
---@param biped BipedObject
---@return AdsWeaponConfig|nil settings
---@return WeaponObject|nil weaponObject
---@return integer|nil weaponHandleValue @identifies the weapon in hand, not just its kind
---@return string|nil weaponTagPath @what kind of weapon it is, for the tables keyed by that
local function getHeldAdsWeapon(biped)
    -- v1 picked the held weapon by branching on blam's weaponSlot over the four
    -- firstWeaponObjectId..fourthWeaponObjectId fields; v2 exposes the slot as currentWeaponId and
    -- the four handles as one array, so the branch collapses into an index.
    local weaponHandle = biped.weapons[biped.currentWeaponId + 1]
    if not weaponHandle or weaponHandle:isNull() then
        return nil
    end
    local weaponObject = getObject(weaponHandle, "weapon")
    if not weaponObject then
        return nil
    end
    -- The path is read off the tag the weapon was built from rather than searched for in a list of
    -- known weapons. adsWeapons is keyed by path already, so the entry is one index away instead of
    -- a walk per tick, and there is no second list of weapons to keep in step with paths.lua: a
    -- weapon whose path is not in that table is simply one this module has nothing to say about.
    local weaponTagEntry = engine.tag.getTagEntry(weaponObject.tagHandle)
    if not weaponTagEntry then
        return nil
    end
    local weaponTagPath = weaponTagEntry.path
    return adsWeapons[weaponTagPath], weaponObject, weaponHandle.value, weaponTagPath
end

-- The vitals the player was last seen with, and the biped they belonged to. Damage is worked out by
-- watching these fall, since v2 has no event for a unit being hurt: what the aim has to react to is
-- the health going down, whatever sent it down. It is a fall of a certain size and not any fall at
-- all, since the shield drifts down on its own once an overshield is on it. The biped is kept
-- beside them because a respawn brings a different one, and reading a fresh biped's health against
-- a dead one's would be a drop that never happened.
local lastVitalsBipedHandleValue = nil
local lastHealth = 0
local lastShield = 0

--- What has happened to the player since the last tick
---@param biped BipedObject
---@param bipedHandleValue integer
---@return boolean isHurt
---@return boolean isDead
local function updatePlayerVitals(biped, bipedHandleValue)
    -- Read on every tick and not only while a weapon that aims is in hand: these are a comparison
    -- against the tick before, so a stretch of ticks that skipped it would leave the next one
    -- reading a drop that happened while nobody was watching.
    local vitals = biped.vitals
    local health = vitals.health
    local shield = vitals.shield
    local isSameBiped = lastVitalsBipedHandleValue == bipedHandleValue
    local isHurt = isSameBiped and
                       (lastHealth - health > damagingVitalsDrop or lastShield - shield >
                           damagingVitalsDrop)
    lastVitalsBipedHandleValue = bipedHandleValue
    lastHealth = health
    lastShield = shield
    return isHurt, health <= 0
end

--- Why the aim has to drop on its own this tick, or nil if it does not
---@param biped BipedObject
---@param weaponObject WeaponObject
---@param adsWeapon AdsWeaponConfig
---@return string|nil
local function getAimBreakingAction(biped, weaponObject, adsWeapon)
    -- All of it is read off the weapon and the unit's own state rather than off unitControlFlags:
    -- v2's flag list and blam's bit numbers run in opposite directions, so the name alone does not
    -- settle which bit `reload` or `grenade` really is. These say what is happening, not what key
    -- went down, which is what the aim should react to anyway.
    -- Counts down through the ready animation, the one a weapon plays as it is brought up on a
    -- swap or a pickup, and sits at zero the rest of the time. hudDynamicCrosshair reads it the
    -- same way to hold the reticle open while a weapon is coming up.
    if weaponObject.readyTicks > 0 then
        return "readying the weapon"
    end
    local magazine = weaponObject.magazines[1]
    if magazine and magazine.reloadTicksRemaining > 0 then
        return "reloading"
    end
    if biped.grenadeState ~= 0 then
        return "throwing a grenade"
    end
    -- Not off meleeState, the sibling of grenadeState above: it sits at zero right through a swing,
    -- as does the player's own actions.melee, while meleeTicks never leaves the twenties whether
    -- anything is happening or not. The unit control flag is the one that actually goes up with the
    -- swing, which is what a melee looks like from here.
    if biped.unitControlFlags.melee then
        return "meleeing"
    end
    -- Only for a weapon that says what its overheating looks like. Heat is not the same thing as
    -- overheating: every weapon carries some, which is what the dynamic crosshairs read it for, and
    -- taking a reading off one that never overheats would drop the aim on nothing.
    if adsWeapon.overheatedHeat and weaponObject.heat >= adsWeapon.overheatedHeat then
        return "overheated"
    end
    return nil
end

--- Work out what the aim is doing this tick and tell the rest of the module about it. Everything
--- that moves is stepped on frames, so what happens here is the deciding, not the drawing.
function aimingDownSights.adsSystem()
    -- Consumed whether or not it can be acted on, so a press made with a weapon that does not aim
    -- does not fire the moment one that does comes back up.
    local isToggleWanted = toggleAction:consume()

    local player = getPlayer()
    local biped = player and getObject(player.unitHandle, "biped")
    local bipedTagData = biped and engine.tag.getTagData(biped.tagHandle, "biped")
    if not (player and bipedTagData) then
        -- Nothing to aim with, on a dead player or between maps. The camera is left where it is,
        -- since the tag it is written on is not reachable either, but everything on screen is asked
        -- to close: the readouts especially, since nothing but this module would take them down.
        isAimingDownSights = false
        setShownWeapon(nil, nil)
        setWeaponAutoaimRange(nil, nil)
        return
    end
    ---@cast biped BipedObject

    local isHurt, isDead = updatePlayerVitals(biped, player.unitHandle.value)
    local adsWeapon, weaponObject, weaponHandleValue, weaponTagPath = getHeldAdsWeapon(biped)
    local aimBreakingAction
    -- What happened to the player comes before what is in hand: a hit has to take the aim down on
    -- the tick it lands, since leaving it up until they die would run the closing and its sound
    -- over the death, and the respawn would come back to a HUD still on its way out.
    if isDead then
        aimBreakingAction = "dead"
    elseif isHurt then
        aimBreakingAction = "taking damage"
    elseif not (adsWeapon and weaponObject) then
        aimBreakingAction = "weapon does not aim"
    elseif isAimingDownSights and aimingWeaponHandleValue ~= weaponHandleValue then
        -- Checked against the aim as it stands coming into this tick, before any toggle below is
        -- applied, so taking aim with a weapon just switched to is not mistaken for a swap.
        aimBreakingAction = "weapon changed"
    else
        aimBreakingAction = getAimBreakingAction(biped, weaponObject, adsWeapon)
    end

    local wasAimingDownSights = isAimingDownSights
    if isToggleWanted and adsWeapon then
        isAimingDownSights = not isAimingDownSights
    end
    if aimBreakingAction then
        isAimingDownSights = false
    end

    -- Resolved from what is true right now rather than from the transition, so all of it lands
    -- correctly whether the aim was just taken, just dropped, or dropped by a swap.
    local targetFieldOfView = hipFieldOfView
    if isAimingDownSights and adsWeapon then
        targetFieldOfView = adsWeapon.fieldOfView
    end
    setShownWeapon(isAimingDownSights and weaponTagPath or nil, adsWeapon)
    setWeaponAutoaimRange(isAimingDownSights and weaponTagPath or nil, adsWeapon)
    if isAimingDownSights and weaponObject then
        readWeaponReadouts(weaponObject)
    end

    if isAimingDownSights ~= wasAimingDownSights then
        aimingWeaponHandleValue = isAimingDownSights and weaponHandleValue or nil
        if isAimingDownSights then
            hsc.sound_impulse_start(path.sound.ui.hud.weapons.aimingDownSight.humanAdsIn, "none", 1)
        elseif not isDead then
            hsc.sound_impulse_start(path.sound.ui.hud.weapons.aimingDownSight.humanAdsOut, "none", 1)
        end
    end

    local currentFieldOfView = math.deg(bipedTagData.cameraFieldOfView)
    if math.abs(currentFieldOfView - targetFieldOfView) > fieldOfViewTolerance then
        bipedTagData.cameraFieldOfView = math.rad(targetFieldOfView)
    end
end

return aimingDownSights
