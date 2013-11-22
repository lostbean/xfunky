import qualified Data.Map as M

import XMonad
import XMonad.Config.Kde
-- to shift and float windows
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer

-- Hooks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Replace

-- Prompts, baby, prompts!
import XMonad.Prompt
import XMonad.Prompt.AppendFile

main = xmonad $ let conf = ewmh kde4Config {
                      -- replace ; ewmh
                      -- kde4Config {

                          modMask = mod4Mask -- use the Windows button as mod
                        , terminal = "urxvt.sh"

                        , manageHook = manageHook kdeConfig <+> myManageHook
                        , logHook = myLogHook

                      }
                      `additionalKeysP`
                      [ ("M-<Escape>", kill)
                      , ("M-<Space>", sendMessage NextLayout)
                      , ("M-r", refresh)
                      , ("M-j", windows W.focusDown)
                      , ("M-k", windows W.focusUp)
                      -- MPC keyboard control
                      , ("<XF86AudioPlay>", spawn "exec mpc toggle")
                      , ("<XF86AudioStop>", spawn "exec mpc stop")
                      , ("<XF86AudioPrev>", spawn "exec mpc prev")
                      , ("<XF86AudioNext>", spawn "exec mpc next")
                      -- My keyboard (a G15) also includes volume controls, but KDE already
                      -- manages some of them.
                      -- For reference, the keys are <XF86AudioMute> <XF86AudioRaiseVolume> <XF86AudioLowerVolume>

                      -- TODO: This will be replaced by a bashrun (but using zsh!) clone
                      , ("M-g", appendFilePrompt defaultXPConfig "/home/colin/notes/notes.txt")

                      -- mpc control via 'normal' keys
                      , ("M-a", submap . M.fromList $
                          [ ((0, xK_l),     spawn "mpc next")
                          , ((0, xK_h),     spawn "mpc prev")
                          , ((0, xK_z),     spawn "mpc random")
                          , ((0, xK_space), spawn "mpc toggle")
                          ])

                      -- Spawn the configured terminal
                      , ("M-<Return>", spawn $ terminal conf)
                      ] in conf

myManageHook = composeAll (

    -- Apps, etc to float & center
    [ className =? c <||> resource =? r <||> title =? t <||> isDialog --> doCenterFloat
    | c <- ["Wine", "Switch2", "quantum-Quantum"]
    , r <- ["Dialog", "Download"]
    , t <- ["Schriftart auswählen", "Choose a directory"]
    ] ++

    -- Separate float apps
    [ className =? "Plasma-desktop" --> doFloat -- For KDE
    , className =? "plasma-desktop" --> makeMaster <+> doFloat -- For KDE
    , className =? "kmix" --> doFloat -- For KDE
    , className =? "mplayer" --> doFloat

    -- Workspaces
    -- , className =? "Firefox"      --> makeMaster <+> moveTo 0
    -- , resource =? ""
    -- , title =? ""

    -- "Real" fullscreen
    , isFullscreen              --> doFullFloat
    , isDialog                  --> placeHook (inBounds (underMouse (0,0))) <+> makeMaster <+> doFloat
    ] )

    -- Default hooks:
    -- <+> insertPosition Below Newer
    -- <+> positionStoreManageHook
    <+> manageDocks
    <+> makeMaster

  where makeMaster = insertPosition Master Newer


myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.1
            >> updatePointer (Relative 0.5 0.5)

--- vim: set syn=haskell nospell:
