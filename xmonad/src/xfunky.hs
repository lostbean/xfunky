{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           XMonad                                 hiding ((|||))

import           XMonad.Actions.CycleWS
import           XMonad.Actions.NoBorders
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.UpdateFocus

import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Decoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowSwitcherDecoration

import           XMonad.Config.Kde

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.ToggleHook
import           XMonad.Hooks.WorkspaceByPos

import           XMonad.Util.Font
import           XMonad.Util.Replace
import           XMonad.Util.WindowProperties
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet       as S

import           Control.Monad         (liftM2)
import           Control.Applicative   ((<$>))
import           Data.List             (find, union)
import           Foreign.C.Types       (CInt)

import           Graphics.X11.Xinerama
import           System.Exit

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Map                 as M
import qualified DBus                     as D
import qualified DBus.Client              as D

main = do
  replace
  logpipe <- getDBusClient
  xmonad $ kde4Config
        { modMask            = modm
        , startupHook        = setWMName "LG3D" <+> startupHook kde4Config
        , manageHook         = manageHook kde4Config <+> myManageHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook logpipe
        , handleEventHook    = handleEventHook kde4Config <+>
                               docksEventHook             <+>
                               focusOnMouseMove           <+>
                               minimizeEventHook          <+>
                               fullscreenEventHook
        , workspaces         = myWorkspaces
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , focusFollowsMouse  = True
        , normalBorderColor  = "#556268"
        , focusedBorderColor = "#557824"
        , terminal           = "konsole"
        , borderWidth        = 2
        }

-- Main key
modm = mod4Mask

myWorkspaces = ["1:web","2:code","3:code","4:code","5:code","6:docs","7:others","8:gimp"]

-- ======================== Remove border from specific windows ==========================

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

data RemovableBorder w = RemovableBorder Property [w] deriving (Read, Show)

-- | Create a LayoutModifier to remove borders from windows with defined properties.
instance LayoutModifier RemovableBorder Window where
  unhook (RemovableBorder p w) = do
    ws <- allWithProperty p
    asks (borderWidth . config) >>= setBorders (union w ws)

  hook (RemovableBorder p w) = do
    ws <- allWithProperty p
    setBorders (union w ws) 0

noBorderOn :: (LayoutClass l Window)=> Property -> l Window
           -> ModifiedLayout RemovableBorder l Window
noBorderOn p = ModifiedLayout $ RemovableBorder p []

-- ===================================== Manage Hook =====================================

myManageHook :: ManageHook
myManageHook = composeAll $
    [ workspaceByPos
    , positionStoreManageHook (Just defaultThemeWithButtons)
    , manageDocks
    , toggleHook "goFloat" doFloat
    , isDialog     --> makeMaster <+> doFloat
    , isFullscreen --> doFullFloat
    ] ++ concat
    [ [ (className =? x <||> title =? x <||> resource =? x) --> doFullFloat            | x <- fullFloats    ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doFloat                | x <- floats        ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doCenterFloat          | x <- centralFloats ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "1:web"   | x <- webApps       ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "2:code"  | x <- codeApps      ]
    , [ (className =? x <||> role  =? x <||> resource =? x) --> doShiftAndGo "8:gimp"  | x <- gimpApps      ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doIgnore               | x <- ignoredApps   ]
    , [ (className =? x <||> title =? x <||> resource =? x) --> doFloat                | x <- kde           ]
    ]
  where
    role           = stringProperty "WM_WINDOW_ROLE"
    makeMaster     = insertPosition Master Newer
    doShiftAndGo   = doF . liftM2 (.) S.greedyView S.shift
    -- Be careful [] is different from [""], which might catch some unwanted windows
    -- Use "xprop | grep WM_CLASS" to find the class
    centralFloats  = ["systemsettings5"]
    fullFloats     = []
    floats         = []
    webApps        = ["firefox"]      -- open on desktop 1
    codeApps       = ["code", "kate"] -- open on desktop 2
    gimpApps       = ["Gimp", "gimp"] -- open on desktop 8
    kde            = ["plasma-desktop", "Plasma-desktop", "plasma", "Plasma", "krunner", "klipper"]
    ignoredApps    = []

-- ===================================== Layout Hook =====================================

myLayoutHook = let
  noBorder   = Title "synapse" `Or` Title "awn-applet"

  --floating   = named "Floating"   $ maximize $ borderResize $ positionStoreFloat
  tiled1     = named "VTiled"     $ maximize $ mouseResizableTile
  tiled2     = named "HTiled"     $ maximize $ mouseResizableTile { isMirrored = True }
  grid       = named "Grid"       $ maximize Grid
  fullscreen = named "Fullscreen" $ maximize $ smartBorders Full
  tabs       = tabbed shrinkText decoTheme
  gimp       = named "GIMP" $
               withIM (0.15) (Role "gimp-toolbox-1") $
               reflectHoriz $
               withIM (0.15) (Role "gimp-dock-1")
               ( fullscreen |||
                 tabs       |||
                 grid       |||
                 tiled1     |||
                 tiled2       )

  in avoidStruts
     $ minimize
     $ onWorkspace "8:gimp" gimp
     $ boringWindows
     $ noBorderOn noBorder
     $ smartBorders
     $ ( mkToggle (single DECO)
         ( tiled1     |||
           grid       |||
           tiled2     |||
           fullscreen
         )
         ||| tabs
       )

-- ================================ Layout decoration toggle =============================

data DECO = DECO deriving (Read, Show, Eq, Typeable)

instance Transformer DECO Window where
  transform DECO x k = let
    tilingDeco = windowSwitcherDecorationWithButtons shrinkText decoThemeButtons .
                 draggingVisualizer
    in k (tilingDeco x) (const x)

decoThemeButtons = addThemeButtons decoTheme

addThemeButtons :: Theme -> Theme
addThemeButtons x = x {
  windowTitleAddons =
     [ (" (M)", AlignLeft)
     , ("_"   , AlignRightOffset 48)
     , ("[]"  , AlignRightOffset 25)
     , ("X"   , AlignRightOffset 10)
     ]
  }

decoTheme = defaultTheme
  { activeColor         = "#90B460"
  , activeTextColor     = "#2E2E2E"
  , activeBorderColor   = "#90B460"
  , inactiveColor       = "#ABABAB"
  , inactiveTextColor   = "#333333"
  , inactiveBorderColor = "#ABABAB"
  , fontName            = "xft:Sans Bold:size=7"
  , decoHeight          = 13
  }

-- ================================== Keyboard Shortcuts =================================

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [
      -- Lauch applications
      ((modm                  , xK_a      ), spawn "firefox")
    , ((modm                  , xK_z      ), spawn "code")
    , ((modm .|. shiftMask    , xK_z      ), spawn "kate")
    , ((modm                  , xK_s      ), spawn "systemsettings5")
    , ((modm                  , xK_f      ), spawn "dolphin")
    , ((modm                  , xK_t      ), spawn $ XMonad.terminal conf)
    , ((modm .|. mod1Mask     , xK_w      ), restart "kwintoxmd" True)
      -- Basic CycleWS setup
    , ((modm .|. mod1Mask     , xK_Right  ), nextScreen)
    , ((modm .|. mod1Mask     , xK_Left   ), prevScreen)
    , ((modm                  , xK_Right  ), moveTo Next NonEmptyWS)
    , ((modm                  , xK_Left   ), moveTo Prev NonEmptyWS)
    , ((modm                  , xK_grave  ), toggleWS)
      -- Move focus
    , ((modm                  , xK_Down   ), windows S.focusDown)
    , ((modm                  , xK_Up     ), windows S.focusUp  )
      -- Swap the focused window
    , ((modm .|. mod1Mask     , xK_Down   ), windows S.swapDown  )
    , ((modm .|. mod1Mask     , xK_Up     ), windows S.swapUp    )
      -- Toggle panel
    , ((modm                  , xK_p      ), sendMessage ToggleStruts)
      -- Toggle conky
    , ((modm                  , xK_c      ), sendMessage $ ToggleStrut L)
      -- Toggle decoration of all windows
    , ((modm                  , xK_d      ), sendMessage $ Toggle DECO)
      -- LayoutScreens JumpToLayout examples: requires hiding XMonad's, using LayoutScreen
    , ((modm .|. mod1Mask     , xK_f      ), sendMessage $ JumpToLayout "Fullscreen")
    , ((modm .|. mod1Mask     , xK_g      ), sendMessage $ JumpToLayout "GIMP")
    , ((modm .|. mod1Mask     , xK_t      ), sendMessage $ JumpToLayout "Tabbed")
    , ((modm .|. mod1Mask     , xK_v      ), sendMessage $ JumpToLayout "VTiled")
    , ((modm .|. mod1Mask     , xK_h      ), sendMessage $ JumpToLayout "HTiled")
    , ((modm .|. mod1Mask     , xK_a      ), sendMessage $ JumpToLayout "Floating")
      -- Rotate through the available layout algorithms
    , ((modm                  , xK_l      ), sendMessage NextLayout)
      -- Reset the layouts on the current workspace to default
    , ((modm .|. mod1Mask     , xK_l      ), setLayout $ XMonad.layoutHook conf)
      -- Move focus to the master window
    , ((modm .|. mod1Mask     , xK_m      ), windows S.focusMaster)
      -- Rotate slave windows
    , ((modm                   , xK_Tab   ), rotSlavesUp)
    , ((modm .|. shiftMask     , xK_Tab   ), rotSlavesDown)
      -- Swap the focused window and the master window
    , ((modm                  , xK_Return ), windows S.swapMaster)
      -- resizing the master/slave ratio. Shrink/Expand the master area
    , ((modm                  , xK_bracketleft ), sendMessage Shrink)
    , ((modm                  , xK_bracketright), sendMessage Expand)
      -- Maximizing
    , ((modm                  , xK_b      ), withFocused $ sendMessage . maximizeRestore)
      -- Minimizing
      -- floating layer support. Push window back and forth into tiling
    , ((modm                  , xK_equal  ), withFocused $ windows . S.sink)
    , ((modm                  , xK_minus  ), withFocused float)
      -- make next new window as floating
    , ((modm .|. mod1Mask     , xK_minus  ), toggleHookNext "goFloat")
    , ((modm .|. mod1Mask     , xK_b      ), withFocused toggleBorder >> refresh)
      -- increase or decrease number of windows in the master area
    , ((modm                  , xK_comma  ), sendMessage (IncMasterN 1))
    , ((modm                  , xK_period ), sendMessage (IncMasterN (-1)))
      -- Close the focused window
    , ((modm                  , xK_Delete )  , kill)
    , ((modm                  , xK_BackSpace), kill)
      -- Resize viewed windows to the correct size
    , ((modm .|. mod1Mask     , xK_r      ), refresh)
      -- quit, or restart
    , ((modm .|. mod1Mask     , xK_Escape ), io (exitWith ExitSuccess))
    , ((modm .|. mod1Mask     , xK_k      ), spawn "xkill")
    
    , ((modm .|. shiftMask    , xK_space  ), spawn "rofi -show run")
    , ((modm                  , xK_space  ), spawn "rofi -show window")

    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]]

-- ==================================== Mouse control ====================================

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask'}) = M.fromList $
    -- Move a floated window by dragging
    [ ((modMask', button1),
       (\w -> do
           isF <- isFloating w
           if isF
             then focus w >> mouseMoveWindow w >> windows S.shiftMaster
             else focus w >> swapMoveWindow w
       ))
      -- Switch to next and first layout
    , ((modMask', button2),
       (\w -> focus w >> sendMessage NextLayout))
    , ((modMask' .|. shiftMask, button2),
       (\_ -> toggleHookNext "goFloat"))

      -- Resize a floated window by dragging
    , ((modMask', button3),
       (\w -> focus w >> mouseResizeWindow w >> windows S.shiftMaster))
    ]

isFloating :: Window -> X (Bool)
isFloating w = do
    ws <- gets windowset
    return $ M.member w (S.floating ws)

performWindowSwitching :: Window -> X ()
performWindowSwitching w1 =
  withDisplay $ \d -> do
    root <- asks theRoot
    (_,_,_,px,py,_,_,_) <- io $ queryPointer d root
    screen <- pointScreen (fromIntegral px) (fromIntegral py)
    tag    <- return $ (S.tag . S.workspace) <$> screen
    target <- maybe (return Nothing) (findWinOnTag px py w1) tag
    io $ print (tag, w1, target)
    case (tag, target) of
      (_, Just w2) -> (windows $ swapWin w1 w2)   >> focus w1
      (Just t, _ ) -> (windows $ S.shiftWin t w1) >> focus w1
      _            -> refresh

findWinOnTag :: CInt -> CInt -> Window -> WorkspaceId -> X (Maybe Window)
findWinOnTag px py wid tag = withDisplay $ \d -> do
  wins <- (S.index . S.view tag) <$> gets windowset
  was  <- mapM (io . getWindowAttributes d) wins
  let
    pairs  = zip was wins
    isInWin mx my wa = let
      x1 = wa_x wa
      x2 = x1 + wa_width wa
      y1 = wa_y wa
      y2 = y1 + wa_height wa
      in mx > x1 && mx < x2 && my > y1 && my < y2
    target = find (\(wa, w) -> w /= wid && isInWin px py wa) pairs
  return (snd <$> target)

swapWin :: (Ord a, Eq a, Eq s, Eq i) => a -> a
        -> S.StackSet i l a s sd -> S.StackSet i l a s sd
swapWin w1 w2 ss = case (S.findTag w1 ss, S.findTag w2 ss) of
  (Just ws1, Just ws2)
    | ws1 /= ws2 -> swapWSs w1 w2 ss
    | otherwise  -> S.modify' (swap w1 w2) $ S.view ws1 ss
  _              -> ss
  where
    swapWSs wa wb = let
      foo ws = maybe ws (\s -> ws {S.stack = Just $ swap wa wb s}) (S.stack ws)
      in S.mapWorkspace foo

    swap wa wb (S.Stack f up down) = let
      newf    = switchEntries wa wb f
      newup   = map (switchEntries wa wb) up
      newdown = map (switchEntries wa wb) down
      switchEntries a b x
        | x == a    = b
        | x == b    = a
        | otherwise = x
      in S.Stack newf newup newdown

swapMoveWindow :: Window -> X ()
swapMoveWindow w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
    let ox = fromIntegral ox'
        oy = fromIntegral oy'
        getx ex = fromIntegral (fromIntegral (wa_x wa) + (ex - ox))
        gety ey = fromIntegral (fromIntegral (wa_y wa) + (ey - oy))
    mouseDrag (\ex ey -> io $ moveWindow d w (getx ex) (gety ey))
              (performWindowSwitching w)

-- ====================================== Logging ========================================

myLogHook :: D.Client -> X ()
myLogHook dbus = do
  fadeInactiveCurrentWSLogHook 0.8
  currentWorkspaceOnTop
  ewmhDesktopsLogHook
  currentScreen <- withWindowSet (return . S.screen . S.current)
  myLogPPDBus currentScreen dbus

myPPLayout :: String -> String
myPPLayout x
  | null fields         = x
  | isThis "HTiled"     = " [=] "
  | isThis "VTiled"     = " [|] "
  | isThis "Floating"   = " [~] "
  | isThis "Fullscreen" = " [ ] "
  | isThis "Grid"       = " [+] "
  | isThis "GIMP"       = " [G] "
  | isThis "Tabbed"     = " [T] "
  | otherwise           = x
  where
    isThis = flip elem fields
    fields = words x

-- ======================= PrettyPrinter for DBus (xmonad log appelet) ===================

myLogPPDBus :: ScreenId -> D.Client -> X ()
myLogPPDBus (S currentSID) dbus = let
  pp = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = const ""
    , ppWsSep    = " | "
    , ppCurrent  = wrap "[" "]"
    , ppVisible  = wrap "(" ")"
    , ppHidden   = id
    , ppUrgent   = id
    , ppLayout   = wrap "" "" . myPPLayout
    , ppSep      = " "
    }
  in dynamicLogWithPP pp

getDBusClient :: IO (D.Client)
getDBusClient = do
  dbus <- D.connectSession
  getWellKnownName dbus
  return dbus

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = x >> return ()
  where x = D.requestName dbus (D.busName_ "org.xmonad.Log")
            [ D.nameAllowReplacement
            , D.nameReplaceExisting
            , D.nameDoNotQueue
            ]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
        D.signalBody = [D.toVariant str]
        }
  D.emit dbus signal