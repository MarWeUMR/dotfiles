import           XMonad

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Run                ( spawnPipe )
import           XMonad.Util.EZConfig           ( additionalKeys )
import           System.IO


import           Data.Char                      ( isSpace )
import           Data.List
import           Data.Monoid
import           Data.Maybe                     ( isJust )
import qualified Data.Map                      as M



import           XMonad.Util.EZConfig           ( additionalKeysP )

import           XMonad.Config.Desktop


import           System.Exit                    ( exitSuccess )
import qualified XMonad.StackSet               as W

    -- Actions
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.Promote
import           XMonad.Actions.MouseResize
import           XMonad.Actions.CycleWS         ( moveTo
                                                , shiftTo
                                                , WSType(..)
                                                , nextScreen
                                                , prevScreen
                                                )
import           XMonad.Actions.WithAll         ( sinkAll
                                                , killAll
                                                )



import qualified DBus                          as D
import qualified DBus.Client                   as D
import qualified Codec.Binary.UTF8.String      as UTF8


    -- Layouts modifiers
import           XMonad.Layout.Decoration
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows     ( limitWindows
                                                , increaseLimit
                                                , decreaseLimit
                                                )
import           XMonad.Layout.MultiToggle      ( mkToggle
                                                , single
                                                , EOT(EOT)
                                                , Toggle(..)
                                                , (??)
                                                )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers
                                                  ( NBFULL
                                                  , MIRROR
                                                  , NOBORDERS
                                                  )
                                                )
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect          ( REFLECTX(..)
                                                , REFLECTY(..)
                                                )
import           XMonad.Layout.Renamed          ( renamed
                                                , Rename(Replace)
                                                )
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowArranger   ( windowArrange
                                                , WindowArrangerMsg(..)
                                                )
import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( toggleLayouts
                                                , ToggleLayout(Toggle)
                                                )

    -- Layouts
import           XMonad.Layout.GridVariants     ( Grid(Grid) )
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spiral
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ZoomRow          ( zoomReset
                                                , ZoomMessage(ZoomFullToggle)
                                                )



------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------


myBorderWidth :: Dimension
myBorderWidth = 4          -- Sets border width for windows

myNormColor :: [Char]
myNormColor = "#292d3e"  -- Border color of normal windows

myFocusColor :: [Char]
myFocusColor = "#bbc5ff"  -- Border color of focused windows

blue = "#83a598"
blue2 = "#2266d0"
red = "#fb4934"
------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =


    -- Xmonad
  [ ( "M-C-r"
    , spawn "xmonad --recompile"
    )      -- Recompiles xmonad
  , ( "M-S-r"
    , spawn "xmonad --restart"
    )        -- Restarts xmonad
  , ( "M-S-q"
    , io exitSuccess
    )                  -- Quits xmonad
  , ( "M-<Return>"
    , spawn "alacritty"
    )


    -- Windows
  , ( "M-q"
    , kill1
    )                           -- Kill the currently focused client


    -- Windows navigation
  , ( "M-m"
    , windows W.focusMaster
    )     -- Move focus to the master window
  , ( "M-j"
    , windows W.focusDown
    )       -- Move focus to the next window
  , ( "M-k"
    , windows W.focusUp
    )         -- Move focus to the prev window
  , ( "M-S-m"
    , windows W.swapMaster
    )    -- Swap the focused window and the master window
  , ( "M-S-j"
    , windows W.swapDown
    )      -- Swap focused window with next window
  , ( "M-S-k"
    , windows W.swapUp
    )        -- Swap focused window with prev window
  , ( "M-<Backspace>"
    , promote
    )         -- Moves focused window to master, others maintain order
  , ( "M-<Up>"
    , sendMessage (MoveUp 10)
    )             --  Move focused window to up
  , ( "M-<Down>"
    , sendMessage (MoveDown 10)
    )         --  Move focused window to down
  , ( "M-<Right>"
    , sendMessage (MoveRight 10)
    )       --  Move focused window to right
  , ( "M-<Left>"
    , sendMessage (MoveLeft 10)
    )         --  Move focused window to left
  , ( "M-S-<Up>"
    , sendMessage (IncreaseUp 10)
    )       --  Increase size of focused window up
  , ( "M-S-<Down>"
    , sendMessage (IncreaseDown 10)
    )   --  Increase size of focused window down
  , ( "M-S-<Right>"
    , sendMessage (IncreaseRight 10)
    ) --  Increase size of focused window right
  , ( "M-S-<Left>"
    , sendMessage (IncreaseLeft 10)
    )   --  Increase size of focused window left
  , ( "M-C-<Up>"
    , sendMessage (DecreaseUp 10)
    )       --  Decrease size of focused window up
  , ( "M-C-<Down>"
    , sendMessage (DecreaseDown 10)
    )   --  Decrease size of focused window down
  , ( "M-C-<Right>"
    , sendMessage (DecreaseRight 10)
    ) --  Decrease size of focused window right
  , ( "M-C-<Left>"
    , sendMessage (DecreaseLeft 10)
    )   --  Decrease size of focused window left


-- Floating windows
  , ( "M-f"
    , sendMessage (T.Toggle "floats")
    )       -- Toggles my 'floats' layout
  , ( "M-<Delete>"
    , withFocused $ windows . W.sink
    ) -- Push floating window back to tile
  , ( "M-S-<Delete>"
    , sinkAll
    )                      -- Push ALL floating windows to tile
  , ( "M-a"
    , windows copyToAll
    ) -- Pin to all workspaces
  , ( "M-C-a"
    , killAllOtherCopies
    ) -- remove window from all but current

-- Layouts
  , ( "M-<Tab>"
    , sendMessage NextLayout
    )                                    -- Switch to next layout
  , ("M-h"  , sendMessage Shrink)
  , ("M-l"  , sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ( "M-S-f"
    , sendMessage zoomReset
    )
        --, ("M-f", sendMessage ZoomFullToggle)
  , ( "M-<Space>"
    , sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts
    ) -- Toggles noborder/full
  , ( "M-S-<Space>"
    , sendMessage ToggleStruts
    )                              -- Toggles struts
  , ( "M-S-n"
    , sendMessage $ Toggle NOBORDERS
    )                              -- Toggles noborder

         --- My Applications (Super+Alt+Key)
  , ("M-M1-b", spawn "google-chrome-stable")
  , ("M-M1-t", spawn "telegram-desktop")
  , ("M-M1-w", spawn "walc")
  , ("M-e"   , spawn "nemo")
  , ( "M-M1-e"
    , spawn "emacs"
    )




    --- Dmenu Scripts (Alt+Ctr+Key)
  , ("M-d", spawn "dmenu_run")
  ] where
  nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
  nonEmptyNonNSP =
    WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))



-----------------------------------------------------------------------------}}}
-- LOGHOOK                                                                   {{{
--------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus = def { ppOutput  = dbusOutput dbus
                     , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
                     , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
                     , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
                     , ppHidden  = wrap " " " "
                     , ppWsSep   = ""
                     , ppSep     = " | "
                     , ppTitle   = myAddSpaces 25
                     }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName)
        { D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
  D.emit dbus signal
 where
  objectPath    = D.objectPath_ "/org/xmonad/Log"
  interfaceName = D.interfaceName_ "org.xmonad.Log"
  memberName    = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where sstr = shorten len str

    ------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.

xmobarEscape :: [Char] -> [Char]
xmobarEscape = concatMap doubleLts
 where
  doubleLts '<' = "<<"
  doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces =
  clickable
    . (map xmobarEscape)
    $ ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]
 where
  clickable l =
    [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>"
    | (i, ws) <- zip [1 .. 9] l
    , let n = i
    ]


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

-- spawnSingleton :: String -> X ()
-- spawnSingleton cmd = flip whenX (spawn cmd) $ do
--   ws <- gets windowset
--   let ws' = W.allWindows ws
--   pure True


myStartupHook :: X ()
myStartupHook = do
  spawn
    "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17"
  spawn "stalonetray"
  spawn "dropbox start"

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing'
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- The layouts that I use
tall = renamed [Replace "tall"] $ limitWindows 12 $ mySpacing 8 $ ResizableTall
  1
  (3 / 100)
  (1 / 2)
  []
monocle = renamed [Replace "monocle"] $ limitWindows 20 $ Full
floats = renamed [Replace "floats"] $ limitWindows 20 $ simplestFloat
grid =
  renamed [Replace "grid"]
    $ limitWindows 12
    $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)
spirals = renamed [Replace "spirals"] $ mySpacing' 8 $ spiral (6 / 7)
threeCol =
  renamed [Replace "threeCol"] $ limitWindows 7 $ mySpacing' 4 $ ThreeCol
    1
    (3 / 100)
    (1 / 2)
threeRow =
  renamed [Replace "threeRow"]
    $ limitWindows 7
    $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
    $ Mirror
    $ ThreeCol 1 (3 / 100) (1 / 2)
tabs = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
                                $ tabbed shrinkText myTabConfig
 where
  myTabConfig = def { activeColor         = "#292d3e"
                    , inactiveColor       = "#3e445e"
                    , activeBorderColor   = "#292d3e"
                    , inactiveBorderColor = "#292d3e"
                    , activeTextColor     = "#ffffff"
                    , inactiveTextColor   = "#d0d0d0"
                    }

-- The layout hook
myLayoutHook =
  avoidStruts
    $ mouseResize
    $ windowArrange
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
    $ myDefaultLayout
 where
  myDefaultLayout =
    tall
      ||| noBorders monocle
      ||| floats
      ||| grid
      ||| noBorders tabs
      ||| spirals
      ||| threeCol
      ||| threeRow


------------------------------------------------------------------------
-- ManageHook
------------------------------------------------------------------------
myManageHook = composeAll
  [ className =? "plasma" --> doFloat
  , className =? "Plasma" --> doFloat
  , className =? "plasma-desktop" --> doFloat
  , className =? "Plasma-desktop" --> doFloat
  , className =? "krunner" --> doIgnore >> doFloat
  , className =? "ksplashsimple" --> doFloat
  , className =? "ksplashqml" --> doFloat
  , className =? "ksplashx" --> doFloat
  ]



main = do
  xmonad
    $                 kde4Config
                        { manageHook = manageDocks <+> myManageHook <+> manageHook kde4Config
                        , layoutHook         = myLayoutHook
                        , modMask            = mod4Mask     -- Rebind Mod to the Windows key
                        , terminal           = "alacritty"
                        , startupHook        = myStartupHook
                        , borderWidth        = myBorderWidth
                        , normalBorderColor  = myNormColor
                        , focusedBorderColor = myFocusColor
                        }
    `additionalKeysP` myKeys
