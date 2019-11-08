--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Control.Monad
import Data.Monoid
import Data.List
import Data.Char
import XMonad

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects (Project(..), dynamicProjects, shiftToProjectPrompt, switchProjectPrompt)
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowGo

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Column
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named
-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import XMonad.Prompt

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

import Graphics.X11.ExtraTypes.XF86
import System.Exit

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces =
--   ["Web", "Terminal", "Office", "Emacs", "Lab", "View", "Design", "PyCharm", "Test"]

myProjects :: [Project]
myProjects =
  [ Project "1:web" "~" . Just $ spawn "firefox",
    Project "2:term" "~" . Just $ spawn (myTerminal ++ " -e one-tmux"),
    Project "3:emacs" "~" Nothing,
    Project "4:py" "~" Nothing,
    Project "5:view" "~" Nothing,
    Project "6:writer" "~" Nothing,
    Project "7:explorer" "~" Nothing,
    Project "8" "~" Nothing,
    Project "9" "~" Nothing,
    Project "matlab" "~" . Just $ spawn "mymatlab",
    Project "gimp" "/tmp" . Just $ spawn "gimp",
    Project "inkscape" "/tmp" . Just $ spawn "inkscape"
  ]

myWorkspaces = map projectName myProjects
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#7c7c7c"

myFocusedBorderColor = "#ff0000"

base03 ="#002b36"
active = "#268bd2"

myPromptTheme = def
    { font                  = "xft:Sarasa Mono SC-10:bold"
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = 20
    , position              = Top
    }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
         [] -> windows copyToAll
         _  -> killAllOtherCopies

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn (myTerminal ++ " -e one-tmux"))
    -- launch dmenu
  , ((modm, xK_p), spawn "rofi -sort -matching fuzzy -show run")
    -- close focused window
  , ((modm .|. shiftMask, xK_q), kill1)
     -- Rotate through the available layout algorithms
  , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
  -- , ((modm, xK_n), refresh)
    -- Move focus to the next window
  -- , ((modm, xK_Tab), nextMatch History (return True))
    -- Move focus to the next window
  -- , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  -- , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- lock screen
  , ((modm .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    -- Shrink the master area
  -- , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
  -- , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
  , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  -- , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  -- , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
  , ((modm, xK_b), sendMessage ToggleStruts)
    -- Quit xmonad
  , ((modm .|. shiftMask, xK_c), io (exitWith ExitSuccess))
    -- Restart xmonad
  -- , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
  , ( (modm .|. shiftMask, xK_slash)
    , spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    -- Mute volume.
  , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
    -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
    -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
  ]
   ++
   zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
   ++
   zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
  -- [ ((m .|. modm, k), windows $ f i)
  --  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  --  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  -- ] ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
  -- [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  -- ]

nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

myAdditionalKeys =
  [
  ("M-,",  switchProjectPrompt myPromptTheme),
  ("M-S-,", shiftToProjectPrompt myPromptTheme),
  ("M-.", nextNonEmptyWS),
  ("M-S-.", prevNonEmptyWS),
  ("M-u", toggleWS' ["NSP"]),
  ("M-<", sendMessage (IncMasterN 1)),
  ("M->", sendMessage (IncMasterN (-1))),
  ("M-; r", namedScratchpadAction myScratchPads "ranger"),
  ("M-; q", namedScratchpadAction myScratchPads "terminal"),
  ("M-; i", namedScratchpadAction myScratchPads "ipython"),
  ("M-; d", namedScratchpadAction myScratchPads "zeal"),
  ("M-; e", spawn "emacsclient -c"),
  ("M-; t", namedScratchpadAction myScratchPads "mail"),
  ("M-; v", namedScratchpadAction myScratchPads "volume"),
  ("M-; w", namedScratchpadAction myScratchPads "writefull"),
  ("M-; f", runOrRaiseNext "nautilus" (className =? "Nautilus")),
  ("M-; p", spawn "rofi -sort -matching fuzzy -show file -modi file:\"rofi-file-browser $HOME/Documents\"")
  , ("<Print>", spawn "flameshot gui")
  , ("M-d", toggleCopyToAll)
  , ("M-n", switchLayer)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-i", sendMessage Shrink)
  , ("M-o", sendMessage Expand)
  , ("M-h", bindOn LD [("tab", windows W.focusUp),("", windowGo L True)])
  , ("M-j", windowGo D True)
  , ("M-k", windowGo U True)
  , ("M-l", bindOn LD [("tab", windows W.focusDown),("", windowGo R True)])
  , ("M-m", spawn "rofi -sort -matching fuzzy -show window")
  , ("M-S-<L>", withFocused (keysResizeWindow (-30,0) (0,0))) --shrink float at right
  , ("M-S-<R>", withFocused (keysResizeWindow (30,0) (0,0))) --expand float at right
  , ("M-S-<D>", withFocused (keysResizeWindow (0,30) (0,0))) --expand float at bottom
  , ("M-S-<U>", withFocused (keysResizeWindow (0,-30) (0,0))) --shrink float at bottom
  , ("M-C-<L>", withFocused (keysResizeWindow (30,0) (1,0))) --expand float at left
  , ("M-C-<R>", withFocused (keysResizeWindow (-30,0) (1,0))) --shrink float at left
  , ("M-C-<U>", withFocused (keysResizeWindow (0,30) (0,1))) --expand float at top
  , ("M-C-<D>", withFocused (keysResizeWindow (0,-30) (0,1))) --shrink float at top
  , ("M-<L>", withFocused (keysMoveWindow (-30,0)))
  , ("M-<R>", withFocused (keysMoveWindow (30,0)))
  , ("M-<U>", withFocused (keysMoveWindow (0,-30)))
  , ("M-<D>", withFocused (keysMoveWindow (0,30)))
  , ("M-; <L>", withFocused $ snapMove L Nothing)
  , ("M-; <R>", withFocused $ snapMove R Nothing)
  , ("M-; <U>", withFocused $ snapMove U Nothing)
  , ("M-; <D>", withFocused $ snapMove D Nothing)
  ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , (\w -> focus w >> Flex.mouseWindow Flex.position w))
    -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
    -- mod-button3, Set the window to floating mode and resize by dragging

  , ((modm .|. shiftMask, button1), (\w -> focus w >> Flex.mouseWindow Flex.resize w))
  -- , ( (modm, button3)
  -- , (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ (tiled ||| tab ||| full)
  --Layouts
  where
    tiled = named "tile" $ smartBorders (ResizableTall 1 (2 / 100) (1 / 2) [])
    -- mtile     = named "mtile" $ Mirror tiled
    tab = named "tab" $ tabbed shrinkText tabConfig
    full = named "full" $ noBorders Full
    -- grid      = named "grid" $ smartBorders(Grid)

tabConfig =
  def
    { activeBorderColor = "#7C7C7C"
    , activeTextColor = "#CEFFAC"
    , activeColor = "#000000"
    , inactiveBorderColor = "#7C7C7C"
    , inactiveTextColor = "#EEEEEE"
    , inactiveColor = "#000000"
    , fontName = "xft:Sarasa Mono SC-10:bold"
    }

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--


myManageHook :: ManageHook
myManageHook =
  (composeAll $
  [isDialog --> doFloat] ++
  [appName =? r --> doIgnore | r <- myIgnores] ++
  -- auto-float certain windows
  [className =? c --> doCenterFloat | c <- myCenFloats] ++
  [className =? t --> doFloat | t <- myFloat] ++
  -- send certain windows to certain workspaces
  [className =? r --> viewShift wsp | (r, wsp) <- myWorkspaceMove] ++
  -- fulscreen windows to fullfloating
  [isFullscreen --> doFullFloat] ++
  [title  =? "urxvt" --> doCenterFloat]
  )
        -- windows to operate
  <+> namedScratchpadManageHook myScratchPads
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    prefixTitle prefix = fmap (prefix `isPrefixOf`) title
    myIgnores = ["desktop", "kdesktop", "desktop_window", "stalonetray"]
    myCenFloats =
      [ "Steam"
      , "steam"
      , "vlc"
      , "Vlc"
      , "mpv"
      , "wine"
      , "electronic-wechat"
      , "smplayer"
      , "flameshot"
      ]
    myFloat = ["Hangouts", "Gnome-terminal", "GoldenDict", "Zeal"]
    myWorkspaceMove = [("Firefox", "web"),
	("Google-chrome", "web"),
	("jetbrains-pycharm", "py"),
	("MATLAB R2018b - academic use", "matlab"),
        ("MATLAB R2018b", "matlab")
        ]

configuredRect::Rational->Rational->Rational->Rational->W.RationalRect
configuredRect l t w h =
  W.RationalRect l (t + height - t * height) w ((1 - height) * h)
  where
    height = (19 / 1080)

myScratchPads =
  [NS "terminal" "urxvtc -title scratchpad" (title =? "scratchpad") doTopFloat
   , NS "ipython" "urxvtc -title ipython -e ipython" (title =? "ipython") doTopLeftFloat
   , NS "mail" "thunderbird" (className =? "Thunderbird") doLeftFloat
   , NS "ranger" "urxvtc -title ranger -e ranger" (title =? "ranger") doBottomLeftFloat
   , NS "volume" "urxvtc -title alsa -e alsamixer" (title =? "alsa") doTopLeftFloat
   , NS "zeal" "zeal" (className =? "Zeal") doTopLeftFloat
   , NS "writefull" "writefull" (className =? "Writefull") doTopLeftFloat
   -- , NS "file" "nautilus" (className =? "Nautilus") doRightFloat
   ]
  where
    prefixTitle prefix = fmap (prefix `isPrefixOf`) title
    doTopFloat = customFloating $ configuredRect (1/4) 0 (1/2) (1/2)
    doTopLeftFloat = customFloating $ configuredRect 0 0 (1/2) (1/2)
    doBottomLeftFloat = customFloating $ configuredRect 0 (1/2) (1/2) (1/2)
    doLeftFloat = customFloating $ configuredRect 0 0 (1/2) 1
    doRightFloat = customFloating $ configuredRect (1/2) 0 (1/2) 1
------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook <+> docksEventHook

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = do
  takeTopFocus
  dynamicLogWithPP xmobarPP
  fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 0xdddddddd

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_pirate

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
main =
  xmonad =<< xmobar (ewmh . docks . dynamicProjects myProjects $ defaults)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults =
  def
      -- simple stuff
    { terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , clickJustFocuses = myClickJustFocuses
    , borderWidth = myBorderWidth
    , modMask = myModMask
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
      -- key bindings
    , keys = myKeys
    , mouseBindings = myMouseBindings
      -- hooks, layouts
    , layoutHook = myLayout
    , manageHook = manageHook defaultConfig <+> myManageHook <+> manageDocks
    , handleEventHook = myEventHook
    , logHook = myLogHook >> historyHook
    , startupHook = myStartupHook
    }  `additionalKeysP` myAdditionalKeys


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'alt'. Default keybindings:"
    , ""
    , "-- launching and killing programs"
    , "mod-Shift-Enter  Launch xterminal"
    , "mod-p            Launch dmenu"
    , "mod-Shift-p      Launch gmrun"
    , "mod-Shift-c      Close/kill the focused window"
    , "mod-Space        Rotate through the available layout algorithms"
    , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
    , "mod-n            Resize/refresh viewed windows to the correct size"
    , ""
    , "-- move focus up or down the window stack"
    , "mod-Tab        Move focus to the next window"
    , "mod-Shift-Tab  Move focus to the previous window"
    , "mod-j          Move focus to the next window"
    , "mod-k          Move focus to the previous window"
    , "mod-m          Move focus to the master window"
    , ""
    , "-- modifying the window order"
    , "mod-Return   Swap the focused window and the master window"
    , "mod-Shift-j  Swap the focused window with the next window"
    , "mod-Shift-k  Swap the focused window with the previous window"
    , ""
    , "-- resizing the master/slave ratio"
    , "mod-h  Shrink the master area"
    , "mod-l  Expand the master area"
    , ""
    , "-- floating layer support"
    , "mod-t  Push window back into tiling; unfloat and re-tile it"
    , ""
    , "-- increase or decrease number of windows in the master area"
    , "mod-comma  (mod-,)   Increment the number of windows in the master area"
    , "mod-period (mod-.)   Deincrement the number of windows in the master area"
    , ""
    , "-- quit, or restart"
    , "mod-Shift-q  Quit xmonad"
    , "mod-q        Restart xmonad"
    , "mod-[1..9]   Switch to workSpace N"
    , ""
    , "-- Workspaces & screens"
    , "mod-Shift-[1..9]   Move client to workspace N"
    , "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3"
    , "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3"
    , ""
    , "-- Mouse bindings: default actions bound to mouse events"
    , "mod-button1  Set the window to floating mode and move by dragging"
    , "mod-button2  Raise the window to the top of the stack"
    , "mod-button3  Set the window to floating mode and resize by dragging"
    ]

--- XMonad.Actions.ConditionalKeys

data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)


-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X())] -> X()
bindOn xc bindings = chooseAction xc $ chooser where
    chooser xc = case find ((xc==).fst) bindings of
            Just (_, action) -> action
            Nothing -> case find ((""==).fst) bindings of
                                Just (_, action) -> action
                                Nothing -> return ()
