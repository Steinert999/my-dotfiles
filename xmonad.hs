-- Copyright (C) 2020-2024 Aditya Shakya <adi1090x@gmail.com>
--
-- Xmonad config for Archcraft

-- ## Modules ## -------------------------------------------------------------------

import Colors.MonokaiPro
import Control.Monad
import Control.Monad.RWS (Monoid (mempty))
import Data.List (replicate, reverse)
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize
import XMonad.Actions.OnScreen
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.BoringWindows qualified as BW
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.ManageHook
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

-- ## Startup hook ## ---------------------------------------------------------------
myStartupHook = do
  spawn "bash ~/.xmonad/scripts/xmonad_autostart"
  modify $ \xstate -> xstate {windowset = onlyOnScreen 1 "1_1" (windowset xstate)}

-- ## Applications ## ---------------------------------------------------------------
-- Terminal
myTerminal :: String
myTerminal = "~/.xmonad/scripts/xmonad_term"

-- Apps
fileManager :: X ()
fileManager = spawn "alacritty -e /bin/zsh -c 'ranger'"

textEditor :: X ()
textEditor = spawn "neovide "

webBrowser :: X ()
webBrowser = spawn "firefox"

-- Rofi Menus
rofiNetworkMenu :: X ()
rofiNetworkMenu = spawn "~/.xmonad/scripts/network_menu"

rofiAsroot :: X ()
rofiAsroot = spawn "~/.xmonad/scripts/rofi_asroot"

rofiBluetooth :: X ()
rofiBluetooth = spawn "~/.xmonad/scripts/rofi_bluetooth"

rofiLauncher :: X ()
rofiLauncher = spawn "~/.xmonad/scripts/rofi_launcher"

rofiPowermenu :: X ()
rofiPowermenu = spawn "~/.xmonad/scripts/rofi_powermenu"

rofiRunner :: X ()
rofiRunner = spawn "~/.xmonad/scripts/rofi_runner"

rofiScreenshot :: X ()
rofiScreenshot = spawn "~/.xmonad/scripts/rofi_screenshot"

rofiWindows :: X ()
rofiWindows = spawn "~/.xmonad/scripts/rofi_windows"

xmonadNcmpcpp :: X ()
xmonadNcmpcpp = spawn "~/.xmonad/scripts/xmonad_music"

xmonadLogout :: X ()
xmonadLogout = spawn "betterlockscreen -l"

-- ## Settings ## -------------------------------------------------------------------

-- focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- clicking on a window to focus
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels
myBorderWidth :: Dimension
myBorderWidth = 3

-- Border colors for focused & unfocused windows
myFocusedBorderColor :: String
myFocusedBorderColor = altMagenta

myNormalBorderColor :: String
myNormalBorderColor = altYellow

-- modMask : modkey you want to use
-- mod1Mask : left alt Key
-- mod4Mask : Windows or Modm Key
myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [WorkspaceId]
-- myWorkspaces = 4 `replicate` "\xf111"
myWorkspaces = ["1", "2", "3", "4"]

isPrefix :: String -> Bool
isPrefix s
  | s `elem` ["0_", "1_"] = True
  | otherwise = False

removePrefixFromString :: String -> String
removePrefixFromString [] = error "Empty String"
removePrefixFromString (x : y : xs) = " win " ++ xs ++ "/scr " ++ [x]

type ScratchpadName = String

myScratchpads :: NamedScratchpads
myScratchpads =
  [ buildNS "keepassxc",
    buildNS "terminal"
  ]
  where
    buildNS :: ScratchpadName -> NamedScratchpad
    buildNS name = NS name (spawnNS name) (findNS name) (manageNS name)

    spawnNS :: String -> String
    spawnNS "keepassxc" = "keepassxc"
    spawnNS "terminal" = myTerminal

    findNS :: ScratchpadName -> Query Bool
    findNS name = title =? name

    manageNS :: ScratchpadName -> ManageHook
    manageNS _ = defaultFloating

-- ## Key Bindings ## -------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Keybindings" $ io $ do
  handler <- spawnPipe "rofi -dmenu -theme $HOME/.xmonad/theme/rofi/keybindings.rasi "
  hPutStr handler $ unlines (showKmSimple x)
  return ()

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  [ subtitle "Terminal keys related",
    ((modm, xK_Return), addName "Launch terminal" $ spawn $ XMonad.terminal conf),
    ((modm .|. shiftMask, xK_Return), addName "Launch terminal in float mode" $ spawn "~/.xmonad/scripts/xmonad_term --float"),
    ((modm .|. mod1Mask, xK_Return), addName "Launch terminal in fullsize mode" $ spawn "~/.xmonad/scripts/xmonad_term --full"),
    subtitle "Applications keys related",
    ((modm .|. shiftMask, xK_f), addName "Launch file manager" fileManager),
    ((modm .|. shiftMask, xK_e), addName "Launch text editor" textEditor),
    ((modm .|. shiftMask, xK_w), addName "Launch web browser" webBrowser),
    ((modm, xK_p), addName "Launch colorpicker" $ spawn "~/.xmonad/scripts/xmonad_colorpicker"),
    subtitle "Rofi keys related",
    ((mod1Mask, xK_F1), addName "Rofi launcher menu" rofiLauncher),
    ((mod1Mask, xK_F2), addName "Rofi runner menu" rofiRunner),
    ((modm, xK_n), addName "Rofi network menu" rofiNetworkMenu),
    ((modm, xK_x), addName "Rofi power menu" rofiPowermenu),
    ((mod1Mask, xK_m), addName "Rofi MPD menu" xmonadNcmpcpp),
    ((modm, xK_s), addName "Rofi screenshot menu" rofiScreenshot),
    ((modm, xK_r), addName "Rofi runner menu as root" rofiAsroot),
    ((modm, xK_w), addName "Rofi windows menu" rofiWindows),
    subtitle "Audio keys related",
    ((0, xF86XK_AudioPlay), addName "MPC play" $ spawn "mpc toggle"),
    ((0, xF86XK_AudioPrev), addName "MPC previous" $ spawn "mpc prev"),
    ((0, xF86XK_AudioNext), addName "MPC next" $ spawn "mpc next"),
    ((0, xF86XK_AudioStop), addName "MPC stop" $ spawn "mpc stop"),
    ((0, xF86XK_AudioRaiseVolume), addName "MPC raise volume" $ spawn "~/.xmonad/scripts/xmonad_volume --inc"),
    ((0, xF86XK_AudioLowerVolume), addName "MPC lower volume" $ spawn "~/.xmonad/scripts/xmonad_volume --dec"),
    ((0, xF86XK_AudioMute), addName "MPC mute volume" $ spawn "~/.xmonad/scripts/xmonad_volume --toggle"),
    ((0, xF86XK_AudioMicMute), addName "MPC mute mic" $ spawn "~/.xmonad/scripts/xmonad_volume --toggle-mic"),
    subtitle "Brightness keys related",
    ((0, xF86XK_MonBrightnessUp), addName "Brightness up" $ spawn "~/.xmonad/scripts/xmonad_brightness --inc"),
    ((0, xF86XK_MonBrightnessDown), addName "Brightness down" $ spawn "~/.xmonad/scripts/xmonad_brightness --dec"),
    subtitle "Screenshot keys related",
    ((0, xK_Print), addName "Print all screens now" $ spawn "~/.xmonad/scripts/xmonad_screenshot --now"),
    ((mod1Mask, xK_Print), addName "Print all screens with delay (5 sec)" $ spawn "~/.xmonad/scripts/xmonad_screenshot --in5"),
    ((shiftMask, xK_Print), addName "Print all screens with delay (10 sec)" $ spawn "~/.xmonad/scripts/xmonad_screenshot --in10"),
    ((controlMask, xK_Print), addName "Print one window" $ spawn "~/.xmonad/scripts/xmonad_screenshot --win"),
    ((modm, xK_Print), addName "Print an area" $ spawn "~/.xmonad/scripts/xmonad_screenshot --area"),
    -- , subtitle "Named scratchpads keys related"
    -- , ((mod1Mask .|. controlMask, xK_k), addName "Open keepassxc as scratchpad" $ namedScratchpadAction myScratchpads "keepassxc")
    -- , ((mod1Mask .|. controlMask, xK_t), addName "Open terminal as scratchpad" $ namedScratchpadAction myScratchpads "terminal")

    subtitle "Window tiling keys related",
    ((modm, xK_c), addName "Kill tile" kill),
    ((modm, xK_Escape), addName "Xkill tile" $ spawn "xkill"),
    ((mod1Mask .|. controlMask, xK_l), addName "Lock screen" $ spawn "betterlockscreen --lock"),
    ((modm .|. controlMask, xK_g), addName "Toggle gaps" $ sendMessage ToggleGaps), -- toggle all gaps
    ((modm .|. shiftMask, xK_g), addName "Set margin gap" $ sendMessage $ setGaps [(L, 10), (R, 15), (U, 100), (D, 15)]), -- reset the GapSpec
    ((modm .|. controlMask, xK_t), addName "Inc gap left" $ sendMessage $ IncGap 10 L), -- increment the left-hand gap
    ((modm .|. shiftMask, xK_t), addName "Dec gap left" $ sendMessage $ DecGap 10 L), -- decrement the left-hand gap
    ((modm .|. controlMask, xK_y), addName "Inc gap top" $ sendMessage $ IncGap 10 U), -- increment the top gap
    ((modm .|. shiftMask, xK_y), addName "Dec gap top" $ sendMessage $ DecGap 10 U), -- decrement the top gap
    ((modm .|. controlMask, xK_u), addName "Inc gap down" $ sendMessage $ IncGap 10 D), -- increment the bottom gap
    ((modm .|. shiftMask, xK_u), addName "Dec gap down" $ sendMessage $ DecGap 10 D), -- decrement the bottom gap
    ((modm .|. controlMask, xK_i), addName "Inc gap right" $ sendMessage $ IncGap 10 R), -- increment the right-hand gap
    ((modm .|. shiftMask, xK_i), addName "Dec gap right" $ sendMessage $ DecGap 10 R), -- decrement the right-hand gap
    ((modm .|. shiftMask, xK_r), addName "Resize windows to correct size" refresh),
    ((modm .|. shiftMask, xK_m), addName "Focus to the master window" $ windows W.focusMaster),
    ((modm .|. shiftMask, xK_s), addName "Swap focused window and master" $ windows W.swapMaster),
    ((modm .|. shiftMask, xK_t), addName "Push window back to tiling" $ withFocused $ windows . W.sink),
    ((modm, xK_space), addName "Rotate layouts" $ sendMessage NextLayout),
    ((modm .|. shiftMask, xK_space), addName "Reset layout on workspace to default" $ setLayout $ XMonad.layoutHook conf),
    ((modm, xK_Tab), addName "Change focus to next window" $ windows W.focusDown),
    ((modm, xK_j), addName "Move focus to down window" $ windows W.focusDown),
    ((modm, xK_Left), addName "Move window to next workspace on current screen" $ windows W.focusDown),
    ((modm, xK_k), addName "Move focus to up window" $ windows W.focusUp),
    ((modm, xK_Right), addName "Move window to previous workspace on current screen" $ windows W.focusUp),
    ((modm .|. shiftMask, xK_j), addName "Swap focused window with down window" $ windows W.swapDown),
    ((modm .|. shiftMask, xK_Left), addName "Swap focused window with next window" $ windows W.swapDown),
    ((modm .|. shiftMask, xK_k), addName "Swap focused window with up window" $ windows W.swapUp),
    ((modm .|. shiftMask, xK_Right), addName "Swap focused window with previous window" $ windows W.swapUp),
    ((modm, xK_h), addName "Shrink master area" $ sendMessage Shrink),
    ((modm .|. controlMask, xK_Left), addName "Shrink master area" $ sendMessage Shrink),
    ((modm, xK_l), addName "Expand master area" $ sendMessage Expand),
    ((modm .|. controlMask, xK_Right), addName "Expand master area" $ sendMessage Expand),
    ((modm, xK_comma), addName "Increment number of windows in master area" $ sendMessage (IncMasterN 1)),
    ((modm, xK_period), addName "Decrease number of windows in master area" $ sendMessage (IncMasterN (-1))),
    ((modm .|. shiftMask, xK_l), addName "Logout XMonad" xmonadLogout),
    ((modm, xK_q), addName "Restart XMonad" $ spawn "xmonad --recompile; xmonad --restart"),
    ((modm, xK_minus), addName "Minimize focused Window" $ withFocused minimizeWindow),
    ((modm .|. shiftMask, xK_minus), addName "Maximize last window and focus" $ withLastMinimized maximizeWindowAndFocus)
  ]
    ^++^ [ ((m .|. modm, k), addName (displayNumber ++ i) $ windows $ onCurrentScreen f i)
           | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9],
             (f, m, displayNumber) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
         ]
    ^++^ [ ((m .|. modm, key), addName (displayNumber ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_Up, xK_Down] [0 ..],
             (f, m, displayNumber) <- [(W.view, 0, "Switch to screen number "), (W.shift, mod1Mask, "Move client to screen number ")]
         ]

myMouseBindings :: XConfig layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod + button 1, Set the window to floating mode and move by dragging
    [ ((modm, 1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
      -- button 3, Set window to tiled mode
      ((modm, 3), \w -> focus w >> withFocused (windows . W.sink)),
      -- button 8, Open menu
      ((0, 8), \w -> spawnHere "jgmenu_run"),
      -- button 9, Set the window to floating mode and resize by dragging
      ((0, 9), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster),
      -- mod + shift + button 1, Raise the window to the top of the stack
      ((modm .|. shiftMask, 1), \w -> focus w >> windows W.shiftMaster)
    ]

-- ## Layouts ## -------------------------------------------------------------------------
myLayout =
  avoidStruts . smartBorders . minimize . BW.boringWindows $
    named "Tilled" tiled ||| named "Mirror Tilled" (Mirror tiled) ||| named "Fullfilled" (noBorders Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

-- ## Window rules ## --------------------------------------------------------------------
myManageHook =
  composeAll . concat $
    [ [manageDocks],
      [isDialog --> doCenterFloat],
      [isFullscreen --> doFullFloat],
      [className =? c --> doCenterFloat | c <- myCFloats],
      [title =? t --> doSideFloat SC | t <- myTFloats],
      [resource =? layout --> doSideFloat CE | layout <- myLFloats],
      [resource =? r --> doSideFloat CW | r <- myRFloats],
      [resource =? i --> doIgnore | i <- myIgnores]
      -- , [namedScratchpadManageHook myScratchpads]
    ]
  where
    myCFloats =
      [ "alacritty-float",
        "Music",
        "MPlayer",
        "mpv",
        "Gimp",
        "feh",
        "Viewnior",
        "Gpicview",
        "Kvantum Manager",
        "qt5ct",
        "VirtualBox Manager",
        "qemu",
        "Qemu-system-x86_64",
        "Lxappearance",
        "Nitrogen",
        "Arandr",
        "Pavucontrol",
        "Xfce4-power-manager-settings",
        "Nm-connection-editor"
      ]

    myTFloats = ["Downloads", "Save As...", "About : Aditya Shakya", "Getting Started"]

    myRFloats = []

    myLFloats = ["xmessage", "Xmessage", "yad", "Yad"]

    myIgnores = ["desktop_window"]

-- ## bar config ## -----------------------------------------------------------------------
myPrettyPrinter :: ScreenId -> PP
myPrettyPrinter s =
  whenCurrentOn
    s
    def
      { ppSep = xyellow " • ",
        ppTitleSanitize = xmobarStrip,
        ppCurrent = wrap " " "" . xmobarBorder "Top" "#76cce0" 2,
        ppHidden = xwhite . wrap " " "",
        ppVisibleNoWindows = pure $ xyellow . wrap " " "",
        ppHiddenNoWindows = xlowWhite . wrap " " "",
        ppUrgent = xred . wrap (xyellow "!") (xyellow "!"),
        ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
        ppOutput = appendFile ("focus" ++ show s) . (++ "\n"),
        ppExtras = [logTitles formatFocused formatUnfocused]
      }
  where
    formatFocused = wrap (xwhite "[") (xwhite "]") . xmagenta . ppWindow . shorten 20
    formatUnfocused = wrap (xlowWhite "[") (xlowWhite "]") . xblue . ppWindow . shorten 5

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w)

    xblue, xlowWhite, xmagenta, xred, xwhite, xyellow :: String -> String
    xmagenta = xmobarColor magenta background
    xblue = xmobarColor blue background
    xwhite = xmobarColor white background
    xyellow = xmobarColor yellow background
    xred = xmobarColor red background
    xlowWhite = xmobarColor foreground background

xmobar0 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.xmonad/theme/xmobar/xmobarrc.0" $ pure (marshallPP (S 0) (myPrettyPrinter (S 0)))

xmobar1 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1 ~/.xmonad/theme/xmobar/xmobarrc.1" $ pure (marshallPP (S 1) (myPrettyPrinter (S 1)))

barSpawner :: ScreenId -> X StatusBarConfig
barSpawner 0 = pure xmobar0
barSpawner 1 = pure xmobar1
barSpawner _ = mempty

-- ## Main Function ## --------------------------------------------------------------------

-- Run xmonad with all the configs we set up.
main :: IO ()
main =
  xmonad
    $ addDescrKeys' ((mod1Mask .|. shiftMask, xK_1), showKeybindings) myKeys
    $ docks
      . ewmhFullscreen
      . ewmh
      . dynamicEasySBs barSpawner
    $ def -- configs
      { terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        clickJustFocuses = myClickJustFocuses,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = withScreens 2 myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        mouseBindings = myMouseBindings,
        -- hooks, layouts
        manageHook = myManageHook,
        layoutHook =
          renamed [Chain [CutWordsLeft 2, Prepend ("<fc=" ++ green ++ ">"), Append "</fc>"]] $
            gaps [(L, 0), (R, 0), (U, 0), (D, 0)] $
              spacingRaw
                False
                ( Border {top = 10, bottom = 0, right = 10, left = 10}
                )
                True
                ( Border {top = 0, bottom = 10, right = 0, left = 10}
                )
                True
                myLayout,
        logHook = dynamicLog,
        startupHook = myStartupHook
      }
