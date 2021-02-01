import           Data.Functor (void)
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Ratio ((%))
import           System.IO (hPutStrLn)
import           XMonad
import           XMonad.Actions.CopyWindow (kill1)
import           XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, swapNextScreen, toggleWS')
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowGo (raise)
import           XMonad.Config.Gnome (gnomeConfig)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops (ewmhDesktopsEventHook, ewmhDesktopsLogHook, ewmhDesktopsStartup)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Hooks.UrgencyHook (NoUrgencyHook (..), clearUrgents, focusUrgent, withUrgencyHook)
import           XMonad.Layout.AutoMaster (autoMaster)
import           XMonad.Layout.Grid (Grid (..))
import           XMonad.Layout.IM (Property (Role), withIM)
import           XMonad.Layout.MultiToggle ((??), EOT (..), Toggle (..), mkToggle)
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Reflect (reflectHoriz)
import           XMonad.Layout.ResizableTile (ResizableTall (..))
import           XMonad.Layout.SimpleFloat (simpleFloat)
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run (spawnPipe)

myWorkspaces :: [String]
myWorkspaces = map show [(1 :: Int) .. 9]

workSpaceN :: Int -> String
workSpaceN i = myWorkspaces !! (i -1)

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [manageHook gnomeConfig],
      [isDialog --> doFloat],
      [className =? c --> doFloat | c <- classFloats],
      [title =? t --> doFloat | t <- titleFloats],
      [resource =? r --> doFloat | r <- resourceFloats],
      [resource =? i --> doIgnore | i <- ignored],
      [className =? c --> doShift (workSpaceN 1) | c <- ws1],
      [className =? c --> doShift (workSpaceN 2) | c <- ws2],
      [className =? c --> doShift (workSpaceN 3) | c <- ws3],
      [className =? c --> doShift (workSpaceN 4) | c <- ws4],
      [className =? c --> doShift (workSpaceN 5) | c <- ws5],
      [className =? c --> doShift (workSpaceN 6) | c <- ws6],
      [className =? c --> doShift (workSpaceN 7) | c <- ws7],
      [className =? c --> doShift (workSpaceN 8) | c <- ws8],
      [className =? c --> doShift (workSpaceN 9) | c <- ws9],
      [className =? c --> doShift (workSpaceN 9) | c <- ws9],
      miscellaneous
    ]
  where
    classFloats =
      [ "Xmessage",
        "Unity-2d-launcher",
        "Vncviewer",
        "feh",
        "Gpick",
        "Ubuntu-tweak",
        "de-tud-cs-se-flashcards-Main",
        "xv",
        "mplayer2",
        "Gxmessage",
        "gxmessage",
        "de-hackermuehle-pdfpresenter-PdfPresenter",
        "gtk-recordmydesktop",
        "Gtk-recordmydesktop",
        "nethack-qt",
        "zoom",
        "sun-awt-X11-XWindowPeer"
      ]
    titleFloats =
      [ "Save As...",
        "Save File",
        "Options",
        "Document Print Status",
        "Terminator Preferences"
      ]
    resourceFloats = []
    ignored = ["Unity-2d-panel", "trayer"]
    ws1 = ["X-terminal-emulator"]
    ws2 = ["Firefox", "Vimperator", "Uzbl-tabbed"]
    ws3 = ["Zathura", ".zathura-wrapped", "Evince", "Okular", "Apvlv", "Acroread"]
    ws4 = []
    {- emacs, but no rule so frames can be opened everywhere-}
    ws5 =
      [ "Gimp-2.6",
        "Vinagre",
        "Remmina",
        "Eclipse",
        "com-install4j-runtime-Launcher",
        "jetbrains-idea-ce",
        "Scala IDE"
      ]
    ws6 = []
    ws7 = []
    ws8 = ["TelegramDesktop", "Spotify", "spotify", "Slack", "signal", "Signal"]
    ws9 = ["MPlayer", "mplayer2", "mpv"]
    miscellaneous =
      [ title =? "vmail" --> doShift (workSpaceN 7),
        fmap ("libreoffice" `isPrefixOf`) className --> doShift (workSpaceN 5)
      ]

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "lower" spawnSpLower findLower manageLower,
    NS "upper" spawnSpUpper findUpper manageUpper,
    NS "right" spawnSpRight findRight manageRight
  ]
  where
    prefix = takeWhile (/= ':')
    spawnSpLower = runTerminal "sp_lower" "@tmx@/bin/tmx sp_lower"
    findLower = (prefix <$> title) =? "sp_lower"
    manageLower = customFloating $ W.RationalRect l t w h
      where
        h = 0.4
        w = 1
        t = 1 - h -0.02
        l = (1 - w) / 2
    spawnSpUpper = runTerminal "sp_upper" "@tmx@/bin/tmx sp_upper"
    findUpper = (prefix <$> title) =? "sp_upper"
    manageUpper = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 1
        t = 0.02
        l = 0
    spawnSpRight = runTerminal "sp_right" "@tmx@/bin/tmx sp_right"
    findRight = (prefix <$> title) =? "sp_right"
    manageRight = customFloating $ W.RationalRect l t w h
      where
        h = 0.96
        w = 0.4
        t = 0.02
        l = 0.6

myTab :: Theme
myTab =
  def
    { activeColor = "black",
      activeTextColor = "orange",
      activeBorderColor = "orange",
      inactiveColor = "#4c4c4c",
      inactiveTextColor = "gray",
      inactiveBorderColor = "gray",
      urgentColor = "orange",
      urgentTextColor = "black",
      urgentBorderColor = "black",
      fontName = myFont,
      decoHeight = 14
    }

myFont :: String
myFont = "xft:DejaVu Sans-7:bold"

myMouseBindings :: p -> [((ButtonMask, Button), Window -> X ())]
myMouseBindings _ = [((myModKey, button3), \w -> focus w >> Flex.mouseWindow Flex.linear w)]

myNewMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myNewMouseBindings x = mouseBindings def x `M.union` M.fromList (myMouseBindings x)

myRemovedKeys :: [(ButtonMask, KeySym)]
myRemovedKeys =
  [ (myModKey, xK_p),
    (myModKey, xK_h),
    (myModKey, xK_l),
    (myModKey, xK_n),
    (myModKey, xK_m),
    (myModKey, xK_t)
  ]

myKeys :: [((ButtonMask, KeySym), X ())]
myKeys =
  [ ((myModCtrl, xK_Return), windows W.swapMaster)
  , ((myModCtrl, xK_e), spawn "@emacsAnywhere@/bin/emacsAnywhere")
  , ((myModCtrl, xK_l), spawn "@lockScreen@/bin/lockScreen")

  , ((myModKey, xK_BackSpace), focusUrgent)
  , ((myModKey, xK_F1), spawn "@autorandr@/bin/autorandr --load mobile")
  , ((myModKey, xK_F11), spawn "~/bin/f11")
  , ((myModKey, xK_F12), spawn "@flameshot@/bin/flameshot gui") -- NOTE: requires flameshot service to be active (nixos or home-manager)
  , ((myModKey, xK_F2), spawn "@autorandr@/bin/autorandr --change")
  , ((myModKey, xK_Return), sendMessage $ Toggle FULL)
  , ((myModKey, xK_Tab), toggleWS' ["NSP"])
  , ((myModKey, xK_a), spawn "@ddgr@/bin/ddgr --gb --ducky $(@rofi@/bin/rofi -p ddgr -dmenu -lines 0)")
  , ((myModKey, xK_b), spawn "@bukuRun@/bin/bukuRun")
  , ((myModKey, xK_d), spawn "@rofi@/bin/rofi -modi run -i -monitor -4 -matching fuzzy -sort -show run")
  , ((myModKey, xK_e), swapNextScreen)
  , ((myModKey, xK_equal), sendMessage Expand)
  , ((myModKey, xK_minus), sendMessage Shrink)
  , ((myModKey, xK_o), submap . M.fromList $ [ ((0, xK_c), raise (className =? "Chromium-browser"))
                                             , ((0, xK_f), raise (className =? "Firefox"))
                                             , ((0, xK_i), raise (className =? "jetbrains-idea-ce"))
                                             , ((0, xK_t), raise (className =? "TelegramDesktop"))
                                             , ((0, xK_s), submap . M.fromList $ [ ((0, xK_i), raise (className =? "Signal"))
                                                                                 , ((0, xK_l), raise (className =? "Slack"))
                                                                                 , ((0, xK_p), raise (className =? "Spotify"))
                                                                                 ])
                                             , ((0, xK_m), raise (className =? "mpv"))
                                             ])
  , ((myModKey, xK_p), submap . M.fromList $ [ ((0, xK_p), spawn "@playerctl@/bin/playerctl -p spotify previous")
                                             , ((0, xK_n), spawn "@playerctl@/bin/playerctl -p spotify next")
                                             , ((0, xK_space), spawn "@playerctl@/bin/playerctl -p spotify play-pause")
                                             ])
  , ((myModKey, xK_q), prevScreen >> spawn "@centerMouse@/bin/centerMouse")
  , ((myModKey, xK_s), spawn "@rofi@/bin/rofi -i -monitor -4 -matching fuzzy -sort -show window")
  , ((myModKey, xK_u), spawn "@browserHistory@/bin/browserHistory")
  , ((myModKey, xK_w), nextScreen >> spawn "@centerMouse@/bin/centerMouse")

  , ((myModShift, xK_BackSpace), clearUrgents)
  , ((myModShift, xK_l), scratchTermLower)
  , ((myModShift, xK_o), scratchTermRight)
  , ((myModShift, xK_q), kill1)
  , ((myModShift, xK_t), withFocused $ windows . W.sink)
  , ((myModShift, xK_u), scratchTermUpper)
  , ((myModShift, xK_w), shiftNextScreen)
  , ((myModShift, xK_x), spawn "@xkill@/bin/xkill")

  , ((myModShiftCtrl, xK_h), spawn "env CM_LAUNCHER=rofi CM_HISTLENGTH=20 @clipmenu@/bin/clipmenu")
  , ((myModShiftCtrl, xK_q), spawn "@xmonadReset@/bin/xmonadReset")

  -- Multimedia via Bose
  , ((0, xF86AudioPlay), spawn "@playerctl@/bin/playerctl play-pause")
  , ((0, xF86AudioPrev), spawn "@playerctl@/bin/playerctl previous")
  , ((0, xF86AudioNext), spawn "@playerctl@/bin/playerctl next")
  , ((0, xF86AudioForward), spawn "@playerctl@/bin/playerctl position +2")
  , ((0, xF86AudioRewind), spawn "@playerctl@/bin/playerctl position -2")

  -- Non-greedy workspace switching
  ] ++ [((m .|. myModKey, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  where
    scratchTermUpper = namedScratchpadAction myScratchPads "upper"
    scratchTermLower = namedScratchpadAction myScratchPads "lower"
    scratchTermRight = namedScratchpadAction myScratchPads "right"
    xF86AudioLowerVolume = 0x1008ff11
    xF86AudioMute = 0x1008ff12
    xF86AudioRaiseVolume = 0x1008ff13
    xF86AudioPlay = 0x1008ff14
    xF86AudioStop = 0x1008ff15
    xF86AudioPrev = 0x1008ff16
    xF86AudioNext = 0x1008ff17
    xF86AudioForward = 0x1008ff97
    xF86AudioRewind = 0x1008ff3e

myModKey :: ButtonMask
myModKey = mod4Mask

myModShift :: ButtonMask
myModShift = myModKey .|. shiftMask

myModCtrl :: ButtonMask
myModCtrl = myModKey .|. controlMask

myModShiftCtrl :: ButtonMask
myModShiftCtrl = myModKey .|. controlMask .|. shiftMask

myLayoutHook =
  mkToggle (NOBORDERS ?? FULL ?? EOT)
    $ avoidStruts . smartBorders
    $ onWorkspace
      (workSpaceN 5)
      (standardLayouts ||| gimpLayout)
      standardLayouts

standardLayouts = tabLayout ||| myTall ||| tiled ||| autoMasterLayout Grid ||| Grid ||| Full ||| simpleFloat
  where
    myTall = Tall 1 (3 % 100) (1 % 2)

autoMasterLayout = autoMaster 1 (1 / 50)

tiled :: ResizableTall t
tiled = ResizableTall 1 (1 / 50) (3 / 4) []

tabLayout = tabbed shrinkText myTab

gimpLayout =
  withIM 0.11 (Role "gimp-toolbox")
    $ reflectHoriz
    $ withIM 0.15 (Role "gimp-dock") Full

workspaceRenamer :: String -> String
workspaceRenamer x = case x of
  "Tabbed Simplest" -> "tabbed"
  "Mosaic" -> "mosaic"
  "Grid" -> "grid"
  "Accordion" -> "accord"
  "Simple Float" -> "sFloat"
  "Full" -> "full"
  "ResizableTall" -> "rTall"
  "IM ReflectX IM Tabbed Simplest" -> "gimp"
  "ReflectX IM Tabbed Simplest" -> "mTabbed"
  "ReflectX IM Grid" -> "mGrid"
  "ReflectX IM Circle" -> "mCircle"
  "IM ReflectX IM Full" -> "Gimp"
  "ReflectX IM Spiral" -> "mSprial"
  _ -> x

myTerminal :: String
myTerminal = "urxvt"

runTerminal :: String -> String -> String
runTerminal termTitle arg =
  unwords [myTerminal, "-title", termTitle, "-e", "bash", "-c", "'" ++ arg ++ "'"]

-- like the standard ewmh, but don't focus (damn it)
ewmhSupport :: XConfig a -> XConfig a
ewmhSupport c =
  c
    { startupHook = startupHook c <> ewmhDesktopsStartup,
      handleEventHook = handleEventHook c <> ewmhDesktopsEventHook,
      logHook = logHook c <> ewmhDesktopsLogHook
    }

main :: IO ()
main = do
  xmobarBottom <- spawnPipe "@xmobar@/bin/xmobar @xmobarLower@"
  void $ spawnPipe "@xmobar@/bin/xmobar @xmobarUpper@"
  xmonad $ ewmhSupport $ docks $ withUrgencyHook NoUrgencyHook $
    def
      { workspaces = myWorkspaces,
        manageHook =
          manageHook def
            <+> myManageHook
            <+> namedScratchpadManageHook myScratchPads,
        borderWidth = 2,
        focusFollowsMouse = False,
        terminal = myTerminal,
        focusedBorderColor = "orange",
        layoutHook = avoidStruts myLayoutHook,
        startupHook = setWMName "LG3D",
        logHook =
          dynamicLogWithPP
            xmobarPP
              { ppOutput = hPutStrLn xmobarBottom,
                ppTitle = xmobarColor "orange" "",
                ppUrgent = xmobarColor "black" "orange" . xmobarStrip,
                ppVisible = xmobarColor "red" "black",
                ppCurrent = xmobarColor "orange" "black",
                ppWsSep = " | ",
                ppSep = " | ",
                ppLayout = xmobarColor "gray" "black" . workspaceRenamer
              },
        modMask = myModKey,
        mouseBindings = myNewMouseBindings
      }
      `removeKeys` myRemovedKeys
      `additionalKeys` myKeys
