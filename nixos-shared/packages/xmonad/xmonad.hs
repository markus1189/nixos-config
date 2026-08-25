import Control.Monad (filterM, when)
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.List (isInfixOf, isPrefixOf)
import Data.Map qualified as M
import Data.Monoid (All (..))
import Data.Ratio ((%))
import Data.Set qualified as S
import XMonad
  ( Button,
    ButtonMask,
    Default (def),
    Event (DestroyWindowEvent, MapNotifyEvent, UnmapEvent, ev_window),
    ExtensionClass (..),
    Full (Full),
    KeySym,
    Layout,
    ManageHook,
    MonadIO (liftIO),
    Query,
    Rectangle (..),
    Resize (Expand, Shrink),
    Tall (Tall),
    Window,
    X,
    XConfig
      ( borderWidth,
        focusFollowsMouse,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        manageHook,
        modMask,
        mouseBindings,
        startupHook,
        terminal,
        workspaces
      ),
    appName,
    asks,
    button3,
    className,
    composeAll,
    controlMask,
    doFloat,
    doIgnore,
    doShift,
    focus,
    mod1Mask,
    mod4Mask,
    queryTree,
    resource,
    runQuery,
    sendMessage,
    shiftMask,
    spawn,
    stringProperty,
    theRoot,
    title,
    whenX,
    windows,
    withDisplay,
    withFocused,
    xK_0,
    xK_1,
    xK_9,
    xK_BackSpace,
    xK_F1,
    xK_F10,
    xK_F11,
    xK_F12,
    xK_F2,
    xK_Insert,
    xK_Return,
    xK_Super_L,
    xK_Tab,
    xK_a,
    xK_b,
    xK_c,
    xK_d,
    xK_e,
    xK_grave,
    xK_h,
    xK_l,
    xK_m,
    xK_minus,
    xK_n,
    xK_o,
    xK_p,
    xK_q,
    xK_r,
    xK_s,
    xK_semicolon,
    xK_space,
    xK_t,
    xK_u,
    xK_w,
    xK_x,
    xmonad,
    (-->),
    (.|.),
    (=?),
    (|||),
  )
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.CycleRecentWS (cycleWindowSets)
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, swapNextScreen, toggleWS')
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, removeEmptyWorkspace)
import XMonad.Actions.FlexibleManipulate qualified as Flex
import XMonad.Actions.GroupNavigation (Direction (Backward, Forward), nextMatchWithThis)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.WindowBringer (bringWindow, gotoMenuArgs')
import XMonad.Actions.WindowGo (raise)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Core (ScreenDetail (screenRect), WindowSet, WindowSpace, WorkspaceId, withWindowSet)
import XMonad.Hooks.DynamicLog
  ( PP
      ( ppCurrent,
        ppLayout,
        ppSep,
        ppTitle,
        ppUrgent,
        ppVisible,
        ppWsSep
      ),
    xmobarColor,
    xmobarPP,
    xmobarStrip,
    xmonadPropLog',
  )
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, setEwmhActivateHook)
import XMonad.Hooks.ManageDocks (avoidStruts, calcGap, docks)
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarGeneric, statusBarProp, withSB)
import XMonad.Hooks.UrgencyHook
  ( NoUrgencyHook (..),
    SuppressWhen (Focused),
    UrgencyConfig (suppressWhen),
    clearUrgents,
    doAskUrgent,
    focusUrgent,
    withUrgencyHookC,
  )
import XMonad.Layout.AutoMaster (autoMaster)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.FocusTracking (focusTracking)
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.IM (Property (Role), withIM)
import XMonad.Layout.MultiToggle (EOT (..), Toggle (..), mkToggle, (??))
import XMonad.Layout.MultiToggle.Instances
  ( StdTransformers (FULL, NOBORDERS),
  )
import XMonad.Layout.NoBorders (hasBorder, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Tabbed
  ( Theme
      ( activeBorderColor,
        activeColor,
        activeTextColor,
        decoHeight,
        fontName,
        inactiveBorderColor,
        inactiveColor,
        inactiveTextColor,
        urgentBorderColor,
        urgentColor,
        urgentTextColor
      ),
    shrinkText,
    tabbed,
  )
import XMonad.ManageHook qualified as MH
import XMonad.Prompt.Window (allWindows)
import XMonad.StackSet qualified as W
import XMonad.Util.Dmenu (menuArgs)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeys)
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
  )

myWorkspaces :: [String]
myWorkspaces = map show ([(1 :: Int) .. 9] ++ [0]) ++ myNamedWorkspaces

myNamedWorkspaces :: [String]
myNamedWorkspaces = [workspaceZwift, workspaceSauce]

workspaceZwift :: String
workspaceZwift = "zwift"

workspaceSauce :: String
workspaceSauce = "sauce"

workSpaceN :: Int -> String
workSpaceN i = myWorkspaces !! (i - 1)

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [manageHook gnomeConfig],
      [isDialog --> doFloat],
      [("Groups - Sauce for Zwift" `isInfixOf`) <$> wmName --> hasBorder False <> doShift workspaceZwift <> doFloat],
      [("O101: Progress Bar" `isInfixOf`) <$> wmName --> hasBorder False <> doShift workspaceZwift <> doFloat],
      [("Sauce for Zwift" `isPrefixOf`) <$> wmName --> doShift workspaceSauce <> doFloat],
      [("Route Profile with segments" `isPrefixOf`) <$> wmName --> hasBorder False <> doShift workspaceSauce],
      [wmName =? "Mod: Nearby Athletes" --> doShift workspaceSauce],
      [MH.className =? c --> doFloat | c <- classFloats],
      [MH.title =? t --> doFloat | t <- titleFloats],
      [stringProperty "WM_NAME" =? t --> doIgnore | t <- windowNameIgnores],
      [resource =? r --> doFloat | r <- resourceFloats],
      [resource =? i --> doIgnore | i <- ignored],
      [MH.className =? c --> doShift (workSpaceN 1) | c <- ws1],
      [MH.className =? c --> doShift (workSpaceN 2) | c <- ws2],
      [MH.className =? c --> doShift (workSpaceN 3) | c <- ws3],
      [MH.className =? c --> doShift (workSpaceN 4) | c <- ws4],
      [MH.className =? c --> doShift (workSpaceN 5) | c <- ws5],
      [MH.className =? c --> doShift (workSpaceN 6) | c <- ws6],
      [MH.className =? c --> doShift (workSpaceN 7) | c <- ws7],
      [MH.className =? c --> doShift (workSpaceN 8) | c <- ws8],
      [MH.className =? c --> doShift (workSpaceN 9) | c <- ws9],
      -- Autostarted windows share their class with windows that must stay
      -- placeable by hand (every other ghostty, every other emacs frame), so
      -- they are matched on the WM_CLASS instance name their unit gives them:
      -- ghostty --x11-instance-name=, emacsclient -F '((name . ...))'.
      -- See my.xmonadAutostart in laptop/home.nix.
      [resource =? r --> doShift (workSpaceN 1) | r <- ws1Resources],
      [resource =? r --> doShift (workSpaceN 4) | r <- ws4Resources]
    ]
  where
    wmName = MH.title
    classFloats =
      [ "Xmessage",
        "Unity-2d-launcher",
        "Vncviewer",
        "feh",
        "flameshot",
        "Gpick",
        "Ubuntu-tweak",
        "xv",
        "mplayer2",
        "Gxmessage",
        "jklgxmessage",
        "de-hackermuehle-pdfpresenter-PdfPresenter",
        "gtk-recordmydesktop",
        "Gtk-recordmydesktop",
        "nethack-qt",
        "zoom",
        "sun-awt-X11-XWindowPeer",
        ".scrcpy-wrapped",
        "Emulator",
        "qemu-system-x86_64"
      ]
    titleFloats =
      [ "Save As...",
        "Save File",
        "Options",
        "Document Print Status",
        "Terminator Preferences",
        "Microsoft Teams Notification"
      ]
    windowNameIgnores =
      [ "NormCap"
      ]
    resourceFloats = []
    ignored = ["Unity-2d-panel", "trayer"]
    ws1 = ["X-terminal-emulator"]
    ws1Resources = ["ws1-default"]
    -- "firefox" is what current Firefox actually sets; "Firefox" is kept for
    -- older builds and forks.
    ws2 = ["firefox", "Firefox", "Vimperator", "Uzbl-tabbed"]
    ws3 = ["Zathura", ".zathura-wrapped", "Evince", "Okular", "Apvlv", "Acroread", "sioyek", "com.github.johnfactotum.Foliate", "KOReader"]
    ws4 = []
    {- emacs, but no class rule so frames can be opened everywhere; only the
       autostarted frame is pinned, via its instance name -}
    ws4Resources = ["emacs-main"]
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
    ws7 = ["MPlayer", "mplayer2", "mpv"]
    ws8 = ["TelegramDesktop", "Spotify", "spotify", "Slack", "signal", "Signal"]
    ws9 = ["teams-for-linux"]

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "lower" spawnSpLower findLower manageLower,
    NS "upper" spawnSpUpper findUpper manageUpper,
    NS "right" spawnSpRight findRight manageRight
  ]
  where
    prefix = takeWhile (/= ':')
    spawnSpLower = runTerminal "sp_lower" "@tmx@/bin/tmx sp_lower"
    findLower = (prefix <$> MH.title) =? "sp_lower"
    manageLower = customFloating $ W.RationalRect l t w h
      where
        h = 0.4
        w = 1
        t = 1 - h - 0.02
        l = (1 - w) / 2
    spawnSpUpper = runTerminal "sp_upper" "@tmx@/bin/tmx sp_upper"
    findUpper = (prefix <$> MH.title) =? "sp_upper"
    manageUpper = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 1
        t = 0.02
        l = 0
    spawnSpRight = runTerminal "sp_right" "@tmx@/bin/tmx sp_right"
    findRight = (prefix <$> MH.title) =? "sp_right"
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
      decoHeight = 24
    }

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Bold:10"

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

recentNonVisibleWS :: (WindowSpace -> Bool) -> WindowSet -> [WorkspaceId]
recentNonVisibleWS p w =
  map W.tag $
    filter p $
      W.hidden w ++ [W.workspace (W.current w)]

isWindowSpaceInteresting :: WindowSpace -> Bool
isWindowSpaceInteresting = (&&) <$> notNSP <*> isNotEmpty
  where
    isNotEmpty = not . null . W.stack
    notNSP w = W.tag w /= "NSP"

myKeys :: [((ButtonMask, KeySym), X ())]
myKeys =
  [ ((myModCtrl, xK_Return), windows W.swapMaster),
    ((myModCtrl, xK_e), spawn "@emacsAnywhere@/bin/emacsAnywhere"),
    ((myModCtrl, xK_l), spawn "@lockScreen@/bin/lockScreen"),
    ((myModKey, xK_BackSpace), focusUrgent),
    ((myModKey, xK_F1), spawn "@autorandr@/bin/autorandr --load mobile"),
    ((myModKey, xK_Insert), spawn "@recordScript@ toggle"),
    ((0, xF86Search), spawn "@recordScript@ toggle"),
    -- Dunst
    ((myModKey, xK_F10), spawn "@dunst@/bin/dunstctl set-paused toggle"),
    ((controlMask, xK_grave), spawn "@dunst@/bin/dunstctl close"),
    ((shiftMask .|. controlMask, xK_grave), spawn "@dunst@/bin/dunstctl history-pop"),
    ((mod1Mask .|. controlMask, xK_grave), spawn "@dunst@/bin/dunstctl context"),
    -- Warpd
    ((shiftMask .|. controlMask, xK_semicolon), spawn "@warpd@/bin/warpd --history --oneshot --click 1"),
    ( (myModKey, xK_F11),
      withFocused
        ( \w -> do
            (title, appName, className) <- runQuery ((,,) <$> MH.title <*> MH.appName <*> MH.className) w
            spawn ("~/bin/f11 '" <> filter (/= '\'') title <> "' '" <> filter (/= '\'') appName <> "' '" <> filter (/= '\'') className <> "'")
        )
    ),
    ((myModKey, xK_F12), spawn "@flameshot@/bin/flameshot gui"), -- NOTE: requires flameshot service to be active (nixos or home-manager)
    ((myModShift, xK_F12), spawn "@flameshotOcr@/bin/flameshotOcr"),
    ((myModKey, xK_F2), spawn "@autorandr@/bin/autorandr --change"),
    ((myModKey, xK_Return), sendMessage $ Toggle FULL),
    -- Tabbing
    --- Next like this
    ((myModShift, xK_Tab), nextMatchWithThis Forward MH.className),
    ((myModShift, xK_grave), nextMatchWithThis Backward MH.className),
    --- Switching
    ((myModShiftCtrl, xK_Tab), cycleWindowSets (recentNonVisibleWS isWindowSpaceInteresting) [xK_Super_L] xK_Tab xK_grave),
    ((myModKey, xK_Tab), toggleWS' ["NSP"]),
    --
    -- dmenu-style leader: super+d then r/s/c/t/d/b
    ( (myModKey, xK_d),
      submap . M.fromList $
        [ ((0, xK_r), spawn "@rofi@/bin/rofi -modi run -i -monitor -4 -matching fuzzy -sort -show run"),
          ((0, xK_s), gotoMenuArgs' "@rofi@/bin/rofi" rofiWindowArgs),
          ((0, xK_c), spawn "@clipcat@/bin/clipcat-menu insert"),
          ((0, xK_t), spawn "@rofiStuffTodayPicker@/bin/rofiStuffTodayPicker"),
          ((0, xK_d), spawn "@rofiDownloadsPicker@/bin/rofiDownloadsPicker"),
          ((0, xK_b), spawn "@bukuRun@/bin/bukuRun")
        ]
    ),
    ((myModKey, xK_e), swapNextScreen),
    ((myModKey, xK_grave), maximizeAcrossScreens),
    ((myModKey, xK_minus), sendMessage Shrink),
    ((myModShift, xK_minus), sendMessage Expand),
    ( (myModKey, xK_p),
      submap . M.fromList $
        [ ((0, xK_p), spawn "@playerctl@/bin/playerctl -p spotify previous"),
          ((0, xK_n), spawn "@playerctl@/bin/playerctl -p spotify next"),
          ((0, xK_space), spawn "@playerctl@/bin/playerctl -p spotify play-pause"),
          ((0, xK_m), spawn "@pamixer@/bin/pamixer -t")
        ]
    ),
    ((myModKey, xK_w), nextScreen'),
    ((myModShift, xK_BackSpace), clearUrgents),
    ((myModShift, xK_l), scratchTermLower),
    ((myModShift, xK_o), scratchTermRight),
    ((myModShift, xK_q), kill1),
    ((myModShift, xK_t), withFocused $ windows . W.sink),
    ((myModShift, xK_u), scratchTermUpper),
    ((myModShift, xK_w), shiftNextScreen),
    ((myModShift, xK_x), spawn "@xkill@/bin/xkill"),
    -- Dynamic workspaces
    ((myModKey, xK_n), rofiWorkspaceSwitch),
    ((myModShift, xK_n), rofiWorkspaceShift),
    ((myModCtrl, xK_n), removeEmptyWorkspace),
    ((myModShiftCtrl, xK_q), spawn "@xmonadReset@/bin/xmonadReset"),
    -- Copy to all, kill again
    ((myModKey, xK_a), windows copyToAll),
    ((myModCtrl, xK_a), killAllOtherCopies),
    -- Multimedia via Bose
    ((0, xF86AudioPlay), spawn "@playerctl@/bin/playerctl play-pause"),
    ((0, xF86AudioPrev), spawn "@playerctl@/bin/playerctl previous"),
    ((0, xF86AudioNext), spawn "@playerctl@/bin/playerctl next"),
    ((0, xF86AudioForward), spawn "@playerctl@/bin/playerctl position +2"),
    ((0, xF86AudioRewind), spawn "@playerctl@/bin/playerctl position -2")
  ]
    -- Non-greedy workspace switching with mod+<num>, greedy with mod+ctrl+<num>
    ++ [ ((m .|. myModKey, k), windows $ f i)
       | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0]),
         (f, m) <- [(W.view, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]
       ]
  where
    scratchTermUpper = namedScratchpadAction myScratchPads "upper"
    scratchTermLower = namedScratchpadAction myScratchPads "lower"
    scratchTermRight = namedScratchpadAction myScratchPads "right"
    -- use xev to find keysym
    xF86AudioLowerVolume = 0x1008ff11
    xF86AudioMute = 0x1008ff12
    xF86AudioRaiseVolume = 0x1008ff13
    xF86AudioPlay = 0x1008ff14
    xF86AudioStop = 0x1008ff15
    xF86AudioPrev = 0x1008ff16
    xF86AudioNext = 0x1008ff17
    xF86AudioForward = 0x1008ff97
    xF86AudioRewind = 0x1008ff3e
    xF86Search = 0x1008ff1b

-- Span the focused window across every screen -- on the docked ultrawide
-- that is both MST tiles of the single physical panel -- minus whatever the
-- docks reserve. Two things it deliberately does not do by hand:
--
--   * the bar heights come from calcGap, i.e. from the struts the bars
--     actually set. Hardcoding them drifts silently: neither xmobarrc
--     declares a height, so it follows the font, and the last font change
--     left the old constant five pixels off for years.
--   * the geometry goes into the StackSet via W.float. A raw
--     moveResizeWindow is undone by the next refresh, which re-derives
--     every float from its stored RationalRect -- and re-tiles anything
--     that was never floated to begin with.
--
-- The rect is stored relative to the *current* screen, so its width is >1
-- whenever the desktop spans several. scaleRationalRect does not clamp,
-- which is what makes that legal.
maximizeAcrossScreens :: X ()
maximizeAcrossScreens = withFocused $ \w -> do
  gap <- calcGap (S.fromList [minBound .. maxBound])
  desktop <- withWindowSet (return . boundingBox . map (screenRect . W.screenDetail) . W.screens)
  here <- withWindowSet (return . screenRect . W.screenDetail . W.current)
  windows $ W.float w (relativeTo here (gap desktop))

-- | Smallest rectangle containing all of the given ones. Total in practice:
-- W.screens is current:visible and so never empty.
boundingBox :: [Rectangle] -> Rectangle
boundingBox rs = Rectangle x0 y0 (fromIntegral (x1 - x0)) (fromIntegral (y1 - y0))
  where
    x0 = minimum [rect_x r | r <- rs]
    y0 = minimum [rect_y r | r <- rs]
    x1 = maximum [rect_x r + fromIntegral (rect_width r) | r <- rs]
    y1 = maximum [rect_y r + fromIntegral (rect_height r) | r <- rs]

-- | Express an absolute rectangle in the fractional coordinates W.float
-- wants, relative to the screen the window is on.
relativeTo :: Rectangle -> Rectangle -> W.RationalRect
relativeTo (Rectangle sx sy sw sh) (Rectangle x y w h) =
  W.RationalRect
    (toInteger (x - sx) % toInteger sw)
    (toInteger (y - sy) % toInteger sh)
    (toInteger w % toInteger sw)
    (toInteger h % toInteger sh)

-- Window switcher via rofi. -dmenu, not rofi's own -show window: that mode
-- focuses its pick by sending _NET_ACTIVE_WINDOW, which setEwmhActivateHook
-- turns into an urgency flag rather than focus. WindowBringer feeds the
-- window list in on stdin and focuses with W.focusWindow instead.
rofiWindowArgs :: [String]
rofiWindowArgs = ["-dmenu", "-i", "-monitor", "-4", "-matching", "fuzzy", "-sort", "-p", "window"]

-- Dynamic workspaces via rofi
rofiArgs :: [String]
rofiArgs = ["-dmenu", "-i", "-monitor", "-4", "-matching", "fuzzy", "-sort", "-p", "workspace"]

numberedWorkspaces :: [WorkspaceId]
numberedWorkspaces = map show ([(0 :: Int) .. 9])

allWorkspaceNames :: X [WorkspaceId]
allWorkspaceNames = withWindowSet (return . map W.tag . W.workspaces)

-- Show named + dynamic workspaces in rofi (exclude numbered 0-9 and NSP)
extraWorkspaceNames :: X [WorkspaceId]
extraWorkspaceNames = filter (\w -> w `notElem` numberedWorkspaces && w /= "NSP") <$> allWorkspaceNames

rofiWorkspaceSwitch :: X ()
rofiWorkspaceSwitch = do
  ws <- extraWorkspaceNames
  choice <- menuArgs "@rofi@/bin/rofi" rofiArgs ws
  case choice of
    "" -> return ()
    name -> do
      exists <- elem name <$> allWorkspaceNames
      if exists
        then windows $ W.greedyView name
        else addHiddenWorkspace name >> windows (W.greedyView name)

rofiWorkspaceShift :: X ()
rofiWorkspaceShift = do
  ws <- extraWorkspaceNames
  choice <- menuArgs "@rofi@/bin/rofi" rofiArgs ws
  case choice of
    "" -> return ()
    name -> do
      exists <- elem name <$> allWorkspaceNames
      if exists
        then windows $ W.shift name
        else addHiddenWorkspace name >> windows (W.shift name)

-- use "xprop"
myKeysP :: [(String, X ())]
myKeysP =
  [ (myModKeyP "o 1 p", raise (iclassName "1password")),
    (myModKeyP "o c h", raise (iclassName "chromium-browser")),
    (myModKeyP "o e m", raise (iclassName "emacs")),
    (myModKeyP "o d i", raise (iclassName "discord")),
    (myModKeyP "o f i", raise (iclassName "firefox")),
    (myModKeyP "o i n", raise ((||) <$> iclassName "jetbrains-idea-ce" <*> iclassName "jetbrains-idea")),
    (myModKeyP "o i m", raise ((&&) <$> iclassName "com.mitchellh.ghostty" <*> (MH.title <&> ("im:" `isPrefixOf`)))),
    (myModKeyP "o t e", raise (iclassName "telegramdesktop")),
    (myModKeyP "o m s", raise (ititleContains "microsoft teams")),
    (myModKeyP "o s i", raise (iclassName "signal")),
    (myModKeyP "o s l", raise (iclassName "slack")),
    (myModKeyP "o s p", raise (iclassName "spotify")),
    (myModKeyP "o m p", raise (iclassName "mpv")),
    (myModKeyP "o z o", raise (ititle "meeting")),
    (myModKeyP "z g", raise (ititle "meeting")),
    (myModKeyP "z z", spawn "zoom 'zoommtg://zoom.us/join?action=join&confno=2387012688'"),
    (myModKeyP "z b", bringAllWindowsByClass "zoom"),
    (myModKeyP "z w", bringAllWindowsByClass "zoom" >> nextScreen'),
    (myModKeyP "z a", spawn "@xdotool@/bin/xdotool search --name 'Meeting' --name 'as_toolbar' windowactivate --sync key alt+a windowactivate --sync \"$(@xdotool@/bin/xdotool getactivewindow)\""),
    (myModKeyP "z e", bringAllWindowsByClass "zoom" >> swapNextScreen')
  ]
  where
    iclassName cls = MH.className <&> (cls ==) . map toLower
    ititle n = MH.title <&> (n ==) . map toLower
    ititleContains n = (n `isInfixOf`) . map toLower <$> MH.title

nextScreen' :: X ()
nextScreen' = nextScreen >> spawn "@centerMouse@/bin/centerMouse"

swapNextScreen' :: X ()
swapNextScreen' = swapNextScreen >> spawn "@centerMouse@/bin/centerMouse"

bringAllWindowsByClass :: String -> X ()
bringAllWindowsByClass cls = do
  ws <- M.elems <$> allWindows
  zoomWindows <- filterM (runQuery (MH.className =? cls)) ws
  let f = foldl' (\acc w -> bringWindow w . acc) id zoomWindows
  windows f

myModKey :: ButtonMask
myModKey = mod4Mask

myModKeyP :: String -> String
myModKeyP = ("M4-" <>)

myModShift :: ButtonMask
myModShift = myModKey .|. shiftMask

myModCtrl :: ButtonMask
myModCtrl = myModKey .|. controlMask

myModShiftCtrl :: ButtonMask
myModShiftCtrl = myModKey .|. controlMask .|. shiftMask

myLayoutHook =
  mkToggle (NOBORDERS ?? FULL ?? EOT) $
    avoidStruts . smartBorders . focusTracking $
      onWorkspace
        (workSpaceN 5)
        (standardLayouts ||| gimpLayout)
        standardLayouts

standardLayouts = tabLayout ||| emptyBSP ||| myTall ||| tiled ||| autoMasterLayout Grid ||| Grid ||| Full ||| simpleFloat
  where
    myTall = Tall 1 (3 % 100) (1 % 2)

autoMasterLayout = autoMaster 1 (1 / 50)

tiled :: ResizableTall t
tiled = ResizableTall 1 (1 / 50) (3 / 4) []

tabLayout = tabbed shrinkText myTab

gimpLayout =
  withIM 0.11 (Role "gimp-toolbox") $
    reflectHoriz $
      withIM 0.15 (Role "gimp-dock") Full

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
myTerminal = "@ghostty@/bin/ghostty"

runTerminal :: String -> String -> String
runTerminal termTitle arg =
  unwords [myTerminal, "--title=" ++ termTitle, "-e", "bash", "-c", "'" ++ arg ++ "'"]

myLowerPP :: PP
myLowerPP =
  xmobarPP
    { ppTitle = xmobarColor "orange" "",
      ppUrgent = xmobarColor "black" "orange" . xmobarStrip,
      ppVisible = xmobarColor "red" "black",
      ppCurrent = xmobarColor "orange" "black",
      ppWsSep = " | ",
      ppSep = " | ",
      ppLayout = xmobarColor "gray" "black" . workspaceRenamer
    }

-- graphical-session.target is already active before the session wrapper execs
-- xmonad, so the autostarted programs hang off xmonad-session.target instead
-- and this is what activates it: the WM announcing its own readiness.
--
-- Guarded, because `systemctl start` on a target starts every *inactive* unit
-- it wants, every time. A program that hands off to an already running
-- instance (spotify, slack, a second ghostty) exits immediately and leaves its
-- unit inactive, so an unguarded start would spawn it afresh on every mod-q
-- restart. The target has no processes of its own and so stays active for the
-- whole session, which makes it the latch: autostart runs once per login, not
-- once per xmonad start.
startAutostart :: X ()
startAutostart =
  spawn . unwords $
    [ systemctl, "is-active", "--quiet", target,
      "||",
      systemctl, "start", "--no-block", target
    ]
  where
    systemctl = "@systemd@/bin/systemctl --user"
    target = "xmonad-session.target"

-- Screen sharing indicator, read by xmobar's NamedXPropertyLog.
--
-- Replaces a Com plugin that shelled out to xdotool twice every two seconds
-- (~15.6ms a poll) to ask a question xmonad is already told the answer to.
sharingProp :: String
sharingProp = "_XMONAD_SHARING"

-- The value goes verbatim into xmobar's template, so it carries its own
-- markup. NamedXPropertyLog is the action-stripping reader: <fc> survives,
-- and an <action> smuggled in through a window title would not.
renderSharing :: S.Set Window -> String
renderSharing s
  | S.null s = ""
  | otherwise = "<fc=red>\9210SHARING\9210</fc> "

-- A set rather than a Bool: a call maps several matching windows at once
-- (the notification and the toolbar) and tears them down independently.
newtype SharingWindows = SharingWindows (S.Set Window)

instance ExtensionClass SharingWindows where
  initialValue = SharingWindows S.empty

-- The four fields a bare `xdotool search` matches against: --name
-- --classname --class --role. Keeping the set identical means this decides
-- exactly what the shell script it replaces decided.
isSharingWindow :: Query Bool
isSharingWindow =
  any (\field -> any (`isInfixOf` field) needles)
    <$> sequence [title, appName, className, stringProperty "WM_WINDOW_ROLE"]
  where
    needles = ["is sharing", "as_toolbar"]

modifySharing :: (S.Set Window -> S.Set Window) -> X ()
modifySharing f = do
  SharingWindows before <- XS.get
  let after = f before
  XS.put (SharingWindows after)
  -- Only write when the rendered value actually changes. Every write is a
  -- PropertyNotify and therefore an xmobar redraw, and MapNotify fires far
  -- more often than a screen share starts.
  when (renderSharing before /= renderSharing after) $
    xmonadPropLog' sharingProp (renderSharing after)

-- Driven by map/unmap/destroy rather than by walking the window set. xmonad
-- selects substructureNotifyMask on the root (XMonad.Config.rootMask) and
-- handleWithHook runs handleEventHook ahead of core's own handler, so these
-- events arrive for override-redirect windows too -- which is the whole
-- point. On this display xdotool sees 88 windows where _NET_CLIENT_LIST has
-- 8, and an always-on-top sharing toolbar is exactly the sort of window that
-- lives in the other 80 and never enters the StackSet.
sharingEventHook :: Event -> X All
sharingEventHook ev = do
  case ev of
    MapNotifyEvent {ev_window = w} ->
      whenX (runQuery isSharingWindow w) $ modifySharing (S.insert w)
    UnmapEvent {ev_window = w} -> modifySharing (S.delete w)
    DestroyWindowEvent {ev_window = w} -> modifySharing (S.delete w)
    _ -> pure ()
  pure (All True)

-- ExtensibleState does not survive mod-q but the X property does, so without
-- this a restart mid-call would leave the indicator stuck on (or, worse,
-- stuck off). One tree walk per xmonad start; it is the only walk left.
rescanSharing :: X ()
rescanSharing = do
  root <- asks theRoot
  wins <- withDisplay $ \d -> liftIO (queryTree d root) <&> \(_, _, cs) -> cs
  matches <- S.fromList <$> filterM (runQuery isSharingWindow) wins
  XS.put (SharingWindows matches)
  xmonadPropLog' sharingProp (renderSharing matches)

-- withSB tracks bar PIDs in persistent state and its startup hook kills
-- stale instances before respawning, so bars survive mod-q restarts without
-- the old spawnPipe/StdinReader EOF trick. The lower bar reads the workspace
-- log from the _XMONAD_LOG property (Run XMonadLog on the xmobar side).
myStatusBars :: StatusBarConfig
myStatusBars =
  statusBarProp "@xmobar@/bin/xmobar @xmobarLower@" (pure myLowerPP)
    <> statusBarGeneric "@xmobar@/bin/xmobar @xmobarUpper@" mempty

main :: IO ()
main =
  xmonad . withSB myStatusBars $
    setEwmhActivateHook doAskUrgent . ewmhFullscreen . ewmh $
      docks $
        withUrgencyHookC NoUrgencyHook (def {suppressWhen = Focused}) $
          def
            { workspaces = myWorkspaces,
              manageHook =
                manageHook def <> myManageHook <> namedScratchpadManageHook myScratchPads,
              borderWidth = 2,
              focusFollowsMouse = False,
              terminal = myTerminal,
              focusedBorderColor = "orange",
              layoutHook = avoidStruts myLayoutHook,
              startupHook = setWMName "LG3D" <> startAutostart <> rescanSharing,
              handleEventHook = sharingEventHook,
              modMask = myModKey,
              mouseBindings = myNewMouseBindings
            }
            `removeKeys` myRemovedKeys
            `additionalKeys` myKeys
            `additionalKeysP` myKeysP
