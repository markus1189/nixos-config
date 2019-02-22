{-# OPTIONS -fno-warn-missing-signatures #-}

import           Control.Monad (unless)
import           Data.Char (toLower)
import           Data.Function (on)
import           Data.Functor (void)
import           Data.List (isInfixOf, isPrefixOf, intercalate)
import qualified Data.Map as M
import           Data.Monoid ((<>), All(..))
import           Data.Ratio ((%))
import           System.IO (hPutStrLn)
import           Text.Printf (printf)

import           XMonad
import           XMonad.Actions.CopyWindow (kill1)
import           XMonad.Actions.CycleWS (toggleWS', swapNextScreen, nextScreen, shiftNextScreen)
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Config.Gnome (gnomeConfig)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup, ewmhDesktopsEventHook, ewmhDesktopsLogHook)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Hooks.UrgencyHook (focusUrgent, clearUrgents, withUrgencyHook, NoUrgencyHook(..))
import           XMonad.Layout (Tall(..))
import           XMonad.Layout.AutoMaster (autoMaster)
import           XMonad.Layout.Grid (Grid(..))
import           XMonad.Layout.IM (withIM, Property(Role))
import           XMonad.Layout.MultiToggle (mkToggle, (??), EOT(..), Toggle(..))
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Reflect (reflectHoriz)
import           XMonad.Layout.ResizableTile (ResizableTall(..))
import           XMonad.Layout.SimpleFloat (simpleFloat)
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Window (windowPromptGoto)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run (spawnPipe)

myWorkspaces :: [String]
myWorkspaces = zipWith (printf " %d:%s ") [(1::Int)..] workSpaceNames

workSpaceNames :: [String]
workSpaceNames = [ "SHELL"
                 , "WEB"
                 , "PDF"
                 , "EMACS"
                 , "MISC"
                 , "SPACE"
                 , "MAIL"
                 , "IM"
                 , "VID"
                 ]

workSpaceN :: Int -> String
workSpaceN i = myWorkspaces !! (i-1)

myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ [manageHook gnomeConfig]
  , [isDialog       --> doFloat]
  , [className =? c --> doFloat                | c <- classFloats]
  , [title =? t     --> doFloat                | t <- titleFloats]
  , [resource =? r  --> doFloat                | r <- resourceFloats]
  , [resource =? i  --> doIgnore               | i <- ignored]
  , [className =? c --> doShift (workSpaceN 1) | c <- ws1]
  , [className =? c --> doShift (workSpaceN 2) | c <- ws2]
  , [className =? c --> doShift (workSpaceN 3) | c <- ws3]
  , [className =? c --> doShift (workSpaceN 4) | c <- ws4]
  , [className =? c --> doShift (workSpaceN 5) | c <- ws5]
  , [className =? c --> doShift (workSpaceN 6) | c <- ws6]
  , [className =? c --> doShift (workSpaceN 7) | c <- ws7]
  , [className =? c --> doShift (workSpaceN 8) | c <- ws8]
  , [className =? c --> doShift (workSpaceN 9) | c <- ws9]
  , [className =? c --> doShift (workSpaceN 9) | c <- ws9]
  , miscellaneous
  ]
    where
        classFloats    = [ "Xmessage", "Unity-2d-launcher", "Vncviewer", "feh", "Gpick"
                         , "Ubuntu-tweak", "de-tud-cs-se-flashcards-Main", "xv", "mplayer2"
                         , "Gxmessage", "gxmessage", "de-hackermuehle-pdfpresenter-PdfPresenter"
                         , "gtk-recordmydesktop", "Gtk-recordmydesktop", "nethack-qt", "zoom"
                         ]
        titleFloats    = [ "Save As...", "Save File", "Options", "Document Print Status"
                         , "Terminator Preferences"]
        resourceFloats = []
        ignored        = [ "Unity-2d-panel", "trayer"]
        ws1            = [ "X-terminal-emulator"]
        ws2            = [ "Firefox", "Vimperator", "Uzbl-tabbed"]
        ws3            = [ "Zathura", ".zathura-wrapped", "Evince", "Okular", "Apvlv", "Acroread"]
        ws4            = [ {- emacs, but no rule so frames can be opened everywhere-} ]
        ws5            = [ "Gimp-2.6", "Vinagre", "Remmina", "Eclipse"
                         , "com-install4j-runtime-Launcher", "jetbrains-idea-ce"
                         , "Metasonic_Build.exe", "Scala IDE"]
        ws6            = [ "de-hackermuehle-pdfpresenter-PdfPresenter", "Hpdfp" ]
        ws7            = [  ]
        ws8            = [  ]
        ws9            = [ "MPlayer", "mplayer2" ]
        miscellaneous  = [ title =? "vmail" --> doShift (workSpaceN 7)
                         , fmap ("libreoffice"  `isPrefixOf`) className --> doShift (workSpaceN 5)
                         ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "lower"  spawnSpLower  findLower  manageLower
                , NS "upper"  spawnSpUpper  findUpper  manageUpper
                , NS "right"  spawnSpRight  findRight  manageRight
                ]
                where
                    prefix = takeWhile (/= ':')
                    spawnSpLower = runTerminal "sp_lower" "@tmx@/bin/tmx sp_lower"
                    findLower   = (prefix <$> title) =? "sp_lower"
                    manageLower = customFloating $ W.RationalRect l t w h
                        where
                        h = 0.4
                        w = 1
                        t = 1-h-0.02
                        l = (1 - w)/2
                    spawnSpUpper = runTerminal "sp_upper" "@tmx@/bin/tmx sp_upper"
                    findUpper   = (prefix <$> title) =? "sp_upper"
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

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
             { font              = myFont
             , bgColor           = "black"
             , fgColor           = "gray"
             , bgHLight          = "orange"
             , fgHLight          = "black"
             , borderColor       = "orange"
             , promptBorderWidth = 1
             , height            = 20
             , position          = Top
             , historySize       = 500
             , historyFilter     = deleteConsecutive
             , autoComplete      = Just 1
             , searchPredicate   = predicate
             }
  where predicate term candidate = all (`isInfixOf` (map toLower candidate)) termWords
          where termWords = words (map toLower term)

myTab :: Theme
myTab = defaultTheme
        { activeColor         = "black"
        , activeTextColor     = "orange"
        , activeBorderColor   = "orange"
        , inactiveColor       = "#4c4c4c"
        , inactiveTextColor   = "gray"
        , inactiveBorderColor = "gray"
        , urgentColor         = "orange"
        , urgentTextColor     = "black"
        , urgentBorderColor   = "black"
        , fontName            = myFont
        , decoHeight          = 14}

myFont :: String
myFont = "xft:DejaVu Sans-7:bold"

myMouseBindings _ = [ ((myModKey, button3), \w -> focus w >> Flex.mouseWindow Flex.linear w) ]
myNewMouseBindings x = mouseBindings defaultConfig x `M.union` M.fromList (myMouseBindings x)

myRemovedKeys :: [(ButtonMask, KeySym)]
myRemovedKeys = [ (myModKey, xK_p)
                , (myModKey, xK_h)
                , (myModKey, xK_l)
                , (myModKey, xK_n)
                , (myModKey, xK_m)
                , (myModKey, xK_t)
                ]

myKeys :: [((ButtonMask,KeySym), X ())]
myKeys = [ ((myModKey   , xK_BackSpace) , focusUrgent)
         , ((myModKey   , xK_Tab)       , toggleWS' ["NSP"])
         , ((myModKey   , xK_Return)    , sendMessage $ Toggle FULL)
         , ((myModKey   , xK_equal)     , sendMessage Expand)
         , ((myModKey   , xK_minus)     , sendMessage Shrink)
         , ((myModKey   , xK_s)         , spawn ("@rofi@/bin/rofi -i -monitor -4 -matching fuzzy -sort -show window"))
         , ((myModKey   , xK_d)         , spawn ("@rofi@/bin/rofi -i -monitor -4 -matching fuzzy -sort -show run"))
         , ((myModKey   , xK_a)         , spawn "@chooseNetwork@/bin/chooseNetwork")
         , ((myModKey   , xK_q)         , spawn "@xmonadReset@/bin/xmonadReset")

         , ((myModKey   , xK_e)         , swapNextScreen)
         , ((myModKey   , xK_w)         , nextScreen >> spawn "@centerMouse@/bin/centerMouse")
         , ((myModShift , xK_w)         , shiftNextScreen)

         , ((myModCtrl  , xK_Return)    , windows W.swapMaster)

         , ((myModShift , xK_l)         , scratchTermLower)
         , ((myModShift , xK_o)         , scratchTermRight)
         , ((myModShift , xK_u)         , scratchTermUpper)
         , ((myModShift , xK_q)         , kill1)
         , ((myModShift , xK_t)         , withFocused $ windows . W.sink)
         , ((myModShift , xK_x)         , spawn "@xkill@/bin/xkill")
         , ((myModShift , xK_BackSpace) , clearUrgents)
         , ((myModCtrl  , xK_BackSpace) , spawn "@centerMouse@/bin/centerMouse")

         , ((myModKey   , xK_F1)        , spawn "@singlehead@/bin/singlehead")
         , ((myModKey   , xK_F2)        , spawn "@autoMonitorConfig@/bin/autoMonitorConfig")
         , ((myModKey   , xK_F12)       , spawn "@takeScreenshot@/bin/takeScreenshot")
         , ((myModCtrl  , xK_l)         , spawn "@lockScreen@/bin/lockScreen")
         , ((myModCtrl  , xK_e)         , spawn "@emacsAnywhere@/bin/emacsAnywhere")

         -- Spotify client
         , ((myModCtrl, xK_F4), spawn "@playerctl@/bin/playerctl previous")
         , ((myModCtrl, xK_F5), spawn "@playerctl@/bin/playerctl play-pause")
         , ((myModShiftCtrl, xK_F5), spawn "@selectSpotifyPlayer@/bin/selectSpotifyPlayer")
         , ((myModCtrl, xK_F6), spawn "@playerctl@/bin/playerctl next")

         , ((myModShiftCtrl, xK_h), spawn "env CM_LAUNCHER=rofi CM_HISTLENGTH=15 @clipmenu@/bin/clipmenu")
         , ((myModShiftCtrl, xK_f), spawn "@rofi@/bin/rofi -i -monitor -4 -disable-history -modi 'mfa:@mfaHelper@/bin/mfaHelper' -show mfa")
         ]
         where
            scratchTermUpper = namedScratchpadAction myScratchPads "upper"
            scratchTermLower = namedScratchpadAction myScratchPads "lower"
            scratchTermRight = namedScratchpadAction myScratchPads "right"
            scratchTermVolume = namedScratchpadAction myScratchPads "volume"

myModKey :: ButtonMask
myModKey = mod4Mask

myModShift :: ButtonMask
myModShift = myModKey .|. shiftMask

myModCtrl :: ButtonMask
myModCtrl  = myModKey .|. controlMask

myModShiftCtrl :: ButtonMask
myModShiftCtrl  = myModKey .|. controlMask .|. shiftMask

myLayoutHook = mkToggle (NOBORDERS ?? FULL ?? EOT)
             $ avoidStruts . smartBorders
             $ onWorkspace (workSpaceN 5) (standardLayouts ||| gimpLayout)
               standardLayouts

standardLayouts = tabLayout ||| myTall ||| tiled ||| autoMasterLayout Grid ||| Grid ||| Full ||| simpleFloat
  where myTall = Tall 1 (3%100) (1%2)

autoMasterLayout = autoMaster 1 (1/50)

tiled = ResizableTall 1 (1/50) (3/4) []

tabLayout = tabbed shrinkText myTab

gimpLayout = withIM 0.11 (Role "gimp-toolbox") $
             reflectHoriz $
             withIM 0.15 (Role "gimp-dock") Full

workspaceRenamer :: String -> String
workspaceRenamer x = case x of
         "Tabbed Simplest"                -> "tabbed"
         "Mosaic"                         -> "mosaic"
         "Grid"                           -> "grid"
         "Accordion"                      -> "accord"
         "Simple Float"                   -> "sFloat"
         "Full"                           -> "full"
         "ResizableTall"                  -> "rTall"
         "IM ReflectX IM Tabbed Simplest" -> "gimp"
         "ReflectX IM Tabbed Simplest"    -> "mTabbed"
         "ReflectX IM Grid"               -> "mGrid"
         "ReflectX IM Circle"             -> "mCircle"
         "IM ReflectX IM Full"            -> "Gimp"
         "ReflectX IM Spiral"             -> "mSprial"
         _                                -> x

myTerminal :: String
myTerminal = "urxvt"

runTerminal :: String -> String -> String
runTerminal title arg =
  intercalate " " [myTerminal,"-title",title,"-e","bash", "-c", "'" ++ arg ++ "'"]

-- like the standard ewmh, but don't focus (damn it)
ewmhSupport :: XConfig a -> XConfig a
ewmhSupport c = c {  startupHook = startupHook c <> ewmhDesktopsStartup
                  ,  handleEventHook = handleEventHook c <> ewmhDesktopsEventHook
                  ,  logHook = logHook c <> ewmhDesktopsLogHook
                  }

-- TODO: make it not focus the window on events
customEwmhDesktopsEventHook :: Event -> X All
customEwmhDesktopsEventHook e = do
  a_aw <- getAtom "_NET_ACTIVE_WINDOW"
  ewmhDesktopsEventHook e

main :: IO ()
main = do
    xmobarBottom <- spawnPipe "@xmobar@/bin/xmobar @xmobarLower@"
    void $ spawnPipe "@xmobar@/bin/xmobar @xmobarUpper@"
    xmonad $ ewmhSupport $ docks $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { workspaces = myWorkspaces
        , manageHook = manageHook defaultConfig
                        <+> myManageHook
                        <+> namedScratchpadManageHook myScratchPads
        , borderWidth = 2
        , focusFollowsMouse = False
        , terminal = myTerminal
        , focusedBorderColor = "orange"
        , layoutHook = avoidStruts myLayoutHook
        , startupHook = setWMName "LG3D"
        , logHook = dynamicLogWithPP xmobarPP { ppOutput  = hPutStrLn xmobarBottom
                                              , ppTitle   = xmobarColor "orange" ""
                                              , ppUrgent  = xmobarColor "black" "orange" . xmobarStrip
                                              , ppVisible = xmobarColor "red"  "black"
                                              , ppCurrent = xmobarColor "orange" "black"
                                              , ppWsSep   = " | "
                                              , ppSep     = " | "
                                              , ppLayout  = xmobarColor "gray" "black" . workspaceRenamer
                                              }
        , modMask = myModKey
        , mouseBindings = myNewMouseBindings
        } `additionalKeys` myKeys `removeKeys` myRemovedKeys
