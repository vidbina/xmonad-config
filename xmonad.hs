import qualified Data.Map                            as M
import           Data.Ratio                          ((%))
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.UpdatePointer
import qualified XMonad.Config                       (def)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FloatNext
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.Cross
import           XMonad.Layout.Gaps
import           XMonad.Layout.Mosaic
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.StackSet                     as W
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad

-- https://github.com/xmonad/xmonad/blob/11d6711dfff3c97a34809d147785906426419ffc/tutorial.md

myFocussedBorderColor = "#FF1493" -- "#e43a67" --
myNormalBorderColor = "#1B1D1E"

mySubduedColor = "#a8a8a8"
myNormalColor = "#ffffff"

myRedColor = "#e91e63"
myGreenColor = "#44bc44"
myYellowColor = "#d0bc00"
myBlueColor = "#2fafff"
myMagentaColor = "#feacd0"
myOrangeColor = "#ff6600"

myTransparentColor = "#00000000"


nextTag :: (String -> String)
nextTag = xmobarColor "black" myYellowColor

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = xmobarColor myMagentaColor "" . wrap "{" "}"
    , ppVisible = xmobarColor myMagentaColor "" . wrap "" ""
    , ppHidden = xmobarColor mySubduedColor ""
    , ppUrgent = xmobarColor myOrangeColor ""
    , ppTitle = xmobarColor myNormalColor "" . wrap " " " "
    , ppExtras = [willFloatNextPP $ nextTag . wrap " " " "]
    }

myPlacement = withGaps (16, 0, 16, 0) (smart (0.5, 0.5))

mySB = statusBarProp myXmobarCommand (pure myXmobarPP)

main :: IO ()
main = do
  xmonad $ withSB mySB myConfig

myConfig =
  docks
    def
      { borderWidth = myBorderWidth
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocussedBorderColor
      , focusFollowsMouse = False
      , keys = \c -> myKeys c `M.union` keys def c
      , layoutHook = avoidStruts myLayoutHook
      , manageHook = myManageHook <+> placeHook myPlacement <+> floatNextHook
      , modMask = myModMask
      , startupHook =
          do setWMName "LG3D"
      , terminal = myTerminalCommand
      , logHook = dynamicLogWithPP myXmobarPP
      }

-- tools
myXmobarCommand = "TZDIR=/etc/zoneinfo xmobar -x0 "

--myDzenCommand = "dzen2 -y '0' -h '24' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
--myConkyCommand = "conky -c /home/vid/.config/conky/conky.conf"
myTerminalCommand = "urxvt"

-- features
myBorderWidth = 3

mySpacingSize = toInteger 5

mySpacing = spacing $ fromInteger mySpacingSize

myResizable =
  mouseResizableTile
    { nmaster = 1
    , masterFrac = 2 / 3
    , fracIncrement = 2 / 100
    , draggerType = BordersDragger
    }

myCenteredHalfWidthRect = W.RationalRect 0.25 0.25 0.5 0.5

doDialogFloat = doRectFloat myCenteredHalfWidthRect

doToolbarFloat = doRectFloat $ W.RationalRect 0.025 0.1 0.1 0.5

doPageFloat = doRectFloat $ W.RationalRect 0.68 0.15 0.30 0.70

windowRole :: String -> Query Bool
windowRole s = (stringProperty "WM_WINDOW_ROLE") =? s

windowName :: String -> Query Bool
windowName s = (stringProperty "WM_NAME") =? s

windowNameAndClass :: String -> String -> Query Bool
windowNameAndClass n c = (windowName n) <&&> (className =? c)

windowRoleAndClass :: String -> String -> Query Bool
windowRoleAndClass r c = (windowRole r) <&&> (className =? c)

myManageHookThunderbird :: ManageHook
myManageHookThunderbird =
  composeAll
    [ windowRoleAndClass "AlarmWindow" "Thunderbird" --> doDialogFloat
    , windowRoleAndClass "Conflicts" "Thunderbird" --> doDialogFloat
    , windowRoleAndClass "Dialog" "Firefox" --> doDialogFloat
    , windowName "Open Firefox in Troubleshoot Mode?" --> doFloat
    , windowName "Emacs Test" --> (doRectFloat $ myCenteredHalfWidthRect)
--  , (className =? "Daily") --> doDialogFloat
--  , (className =? "Dialog") --> doDialogFloat
--  , (className =? "Firefox" <&&> title =? "File Upload") --> doDialogFloat
--  , (className =? "Firefox" <&&> title =? "Save As") --> doDialogFloat
--  , windowRoleAndClass "AlarmWindow" "Daily" --> doDialogFloat
--  , windowRoleAndClass "EventDialog" "Daily" --> doDialogFloat
--  , windowRoleAndClass "EventDialog" "Calendar" --> doDialogFloat
    ]

-- https://github.com/xmonad/xmonad/issues/146
myManageHookVirtualbox :: ManageHook
myManageHookVirtualbox =
  composeAll [(className =? "VirtualBox") --> doDialogFloat]

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#Tiling_most_windows_in_Gimp
manageGimp :: ManageHook
manageGimp =
  composeAll
    [ (windowRole "gimp-toolbox" <||> windowRole "gimp-image-window") -->
      (ask >>= doF . W.sink)
    , (className =? "Gimp") --> doDialogFloat
    , (windowRole "gimp-layer-new") --> doDialogFloat
    , (windowRole "gimp-message-dialog") --> doDialogFloat
    , (windowRole "gimp-toolbox-color-dialog") --> doDialogFloat
    --, (windowRole "toolbox_window") --> doToolbarFloat
    ]

myManageHookCameraTools :: ManageHook
myManageHookCameraTools =
  composeAll
  [ (className =? "guvcview") --> doFloat
  , (className =? "Guvcview") --> doFloat
  , (className =? ".guvcview-wrapped") --> doFloat
  ]

-- https://pbrisbin.com/posts/xmonad_scratchpad/#cb3-2
myManageHookScratchpad :: ManageHook
myManageHookScratchpad =
  composeAll
    [ windowName "scratchpad" --> doFloat ]

bottomRightPlace = fixed (0.9, 0.9)
bottomRightPlaceHook = placeHook bottomRightPlace

myManageHook =
  composeAll
    [ myManageHookThunderbird
    , myManageHookVirtualbox
    , myManageHookScratchpad
    , myManageHookCameraTools
    , manageGimp
    , (className =? ".arandr-wrapped") --> doFloat
    , (className =? ".blueman-manager-wrapped") --> doDialogFloat
    , (className =? "Eog") --> doDialogFloat
    , (className =? "Ghidra") --> doFloat
    , (className =? "Gnuplot") --> doFloat
    , (className =? "Godot_ProjectList") --> doFloat
    , (className =? "Gucharmap") --> doFloat
    , (className =? "Nm-connection-editor") --> doFloat
    , (className =? "Octave") --> doFloat
    , (className =? "pinentry") --> doFloat
    , (className =? "Xmessage") --> doDialogFloat
    , (className =? "feh") --> doFloat
    , (className =? "ffplay") --> doFloat
    , (className =? "gnuplot_qt") --> doFloat
    , (className =? "ibus-setup") --> doDialogFloat
    , (className =? "mpv") --> doFloat
    , (className =? "processing-app-Base") --> doDialogFloat
    , (className =? "qemu-system-i386") --> doDialogFloat
    , (className =? "vokoscreen") --> doFloat
    , (className =? "scribus") --> doFloat
    , (className =? "Zathura") --> doPageFloat
    , (className =? "okular") --> doPageFloat
    , windowNameAndClass "Print To File ..." "okular" --> doFloat
    , windowNameAndClass "Print" "okular" --> doFloat
    , windowNameAndClass "Select Color" "okular" --> doFloat
    , windowName "Cinelerra-CV: Errors" --> doFloat
    , windowName "Emoji Choice" --> doFloat
    , windowName "Volume Control" --> doFloat
    , windowName "zoom_linux_float_video_window" --> doFloat
    , windowNameAndClass "Formula editor" "FreeCAD" --> doFloat
    , windowNameAndClass "Media viewer" "TelegramDesktop" --> doFloat
    , windowRole "GtkFileChooserDialog" --> doDialogFloat
    , windowRole "PictureInPicture" --> bottomRightPlaceHook <+> doFloat
    , manageDocks
    ]

-- bindings
myModMask = mod4Mask

myAudioKeys =
  [ ((0, 0x1008ff12), spawn "amixer -q sset Master toggle")
  , ((0, 0x1008ff11), spawn "amixer -q sset Master 10%-")
  , ((0, 0x1008ff13), spawn "amixer -q sset Master 10%+")
  , ((0, 0x1008ff14), spawn "playerctl play-pause") -- play
  , ((0, 0x1008ff16), spawn "playerctl previous") -- prev
  , ((0, 0x1008ff17), spawn "playerctl next") -- next
  ]

myMirrorKeys =
  [ ((myModMask, xK_a), sendMessage MirrorShrink)
  , ((myModMask .|. shiftMask, xK_a), sendMessage MirrorExpand)
  ]

myTriggerKeys =
  [ ((myModMask, xK_Escape), spawn "sleep 0.2; xtrlock-pam -b none")
  --, ((myModMask, xK_Insert), spawn "systemctl hibernate")
  , ((myModMask .|. shiftMask, xK_Escape), spawn "sleep 0.2; xlock")
  , ((0, xK_Print), spawn "sleep 0.2; scrot -d 0.1")
  , ((myModMask, xK_Print), spawn "sleep 0.2; scrot -s")
  ]

mySpacingKeys =
  [ ((myModMask, xK_z), setScreenWindowSpacing 0)
  , ((myModMask .|. shiftMask, xK_z), setScreenWindowSpacing mySpacingSize)
  ]

myToggleKeys =
  [ ((myModMask, xK_f), sendMessage $ Toggle FULL)
  , ((myModMask, xK_x), sendMessage $ Toggle MIRROR)
  --, ((myModMask, xK_x), sendMessage $ Toggle NOBORDERS)
  , ((myModMask .|. shiftMask, xK_x), sendMessage $ Toggle SMARTBORDERS)
  ]

-- Mosaic keybindings
myMosaicKeys =
  [ ((myModMask, xK_s), withFocused (sendMessage . tallWindowAlt))
  , ((myModMask, xK_d), withFocused (sendMessage . wideWindowAlt))
  ]

--  , ((myModMask, xK_a), withFocused (sendMessage . expandWindowAlt))
--  , ((myModMask, xK_z), withFocused (sendMessage . shrinkWindowAlt))
-- ResizableTall keybindings
myMouseResizableTallKeys =
  [ ((myModMask, xK_u), sendMessage ShrinkSlave)
  , ((myModMask, xK_i), sendMessage ExpandSlave)
  ]

myWorkspaceKeys =
  [ ((myModMask .|. shiftMask, xK_w), renameWorkspace def)
  , ((myModMask .|. mod1Mask, xK_w), selectWorkspace def)
  , ((myModMask, xK_Tab), nextWS)
  , ((myModMask .|. shiftMask, xK_Tab), prevWS)
  , ((myModMask .|. mod1Mask, xK_Tab), nextScreen)
  , ((myModMask .|. mod1Mask .|. shiftMask, xK_Tab), prevScreen)
  ]

myFloatKeys
  -- resize width
 =
  [ ((myModMask, xK_s), withFocused (keysResizeWindow (-20, 0) (1 % 2, 1 % 2)))
  , ((myModMask .|. mod1Mask, xK_s), withFocused (keysResizeWindow (20, 0) (1 % 2, 1 % 2)))
  -- resize height
  , ((myModMask, xK_d), withFocused (keysResizeWindow (0, -20) (1 % 2, 1 % 2)))
  , ((myModMask .|. mod1Mask, xK_d), withFocused (keysResizeWindow (0, 40) (1 % 2, 1 % 2)))
  , ((myModMask .|. shiftMask, xK_e), toggleFloatNext >> runLogHook)
  -- window move
  , ((myModMask .|. mod1Mask, xK_h), withFocused (keysMoveWindow (-10, 0)))
  , ((myModMask .|. mod1Mask, xK_l), withFocused (keysMoveWindow (10, 0)))
  , ((myModMask .|. mod1Mask, xK_j), withFocused (keysMoveWindow (0, 10)))
  , ((myModMask .|. mod1Mask, xK_k), withFocused (keysMoveWindow (0, -10)))
  -- fast window move
  , ((myModMask .|. mod1Mask .|. controlMask, xK_h), withFocused (keysMoveWindow (-100, 0)))
  , ((myModMask .|. mod1Mask .|. controlMask, xK_l), withFocused (keysMoveWindow (100, 0)))
  , ((myModMask .|. mod1Mask .|. controlMask, xK_j), withFocused (keysMoveWindow (0, 100)))
  , ((myModMask .|. mod1Mask .|. controlMask, xK_k), withFocused (keysMoveWindow (0, -100)))
  ]

runKeys =
  [
    ((myModMask, xK_p), spawn "dmenu_run -p ':>' -fn 'DejaVuSansMono-20:style=bold' -nf white -nb black -sf black -sb '#FF1493'")
  , ((myModMask, xK_a), spawn "rofi -show emoji")
  , ((myModMask, xK_c), spawn "rofi -show calc")
  --, ((myModMask .|. shiftMask, xK_p), spawn "bash -ci 'gmrun'")
  , ((myModMask .|. shiftMask, xK_p), spawn "rofi -show run")
  ]

scratchpadKeys =
  [
      ((myModMask, xK_0), urxvtScratchpad)
  ]
  where
    termiteScratchpad = scratchpadSpawnActionTerminal "termite -t scratch"
    urxvtScratchpad   = scratchpadSpawnActionTerminal "urxvt"
    kittyScratchpad   = scratchpadSpawnActionCustom "kitty --name=scratchpad"

-- REMEMBER: myModMask+Shift+(xK_j | xK_k) shifts windows around
myWindowKeys =
  [ ((myModMask, xK_b), sendMessage ToggleStruts)
  , ((myModMask .|. shiftMask, xK_slash), withFocused $ \w -> spawn ("xkill -id " ++ show w))
  ] ++
  myFloatKeys ++
  myTriggerKeys ++
  myWorkspaceKeys ++
  mySpacingKeys ++
  myToggleKeys ++
  myMouseResizableTallKeys -- ++ myMosaicKeys
  -- ++ myMirrorKeys
   ++
  runKeys ++ scratchpadKeys

myKeys conf@XConfig {XMonad.modMask = myModMask} =
  M.fromList $ myWindowKeys ++ myAudioKeys

-- myTiling = Tall 1 (5/100) (1/2)
mySpacedSplitWithLargeMasterLayout = mySpacing myResizable

-- myTabbedLayout = myTabs
myMultiColumnLayout = multiCol [1, 1, 2] 2 0.01 0.4

mySpacedMultiColumnLayout = mySpacing myMultiColumnLayout

toggles = mkToggle (MIRROR ?? FULL ?? NOBORDERS ?? SMARTBORDERS ?? EOT)

toggledLayoutsHook =
  toggles $ mySpacedSplitWithLargeMasterLayout ||| mySpacedMultiColumnLayout

myLayoutHook = toggledLayoutsHook
-- mySpacedSplitWithLargeMasterLayout
--                -- ||| myTabbedLayout
--                -- ||| simpleCross
--                -- ||| multiCol [1] 2 0.05 0.5
--                -- ||| Mirror (multiCol [1] 4 0.01 0.5)
--                -- ||| mosaic 2 [3, 2]
--                -- ||| mosaic 1.5 []
--                -- MosaicAlt M.empty
--                -- ||| spiral (1/2)
