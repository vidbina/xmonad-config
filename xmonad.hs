import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Cross
import XMonad.Layout.Gaps
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.StackSet as W
import XMonad.Util.Run

myFocussedBorderColor = "#FF1493" -- "#e43a67" -- "#3abce4"

myNormalBorderColor = "#1B1D1E"

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = xmobarColor "black" myFocussedBorderColor . wrap "[" "]"
    , ppVisible =
        xmobarColor myFocussedBorderColor myNormalBorderColor . wrap "(" ")"
    , ppUrgent = xmobarColor "red" ""
    , ppTitle = xmobarColor "green" ""
    , ppExtras = [willFloatNextPP id]
    }

myPlacement = withGaps (16, 0, 16, 0) (smart (0.5, 0.5))

-- Sourced from https://github.com/jonascj/.xmonad/blob/master/xmonad.hs
barCreator :: DynamicStatusBar
barCreator (S sid) = spawnPipe $ myXmobarCommand ++ " --screen " ++ show sid

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

main = do
  xmobarPipe <- spawnPipe myXmobarCommand
  xmonad $ myConfig xmobarPipe

myConfig p =
  docks
    def
      { borderWidth = myBorderWidth
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocussedBorderColor
      , focusFollowsMouse = False
      , keys = \c -> myKeys c `M.union` keys defaultConfig c
      , layoutHook = avoidStruts myLayoutHook
      , manageHook = placeHook myPlacement <+> floatNextHook <+> myManageHook
      , modMask = myModMask
      , handleEventHook = dynStatusBarEventHook barCreator barDestroyer
      , startupHook =
          do setWMName "LG3D"
             dynStatusBarStartup barCreator barDestroyer
      , terminal = myTerminalCommand
      , logHook =
          multiPP myXmobarPP myXmobarPP >> updatePointer (0.9, 0.9) (0, 0)
      }

-- tools
myXmobarCommand = "TZDIR=/etc/zoneinfo xmobar -x0 "

myDzenCommand = "dzen2 -y '0' -h '24' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"

myConkyCommand = "conky -c /home/vid/.config/conky/conky.conf"

myTerminalCommand = "urxvtc"

-- features
myBorderWidth = 3

mySpacingSize = 5

mySpacing = spacing mySpacingSize

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

myManageHook =
  composeAll
    [ className =? "Pinentry" --> doFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog") -->
      doDialogFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "toolbox_window") --> doToolbarFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "gimp-message-dialog") -->
      doDialogFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog") -->
      doDialogFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "gimp-layer-new") --> doDialogFloat
    , (className =? "Gimp") --> doDialogFloat
  -- Thunderbird
    , (stringProperty "WM_WINDOW_ROLE" =? "EventDialog" <&&> className =?
       "Calendar") -->
      doDialogFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "EventDialog" <&&> className =?
       "Daily") -->
      doDialogFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "AlarmWindow" <&&> className =?
       "Daily") -->
      doDialogFloat
    , (className =? "Firefox" <&&> title =? "File Upload") --> doDialogFloat
    , (className =? "Firefox" <&&> title =? "Save As") --> doDialogFloat
    , (className =? "Dialog") --> doDialogFloat
  -- https://github.com/xmonad/xmonad/issues/146
  -- import Data.List
  --, (className =? "VirtualBox" <&&> fmap ("[Running]" `isInfixOf`) title) -->
    , (className =? "VirtualBox") --> doDialogFloat
    , (className =? "processing-app-Base") --> doDialogFloat
    , (className =? "Nm-connection-editor") --> doFloat
    , (className =? "qemu-system-i386") --> doDialogFloat
    , (className =? "Eog") --> doDialogFloat
    , (className =? "feh") --> doFloat
    , (className =? ".blueman-manager-wrapped") --> doDialogFloat
    , (className =? "ibus-setup") --> doDialogFloat
    , (stringProperty "WM_NAME" =? "Cinelerra-CV: Errors") --> doFloat
    , (stringProperty "WM_NAME" =? "Emoji Choice") --> doFloat
    , (stringProperty "WM_NAME" =? "Media viewer" <&&> className =?
       "TelegramDesktop") -->
      doFloat
    , (stringProperty "WM_NAME" =? "Formula editor" <&&> className =? "FreeCAD") -->
      doFloat
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
  , ((myModMask .|. shiftMask, xK_Escape), spawn "sleep 0.2; xlock")
  , ((0, xK_Print), spawn "sleep 0.2; scrot -d 0.1")
  , ((myModMask, xK_Print), spawn "sleep 0.2; scrot -s")
  ]

mySpacingKeys =
  [ ((myModMask, xK_z), setSpacing 0)
  , ((myModMask .|. shiftMask, xK_z), setSpacing mySpacingSize)
  ]

myToggleKeys =
  [ ((myModMask, xK_f), sendMessage $ Toggle FULL)
  , ((myModMask, xK_x), sendMessage $ Toggle MIRROR)
  --, ((myModMask, xK_x), sendMessage $ Toggle NOBORDERS)
  , ((myModMask .|. shiftMask, xK_x), sendMessage $ Toggle SMARTBORDERS)
  ]

-- Mosaic keybindings
--myMosaicKeys =
--  [ ((myModMask, xK_s), withFocused (sendMessage . tallWindowAlt))
--  , ((myModMask, xK_d), withFocused (sendMessage . wideWindowAlt))
----  , ((myModMask, xK_a), withFocused (sendMessage . expandWindowAlt))
----  , ((myModMask, xK_z), withFocused (sendMessage . shrinkWindowAlt))
--  ]
-- ResizableTall keybindings
myMouseResizableTallKeys =
  [ ((myModMask, xK_u), sendMessage ShrinkSlave)
  , ((myModMask, xK_i), sendMessage ExpandSlave)
  ]

myWorkspaceKeys =
  [ ((myModMask .|. shiftMask, xK_w), renameWorkspace def)
  , ((myModMask .|. mod1Mask, xK_w), selectWorkspace def)
  ]

myFloatKeys
  -- resize width
 =
  [ ((myModMask, xK_s), withFocused (keysResizeWindow (-20, 0) (1 % 2, 1 % 2)))
  , ( (myModMask .|. shiftMask, xK_s)
    , withFocused (keysResizeWindow (20, 0) (1 % 2, 1 % 2)))
  -- resize height
  , ((myModMask, xK_d), withFocused (keysResizeWindow (0, -20) (1 % 2, 1 % 2)))
  , ( (myModMask .|. shiftMask, xK_d)
    , withFocused (keysResizeWindow (0, 40) (1 % 2, 1 % 2)))
  , ((myModMask .|. shiftMask, xK_e), toggleFloatNext >> runLogHook)
  -- window move
  , ((myModMask .|. mod1Mask, xK_h), withFocused (keysMoveWindow (-10, 0)))
  , ((myModMask .|. mod1Mask, xK_l), withFocused (keysMoveWindow (10, 0)))
  , ((myModMask .|. mod1Mask, xK_j), withFocused (keysMoveWindow (0, 10)))
  , ((myModMask .|. mod1Mask, xK_k), withFocused (keysMoveWindow (0, -10)))
  -- fast window move
  , ( (myModMask .|. mod1Mask .|. controlMask, xK_h)
    , withFocused (keysMoveWindow (-100, 0)))
  , ( (myModMask .|. mod1Mask .|. controlMask, xK_l)
    , withFocused (keysMoveWindow (100, 0)))
  , ( (myModMask .|. mod1Mask .|. controlMask, xK_j)
    , withFocused (keysMoveWindow (0, 100)))
  , ( (myModMask .|. mod1Mask .|. controlMask, xK_k)
    , withFocused (keysMoveWindow (0, -100)))
  ]

-- REMEMBER: myModMask+Shift+(xK_j | xK_k) shifts windows around
myWindowKeys =
  [ ((myModMask, xK_b), sendMessage ToggleStruts)
  , ((myModMask, xK_r), sendMessage Reset)
  , ((myModMask .|. shiftMask, xK_r), sendMessage resetAlt)
  ] ++
  myFloatKeys ++
  myTriggerKeys ++
  myWorkspaceKeys ++ mySpacingKeys ++ myToggleKeys ++ myMouseResizableTallKeys-- ++ myMosaicKeys
  -- ++ myMirrorKeys

myKeys conf@XConfig {XMonad.modMask = myModMask} =
  M.fromList $ myWindowKeys ++ myAudioKeys

-- myTiling = Tall 1 (5/100) (1/2)
mySpacedSplitWithLargeMasterLayout = mySpacing myResizable

-- myTabbedLayout = myTabs
myMultiColumnLayout = multiCol [1] 4 0.01 0.5

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
