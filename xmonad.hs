import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Cross
import XMonad.Layout.Gaps
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MultiColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.StackSet as W
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe myXmobar
  xmonad $ myConfig xmproc

myConfig p = docks def
  { borderWidth = myBorderWidth
  , keys = \c -> myKeys c `M.union` keys defaultConfig c
  , layoutHook = avoidStruts $ myLayoutHook
  , manageHook = myManageHook
  , modMask = myModMask
  , terminal = myTerminal
  , logHook = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn p }
  }

-- tools
myXmobar = "xmobar -x0"
myDzen = "dzen2 -y '0' -h '24' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myConky = "conky -c /home/vid/.config/conky/conky.conf"
myTerminal = "termite"

-- features
myBorderWidth = 2
spacingSize = 5
mySpacing = spacing spacingSize
myResizable = mouseResizableTile { nmaster = 1, masterFrac = 2/3, fracIncrement = 2/100, draggerType = BordersDragger }

centeredHalfWidthRect = W.RationalRect 0.25 0.25 0.5 0.5
doDialogFloat = doRectFloat centeredHalfWidthRect

myManageHook = composeAll [
    className =? "Pinentry" --> doFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog") -->
      doDialogFloat
    , (className =? "Firefox" <&&> title =? "File Upload") -->
      doDialogFloat
    , (className =? "Firefox" <&&> title =? "Save As") -->
      doDialogFloat
    , (className =? "ibus-setup") -->
      doDialogFloat
  ]

-- bindings
myModMask = mod4Mask

audioKeys =
  [ ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
  , ((0, 0x1008ff11), spawn "amixer -q set Master 10%-")
  , ((0, 0x1008ff13), spawn "amixer -q set Master 10%+")
  , ((0, 0x1008ff14), spawn "playerctl play-pause") -- play
  , ((0, 0x1008ff16), spawn "playerctl previous") -- prev
  , ((0, 0x1008ff17), spawn "playerctl next") -- next
  ]

-- Mosaic keybindings
mosaicKeys =
  [ ((myModMask, xK_s), withFocused (sendMessage . tallWindowAlt))
  , ((myModMask, xK_d), withFocused (sendMessage . wideWindowAlt))
--  , ((myModMask, xK_a), withFocused (sendMessage . expandWindowAlt))
--  , ((myModMask, xK_z), withFocused (sendMessage . shrinkWindowAlt))
  ]

-- ResizableTall keybindings
mirrorKeys = [
    ((myModMask, xK_a), sendMessage MirrorShrink)
  , ((myModMask .|. shiftMask, xK_a), sendMessage MirrorExpand)
  ]

mouseResizableTallKeys = [
    ((myModMask, xK_u), sendMessage ShrinkSlave)
  , ((myModMask, xK_i), sendMessage ExpandSlave)
  ]

-- REMEMBER: myModMask+Shift+(xK_j | xK_k) shifts windows around
windowKeys = [
    ((myModMask, xK_z), setSpacing 0)
  , ((myModMask .|. shiftMask, xK_z), setSpacing spacingSize)
  , ((myModMask, xK_b), sendMessage ToggleStruts)
  , ((myModMask, xK_r), sendMessage Reset)
  , ((myModMask .|. shiftMask,  xK_r), sendMessage resetAlt)
  ]
  ++ mouseResizableTallKeys
  -- ++ mosaicKeys
  -- ++ mirrorKeys

myKeys conf@(XConfig {XMonad.modMask = myModMask}) = M.fromList $
  windowKeys ++ audioKeys

-- myTiling = Tall 1 (5/100) (1/2)

mySpacedSplitWithLargeMasterLayout = mySpacing $ myResizable
-- myTabbedLayout = myTabs

myLayoutHook = mySpacedSplitWithLargeMasterLayout
               -- ||| myTabbedLayout
               -- ||| simpleCross
               -- ||| multiCol [1] 2 0.05 0.5
               -- ||| Mirror (multiCol [1] 4 0.01 0.5)
               ||| multiCol [1] 4 0.01 0.5
               -- ||| mosaic 2 [3, 2]
               -- ||| mosaic 1.5 []
               -- MosaicAlt M.empty
               ||| Full
               -- ||| spiral (1/2)
