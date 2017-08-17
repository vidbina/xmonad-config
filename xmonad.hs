import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Cross
import XMonad.Layout.Gaps
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MultiColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map as M

main = do
  statusBar <- spawnPipe myXmobar -- myStatusBar
  --statPane <- spawnPipe myConky
  --xmonad $ myConfig myXmobar -- statBar
  xmonad $ myConfig statusBar -- statPane

myConfig p = def
  { borderWidth = myBorderWidth
  , keys = \c -> myKeys c `M.union` keys defaultConfig c
  , layoutHook = myLayoutHook
  , modMask = myModMask
  , terminal = myTerminal
  }

-- tools
myXmobar = "xmobar -x0"
myDzen = "dzen2 -y '0' -h '24' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myConky = "conky -c /home/vid/.config/conky/conky.conf"
--myStatusBar = myConky ++ " | " ++ myDzen
--myStatusBar = myXmobar ++ " | " ++ myDzen
myTerminal = "urxvt"

-- bindings
myModMask = mod4Mask

-- features
myBorderWidth = 2
myUpperGap = 28
-- NOTE: signatures, just as a reminder
-- gaps :: GapSpec -> l a -> ModifiedLayout Gaps l a
-- tabbed :: (Eq a, Shrinker s) => s -> Theme -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
-- mySpacing :: Int -> l a -> ModifiedLayout Spacing l a
-- LayoutClass ResizableTall
myGaps = gaps [(U,myUpperGap)]
-- myTabs = tabbed shrinkText def
mySpacing = spacing 10
myResizable = ResizableTall 1 (2/100) (2/3) []

audioKeys = [
    ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
  , ((0, 0x1008ff11), spawn "amixer -q set Master 10%-")
  , ((0, 0x1008ff13), spawn "amixer -q set Master 10%+")
  ]

windowKeys = [
    ((myModMask, xK_a), withFocused (sendMessage . expandWindowAlt))
  , ((myModMask, xK_z), withFocused (sendMessage . shrinkWindowAlt))
  , ((myModMask, xK_s), withFocused (sendMessage . tallWindowAlt))
  , ((myModMask, xK_d), withFocused (sendMessage . wideWindowAlt))
  , ((myModMask, xK_r), sendMessage resetAlt)
--    ((myModMask, xK_a), sendMessage Taller)
--  , ((myModMask, xK_z), sendMessage Wider)
--  , ((myModMask, xK_r), sendMessage Reset)
  ]
myKeys conf@(XConfig {XMonad.modMask = myModMask}) = M.fromList $
  windowKeys ++ audioKeys

-- myTiling = Tall 1 (5/100) (1/2)

mySpacedSplitWithLargeMasterLayout = mySpacing $ myResizable
-- myTabbedLayout = myTabs

myLayoutHook = myGaps mySpacedSplitWithLargeMasterLayout
               -- ||| myTabbedLayout
               -- ||| simpleCross
               -- ||| multiCol [1] 2 0.05 0.5
               -- ||| Mirror (multiCol [1] 4 0.01 0.5)
               ||| multiCol [1] 4 0.01 0.5
               -- ||| mosaic 2 [3, 2]
               -- ||| mosaic 1.5 []
               -- ||| MosaicAlt M.empty
               ||| Full
               -- ||| spiral (1/2)
