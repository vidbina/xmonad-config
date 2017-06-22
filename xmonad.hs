import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Cross
import XMonad.Layout.Gaps
import XMonad.Layout.Mosaic
import XMonad.Layout.MultiColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
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

myKeys conf@(XConfig {XMonad.modMask = myModMask}) = M.fromList $
  [
    ((myModMask, xK_a), sendMessage Taller)
  , ((myModMask, xK_z), sendMessage Wider)
  , ((myModMask, xK_r), sendMessage Reset)
  ]

-- myTiling = Tall 1 (5/100) (1/2)

mySpacedSplitWithLargeMasterLayout = mySpacing $ myResizable
-- myTabbedLayout = myTabs

myLayoutHook = myGaps mySpacedSplitWithLargeMasterLayout
               -- ||| myTabbedLayout
               ||| Full
