import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)

main = do
	xmproc <- spawnPipe myStatusBar
	xmonad $ myConfig xmproc

myConfig p = def
	{ borderWidth = myBorderWidth
	, modMask = myModMask
	, terminal = myTerminal
	, layoutHook = myLayoutHook
	}

-- tools
myStatusBar = "xmobar -x0"
myTerminal = "urxvt"

-- bindings
myModMask = mod4Mask

-- features
myBorderWidth = 2
myUpperGap = 32

myGaps = gaps [(U,myUpperGap)]
myTabs = tabbed shrinkText def

myWindowSpacing = spacing 10
mySpacingTiling = Tall 1 (5/100) (1/2)

mySpacedSplitWithLargeMasterLayout = myGaps $ myWindowSpacing $ ResizableTall 1 (2/100) (1/3) []
myTabbedLayout = myTabs

myLayoutHook = mySpacedSplitWithLargeMasterLayout ||| myTabbedLayout
