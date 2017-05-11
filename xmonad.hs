import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
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
myUpperGap = 34

myGaps = gaps [(U,myUpperGap)]
myWindowSpacing = spacing 10
mySpacingTiling = Tall 1 (5/100) (1/2)

myGappedSplitPaneWithLargeMasterLayout = myGaps $ myWindowSpacing $ ResizableTall 1 (2/100) (1/3) []

myLayoutHook = myGappedSplitPaneWithLargeMasterLayout
