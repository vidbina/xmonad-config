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
	, layoutHook = myGaps $ myWindowSpacing $ tiled
	-- , layoutHook = gaps [(U,18), (R,23), (L,23), (D,18)] $ tiled ||| Full
	-- , layoutHook = avoidStruts $ tiled -- defaultConfig
	--  $ Tall 1 (3/100) (1/2) ||| Full
	--, layoutHook = avoidStruts $ layoutHook defaultConfig-- spacing 4 Tail 1 (3/100) (1/2) --gaps [(U, myUpperGap)]
	}

-- tools
myStatusBar = "xmobar -x0"
myTerminal = "urxvt"

-- bindings
myModMask = mod4Mask

-- features
myBorderWidth = 2
myUpperGap = 38

myGaps = gaps [(U,myUpperGap)]
myWindowSpacing = spacing 10
mySpacingTiling = Tall 1 (5/100) (1/2)
tiled = ResizableTall 1 (2/100) (1/3) []
