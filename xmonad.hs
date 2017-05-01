import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Gaps
import XMonad.Util.Run

main = do
	xmproc <- spawnPipe myStatusBar
	xmonad
--		$ defaultConfig
		$ myConfig xmproc

myConfig p = def
	{ borderWidth = myBorderWidth
	, modMask = myModMask
	, terminal = myTerminal
	}

-- tools
myStatusBar = "xmobar -x0"
myTerminal = "urxvt"

-- bindings
myModMask = mod4Mask

-- features
myBorderWidth = 2
