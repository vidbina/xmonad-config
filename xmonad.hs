import XMonad
import XMonad.Config.Desktop

baseConfig = desktopConfig

main = xmonad $ baseConfig
    { terminal = "urxvt"
    ,  borderWidth = 2
    ,  modMask = mod4Mask
    }
