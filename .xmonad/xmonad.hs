-- default desktop configuration for Fedora
import XMonad

main = do
 xmonad $ defaultConfig
        { borderWidth        = 2
        , terminal           = "urxvt"
        , normalBorderColor  = "#000000"
        , focusedBorderColor = "#657b83"
}
