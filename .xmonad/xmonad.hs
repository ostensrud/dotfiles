import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ docks defaultConfig
    {
      manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ layoutHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
        {
          ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
        }
      , modMask = mod4Mask -- mod to Super
      , terminal = "urxvt"
      , normalBorderColor  = "#000000"
      , focusedBorderColor = "#a6e22e"
      , focusFollowsMouse  = False
    }

