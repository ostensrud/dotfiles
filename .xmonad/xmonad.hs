import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations
 
import System.IO
import System.Exit
 
import XMonad.Util.Run
 
import XMonad.Actions.CycleWS
 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
 
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid

import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
 dzenLeftBar <- spawnPipe myXmonadBar
 dzenRightBar <- spawnPipe myStatusBar 
 xmonad $ defaultConfig
        { borderWidth        = 2
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        , normalBorderColor  = "#000000"
        , focusedBorderColor = "#a6e22e"
        , keys               = myKeys <+> keys defaultConfig
        , focusFollowsMouse  = False
        , logHook            = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
        , layoutHook          = layoutHook'
        , manageHook          = manageHook'
}

myTerminal = "urxvt"
myWorkspaces = ["1:main","2:web","3:tunnels","4:whatever","5:spotify"]
myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '640' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E' -fn Vera:size=10"
myStatusBar = "conky -c ~/.xmonad/conky.rc_xmonad | dzen2 -x '640' -h '24' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0' -fn Vera:size=10"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  {
        ppCurrent           =   dzenColor "#a6e22e" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#a6e22e" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "(RT)"
                                    "Mirror ResizableTall"      ->      "(MR)"
                                    "Full"                      ->      "(F)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
  }

layoutHook'  =  customLayout

customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

--}}}

-- Theme {{{
-- Color names are easier to remember:
colorOrange         = "#FD971F"
colorDarkGray       = "#1B1D1E"
colorPink           = "#F92672"
colorGreen          = "#A6E22E"
colorBlue           = "#66D9EF"
colorYellow         = "#E6DB74"
colorWhite          = "#CCCCC6"

barFont  = "terminus"
barXFont = "inconsolata:size=8"
xftFont = "xft: inconsolata-10"
--}}} 
-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 22
                }
-- }}}

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
    ]

-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
  [ [className  =?  c --> doShift "2:web"     | c <-  myWebs  ] -- move webapps to web
  ,[className  =?  c --> doShift "5:spotify" | c <-  myMusic ] -- move music to music
  ])
  where
    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"

    --classnames
    myMusic = ["Spotify"]
    myWebs  = ["Chromium","Chromium-browser","Firefox"]
-- }}}
