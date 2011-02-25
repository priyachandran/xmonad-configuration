import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Actions.CycleWS 
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger                                         
import XMonad.Layout.Mosaic
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Monoid
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Submap as SM
import Control.Monad (liftM2)     
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
     
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
     
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.XSelection
     
-- for dbus (gnome-session hang)
import System.Environment
import System.Cmd
import Control.Concurrent


import qualified Data.Map as M
 
--  colors match Ubuntu Human theme and Gnome panels
selected   = "'#fad184'"
background = "'#efebe7'"
foreground = "'#000000'"
 
-- height matches Ubuntu top Gnome panel
barHeight = "24"
 
--  font intended to match Ubuntu default application font
appFontXft = "'xft\
                \:Sans\
                \:pixelsize=14\
                \:weight=regular\
                \:width=semicondensed\
                \:dpi=96\
                \:hinting=true\
                \:hintstyle=hintslight\
                \:antialias=true\
                \:rgba=rgb\
                \:lcdfilter=lcdlight\
             \'"
 
-- currently dzen2 compiled locally to get xft support
-- (-e prevents loss of title if naive user clicks on dzen2)
myDzenTitleBar =
    "dzen2\
        \ -ta l\
        \ -x 400 -w 900 -y 0\
        \ -e 'entertitle=uncollapse'\
        \ -h  " ++ barHeight  ++ "\
        \ -bg " ++ background ++ "\
        \ -fg " ++ foreground ++ "\
        \ -fn " ++ appFontXft
 
-- dmenu patched and compiled locally to add xft support
myDmenuTitleBar =
    "exec `dmenu_path | dmenu\
        \ -p 'Run:'\
        \ -i\
        \ -bh " ++ barHeight  ++ "\
        \ -nb " ++ background ++ "\
        \ -nf " ++ foreground ++ "\
        \ -sb " ++ selected   ++ "\
        \ -fn " ++ appFontXft ++ "\
    \`"
-- tab theme
myTab = defaultTheme
    { activeColor         = "black"
    , inactiveColor       = "black"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"                                                                               
    , urgentBorderColor   = "black"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow" }

-- layouts                                
myLayout = avoidStruts $ toggleLayouts (noBorders Full)
--   (smartBorders ( mosaic 2 [3,2] ||| tiled ||| Mirror tiled ||| layoutHints (tabbed shrinkText myTab)))
   (smartBorders ( Mirror tiled ||| tiled ||| layoutHints (tabbed shrinkText myTab)))

-- myLayout = tiled ||| Mirror tiled ||| Full
    where                                 
        tiled   = layoutHints $ ResizableTall nmaster delta ratio []                                                
--	tiled   = Tall nmaster delta ratio
        nmaster = 1                       
        delta   = 3/100                   
        ratio   = 1/2    
 
main = do
    xmproc <- spawnPipe myDzenTitleBar
 
    xmonad $ gnomeConfig
        { modMask = mod1Mask    
        , logHook    = myLogHookWithPP $ defaultPP
                         { ppOutput = hPutStrLn xmproc
                         , ppOrder = take 1 . drop 2
                         }
        , keys       = myKeys
	, terminal   = "gnome-terminal --hide-menubar"
	, layoutHook = myLayout
	, workspaces = ["term1","term2","term3","misc4","misc5","misc6",
	  	        "doc7","mail8","web9","misc0"]
	, manageHook = newManageHook 
        }
    where
 
newManageHook = composeAll
	  	     [ manageHook gnomeConfig
		     , title =? "gitk" --> doFloat
		     , className =? "Gimp" --> doF (W.shift "doc7")
		     , className =? "Icedove" --> doF (W.shift "mail8")
		     , className =? "Pidgin" --> doFloat
		     , className =? "MPlayer" --> doFloat
		     , className =? "totem" --> doFloat
		     , className =? "evince"  --> viewShift "doc7"
		     , className =? "epiphany-browser" --> viewShift "web9"
		     , className =? "chromium-browser" --> viewShift "web9"
		     , className =? "Iceweasel" --> viewShift "web9"
		     , title =? "iceweasel" --> viewShift "web9"
		     , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
		     , title =? "Open Password Database" --> doFloat
		     , className =? "gorilla" <||> className =? "Gorilla" --> doFloat
--		     , isFullscreen --> doFullFloat
		     , resource =? "desktop_window" --> doIgnore
		     ]
		  where viewShift = doF . liftM2 (.) W.greedyView W.shift

myLogHookWithPP :: PP -> X ()
myLogHookWithPP pp = do
    ewmhDesktopsLogHook
    dynamicLogWithPP pp
 
defKeys    = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $                                                     
     
    -- terminals
    [ ((modMask,                 xK_Return), spawn $ XMonad.terminal conf)
     
    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
     
    -- quake terminal
    , ((modMask,                 xK_Down  ), scratchpadSpawnAction conf)
     
    -- file manager
    --, ((modMask,                 xK_Up    ), runOrRaise "nautilus ~" (className =? "Nautilus"))
    --, ((modMask .|. shiftMask,   xK_Up    ), spawn "nautilus ~")
    --, ((modMask,                 xK_Up    ), spawn "nautilus ~")
     
    -- browser
    , ((modMask,               xK_b     ), spawn "firefox")
     
    -- open selection as URL
    , ((modMask .|. shiftMask, xK_s     ), safePromptSelection "firefox")
     
    -- cycle through workspaces
    , ((modMask,               xK_Right ), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask,               xK_Left  ), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
     
    -- move windows through workspaces                                                                              
    , ((modMask .|. shiftMask, xK_Right ), shiftTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. shiftMask, xK_Left  ), shiftTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. controlMask, xK_Right), shiftTo Next EmptyWS)
    , ((modMask .|. controlMask, xK_Left), shiftTo Prev EmptyWS)
     
    -- Move focus to the next/previous window
    , ((modMask,               xK_j     ), windows W.focusUp)
    , ((modMask,               xK_Tab   ), windows W.focusUp)
    , ((mod1Mask,              xK_Tab   ), windows W.focusUp)
    , ((modMask,               xK_k     ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusDown)
    , ((mod1Mask .|. shiftMask, xK_Tab  ), windows W.focusDown)
     
    -- Swap the focused window with next/prev window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)
    -- Shrink/Expand the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)                                                      
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)
     
	--Rotate thru layouts
	, ((modMask,               xK_grave ), sendMessage NextLayout)

    -- Lock screen
    , ((modMask .|. shiftMask, xK_y     ), spawn "gnome-screensaver-command -l &")
     
    -- Swap the focused window and the master window
    , ((modMask,            xK_semicolon), windows W.swapMaster)
     
    -- Increment/Deincrement the number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))
     
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
    -- Reset layout of current workspace
    , ((modMask .|. shiftMask, xK_n     ), setLayout $ XMonad.layoutHook conf)
     
     
    -- Mosaic
    , ((modMask , xK_a                  ), sendMessage Taller)
    , ((modMask , xK_z                  ), sendMessage Wider)
    , ((modMask .|. controlMask, xK_N   ), sendMessage Reset)
     
    -- Push window back into tiling
    , ((modMask,               xK_s     ), withFocused $ windows . W.sink)
    --, ((modMask .|. shiftMask, xK_s     ), sendMessage Arrange)
     
    -- toggle the status bar gap
    , ((modMask,               xK_f     ), sendMessage ToggleStruts)
     
    -- close focused window
    , ((modMask              , xK_w     ), kill)
     
    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_q),
        broadcastMessage ReleaseResources >> restart "xmonad" True)
	, ((modMask,                xK_Right),  nextWS)          
    , ((modMask,               xK_Left),    prevWS)         
    , ((modMask .|. shiftMask, xK_Right),  shiftToNext)     
    , ((modMask .|. shiftMask, xK_Left),    shiftToPrev)    
    , ((modMask,               xK_Up), nextScreen)          
    , ((modMask,               xK_Down),  prevScreen)                                                              
    , ((modMask .|. shiftMask, xK_Up), shiftNextScreen)     
    , ((modMask .|. shiftMask,  xK_Down),  shiftPrevScreen) 
    , ((modMask,               xK_z),     toggleWS)         
    , ((modMask,                       xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask,               xK_f), moveTo Next EmptyWS)  
    ]
    ++      
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]
 
-- remove some of the default key bindings
toRemove x =
    [
    ]
 
