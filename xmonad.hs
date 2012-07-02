-- Imports.
import XMonad
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import qualified Data.Map as M

-- The main function.
main = do 
        numScreens <- countScreens
	(xmonad =<< statusBar myBar myPP toggleStrutsKey (myConfig numScreens)) 

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- My mod
m = mod3Mask

-- My browser
browser = "/usr/bin/opera"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = m} = (m, xK_b)

isNotTransient :: Query Bool
isNotTransient = do
   mbw <- transientTo
   case mbw of
     Nothing -> return True
     Just _  -> return False

isTransient :: Query Bool
isTransient = do
   mbw <- transientTo
   case mbw of
     Nothing -> return False
     Just _  -> return True


myManageHook numScreens = composeAll
	 [ 
	   className =? "Firefox" <&&> isNotTransient --> doShift "5:web"
	 , className =? "Chromium" <&&> isNotTransient--> doShift "5:web"
	 , className =? "Opera" <&&> isNotTransient --> doShift "5:web"
	 , className =? "Emacs" <&&> isNotTransient --> doShift "4:emacs"
	 , className =? "Eclipse" <&&> isNotTransient --> doShift "1:dev-a"
	 , className =? "Eclipse" <&&> isTransient <&&> isDialog <&&> title =? "       " --> toSecondDevWorkspace
	 , className =? "Eclipse" <&&> isTransient <&&> isDialog <&&> title =? "       " --> unfloat
	 , className =? "Eclipse" <&&> isTransient <&&> isDialog <&&> title =? "       " --> swapup
	 , className =? "Eclipse" <&&> isTransient <&&> isDialog <&&> title =? "       " --> focusup
	 , className =? "Thunderbird" <&&> isNotTransient --> doShift "3:mail"
	 , className =? "com-clcbio-framework-general-WorkbenchManagerImpl" --> doShift "7:clc"
	 , className =? "Skype" --> doShift "8:comm"
	 , className =? "Spotify" --> doShift "9:misc"
	 , manageDocks
	 ]
	 where unfloat = ask >>= doF . W.sink
	       swapup = ask >>= doF . (\ x-> W.swapUp)
	       focusup = ask >>= doF . (\ x-> W.focusUp)
	       toSecondDevWorkspace = if (numScreens == 2)
                                         then doShift "2:dev-b"
					 else doShift "1:dev-a"

-- Main configuration, override the defaults to your liking.
myConfig numScreens = defaultConfig {
           workspaces = ["1:dev-a", "2:dev-b", "3:mail","4:emacs", "5:web", "6:shell","7:clc","8:comm","9:misc","0"]
	 , modMask = m
	 , startupHook = setWMName "LG3D" 
	 , manageHook = (myManageHook numScreens) <+> manageHook defaultConfig 
	 , terminal = "gnome-terminal"
	 } `additionalKeys`
	 [ ((m .|. shiftMask, xK_h), sendMessage Shrink)
	 , ((m .|. shiftMask, xK_l), sendMessage Expand)
	 , ((m              , xK_d), spawn "setxkbmap dk ; xmodmap ~/.Xmodmap")
	 , ((m              , xK_u), spawn "setxkbmap us ; xmodmap ~/.Xmodmap")
	 , ((m              , xK_c), clcbioJump)
	 , ((m		    , xK_s), searchPromptAndSwitch)
	 , ((m .|. shiftMask, xK_s), searchClipboardAndSwitch)
	 ]

data Clcbio = Clcbio

instance P.XPrompt Clcbio where
          showXPrompt Clcbio = "Clcbio: "

clcbioJump =
	   P.mkXPrompt Clcbio P.defaultXPConfig (P.mkComplFunFromList
	   ["jira", "fisheye", "confluence", "ehour"]) startFocusClcbio

startFocusClcbio s = do
	    startFocus (browser ++ " https://intranet.clcbio.com/" ++ s) "5:web"

startFocus command workspace = do
        spawn command
        windows $ W.greedyView workspace

searchPromptAndSwitch = do 
	SM.submap $ searchEngineMap $ S.promptSearch P.defaultXPConfig
	windows $ W.greedyView "5:web"

searchClipboardAndSwitch = do
        SM.submap $ searchEngineMap $ S.selectSearch
	windows $ W.greedyView "5:web"

searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google)
       , ((0, xK_h), method S.hoogle)
       , ((0, xK_w), method S.wikipedia)
       ]