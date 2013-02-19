-- current darcs as of 2010-12-31
{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances,
     OverloadedStrings
     #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
 
import Control.Applicative
import Control.Monad
import Control.Monad.Instances ()
import Control.Monad.Writer
import Control.Monad.Trans (liftIO)
import Data.List
import Data.Int
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Data.Traversable(traverse)
import Graphics.X11.Xinerama
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import System.IO
import XMonad
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.Drawer
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Master
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Config.Xfce
import System.Environment (getArgs)
import XMonad.Actions.SpawnOn
import XMonad.Operations (windows)
import XMonad.Util.Scratchpad
import XMonad.Actions.GridSelect
--import XMonad.Hooks.FadeInactive
import System.Process
import Prelude
import Text.Regex.Posix
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD
import DBus
import DBus.Client

-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 2
myNormalBorderColor = "#202030"
myFocusedBorderColor = "#A0A0D0"

--layouts
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 1/100
    ratio   = 4/5
tallLayout = named "tall" $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full

myManageHook =  composeAll
                -- per-window options, use `xprop' to learn window names and classes
				[ className =? "MPlayer"        				--> doFullFloat
				, className =? "MPlayer2"       				--> doFullFloat
				, className =? "Smplayer2"						--> doFloat
				, className =? "Gnome-mplayer"  				--> doFullFloat
				, className =? "Gimp-2.6"          				--> doFloat
				, className =? "Xfrun4"							--> doFloat
				, className =? "Wine"							--> (do
									doFloat
									doF $ W.shift "games")	
				, className =? "Xfce4-panel"					--> doFloat
				, title     =? "EPresent"       				--> doFloat
				, isFullscreen                  				--> doFullFloat
				, isDialog										--> doFloat
				, className =? "xfce4-appearance-settings"		--> doFloat
				, className =? "deluge"							--> (doF $ W.shift "torrents")
				, className =? "clementine"						--> (doF $ W.shift "misc")
				, className =? "terminator"						--> (doF $ W.shift "misc")
				, className =? "opera"							--> (doF $ W.shift "inet")
				, className =? "pidgin"							--> (doF $ W.shift "inet")
				, isOffice										--> (doF $ W.shift "office")
				, isFM											--> (doF $ W.shift "fm")
				, className =? "xfce4-power-manager-settings"	--> doFloat
				, className =? "xfce4-appfinder"				--> doFloat
				, className =? "Xfce4-appfinder"				--> doFloat
				, isXFCEPlugin									--> doFloat
				, className =? "remmina"						--> (doF $ W.shift "adm")
				, className =? "tsclient"						--> (doF $ W.shift "adm")
				, className =? "gimp"							--> doFloat
				, className =? "Xfce4-notifyd"					--> doIgnore
				, isDesktop	--> doIgnore
                ] where
                	isOffice = foldr1 (<||>) [
                		className =? "libreoffice-base",
                		className =? "libreoffice-draw",
                		className =? "libreoffice-writer",
                		className =? "libreoffice-calc",
                		className =? "libreoffice-impress"
                		]
                	isFM = foldr1 (<||>) [
                		className =? "nautilus",
                		className =? "Nautilus",
                		className =? "thunar",
                		className =? "Thunar"
                		]
                	isXFCEPlugin = foldr1 (<||>) [
                		className =? "xfce4-xkb-plugin",
                		className =? "xfce4-netload-plugin",
                		className =? "xfce4-mixer"
                		]
			isDesktop = foldr1 (<||>) [ className =? "Xfdesktop", className =? "xfdesktop" ]

myWorkspaces = ["inet", "misc", "torrents", "office", "fm", "games", "dev", "adm", "stuff"]

setVolume :: MonadIO m => String -> m()
setVolume d = do
	t <- runProcessWithInput "mpc" ["volume", d] []
	volume <- runProcessWithInput "mpc" ["volume"] []
	safeSpawn "notify-send" ["-t", "5000", "MPD", volume]

_action :: MonadIO m => (MPD.MPD ()) -> m()
_action a = io $ return . fromRight =<< MPD.withMPD a

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    [ ((modm, xK_b), sendMessage ToggleStruts)
	, ((mod1Mask, xK_F4), kill)
	, ((controlMask, xK_Print), spawn "sleep 1; scrot -s")
	, ((0, xK_Print), spawn "scrot")
	, ((modm, xK_l), spawn "xflock4")
	, ((mod4Mask .|. shiftMask, xK_l), spawn "xfce4-session-logout")
	, ((modm, xK_x), _action MPD.toggle)
	, ((modm, xK_v), _action MPD.next)
	, ((modm, xK_z), _action MPD.previous)
	, ((modm, xK_c), _action MPD.stop)
	, ((modm, xK_d), io $ return . fromRight =<< MPD.withMPD (MPD.update []))
	, ((modm, xK_a), setVolume "-2")
	, ((modm, xK_s), setVolume "+2")
	, ((modm, xK_bracketleft), sendMessage Shrink)
	, ((modm, xK_bracketright), sendMessage Expand)
	, ((mod4Mask .|. mod1Mask, xK_l), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 })
	, ((mod4Mask, xK_g), goToSelected defaultGSConfig)
    ] -- Mod-b: toggle XFCE panel

keysToRemove :: XConfig Layout -> [(KeyMask, KeySym)]
keysToRemove x = [ (mod4Mask, xK_l ), (mod4Mask, xK_h ) ]
newKeys x  = M.union (foldr M.delete (keys xfceConfig x) (keysToRemove x)) (M.fromList (myKeys x))

initMPD :: MonadIO m => m ()
initMPD = do
	let x = MPD.withMPD $ MPD.random True
	let y = MPD.withMPD $ MPD.update []
	return ()

startup :: X ()
startup = do
	initMPD
	safeSpawn "sudo" ["ntpdate", "192.168.254.1"]
	safeSpawn "pkill" ["xfdesktop"]
	safeSpawn "nautilus" ["-n"]

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLayout =
         trackFloating . smartBorders . avoidStruts
         . onWorkspace "inet" (avoidStruts $ layoutHook defaultConfig)
         . onWorkspace "stuff" (Full)
         . onWorkspace "games" (noBorders Full)
         $ m ||| named "F" (noBorders Full)
    where nav = configurableNavigation (navigateColor "#ffff00")
          m = named "M"
            . avoidStruts $ layoutHook defaultConfig

main = do
	args <- getArgs
	when ("--replace" `elem` args) replace
	xmonad $ xfceConfig { modMask = mod4Mask   -- use the super key for xmonad commands
   	   , manageHook = manageDocks <+> myManageHook
   	   , keys = newKeys
   	   , layoutHook = myLayout
   	   , startupHook = startup <+> ewmhDesktopsStartup >> setWMName "LG3D"
   	   , workspaces = myWorkspaces
   	   , terminal = "terminator"
   	   , logHook  = do
   	   		ewmhDesktopsLogHook
			setWMName "LG3D"
   	   , handleEventHook = ewmhDesktopsEventHook
   	   }
