module Main where

import Control.Monad ( (<=<) )
import Control.Concurrent ( threadDelay )
import Control.Exception ( try, SomeException )
import Data.Functor ( ($>), void )
import Graphics.X11.ExtraTypes.XF86
import System.Directory ( doesFileExist
                        , findExecutable
                        , getHomeDirectory
                        , getXdgDirectory
                        , XdgDirectory(XdgState)
                        )
import System.FilePath.Posix ( (</>) )
import qualified System.Process as P
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks ( avoidStruts, docks, ToggleStruts(..) )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.NoBorders ( smartBorders, noBorders )
--import XMonad.Layout.SimpleFloat ( simpleFloat )
import XMonad.Util.Run ( spawnPipe, hPutStrLn )
import XMonad.Util.Paste ( sendKey )
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Text.Printf ( printf )

type Workspace = (String, KeySym)

myWorkspaces :: [ Workspace ]
myWorkspaces = [ ("1", xK_1)
               , ("2", xK_2)
               , ("3", xK_parenleft)
               , ("4", xK_dollar)
               , ("5", xK_braceleft)
               , ("6", xK_equal)
               , ("p", xK_p)
               , ("w", xK_w)
               , ("v", xK_v)
               , ("m", xK_m)
               , ("c", xK_c)
               , ("g", xK_g)
               ]

type Keybinding = ( (ButtonMask, KeySym), X () )

makeWorkspaceKeys :: ButtonMask -> [ Workspace ] -> [ Keybinding ]
makeWorkspaceKeys mask ws = gotoKeys ws ++ moveKeys ws
    where gotoKeys = map (\(name, key) -> ((mask, key), windows $ W.greedyView name))
          moveKeys = map (\(name, key) -> ((mask .|. shiftMask, key), windows $ W.shift name))

makeScreenKeys :: ButtonMask -> [ Keybinding ]
makeScreenKeys mask =
  [ ((m .|. mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- [ (xK_h, 0), (xK_l, 1) ]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

makeActionKey :: (ButtonMask, KeySym) -> String -> IO [ Keybinding ]
makeActionKey (bm, ks) a = findExecutable "action" >>= \case
  Nothing -> return []
  Just action -> do
      out <- P.readCreateProcess (P.proc action [ "-p", a ]) ""
      let socketPath = head . lines $ out
      let x = ifM (liftIO $ doesFileExist socketPath)
                (liftIO . void $ P.readCreateProcess (P.proc action [ "-t", a ]) "")
                (sendKey bm ks)
      return $ [ ((bm, ks), x) ]

makeKeys :: FilePath -> FilePath
         -> IO (XConfig l -> M.Map (ButtonMask, KeySym) (X ()))
makeKeys bin localBin = do
  as <- fmap concat . sequence $ [ makeActionKey (0, xK_F9) "f9"
                                 , makeActionKey (0, xK_F10) "f10"
                                 , makeActionKey (0, xK_F11) "f11"
                                 , makeActionKey (0, xK_F12) "f12"
                                 ]
  return $ \XConfig { terminal = t } -> M.fromList $
    makeWorkspaceKeys mod4Mask myWorkspaces ++
    makeScreenKeys mod4Mask ++
    as ++
    [ ((mod1Mask .|. shiftMask, xK_Return), spawn t)
    , ((mod1Mask, xK_p), spawn "dmenu_run")
    , ((mod1Mask, xK_k), spawn "k")
    , ((mod1Mask .|. shiftMask, xK_c), kill)
    , ((mod1Mask, xK_space), sendMessage NextLayout)
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp )
    , ((mod1Mask, xK_m), windows W.focusMaster )
    , ((mod1Mask, xK_Return), windows W.swapMaster)
    , ((mod1Mask .|. shiftMask, xK_j), windows W.swapDown )
    , ((mod1Mask .|. shiftMask, xK_k), windows W.swapUp )
    , ((mod1Mask, xK_h), sendMessage Shrink)
    , ((mod1Mask, xK_l), sendMessage Expand)
    , ((mod1Mask, xK_t), withFocused $ windows . W.sink)
    , ((mod1Mask, xK_period), spawn $ "pass-pick")
    , ((mod1Mask .|. shiftMask, xK_period), spawn $ "pass-pick -m")
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
    , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
    , ((controlMask, xK_space), spawn "dunstctl close")
    , ((0, xF86XK_MonBrightnessUp), spawn $ bin </> "brightness +5")
    , ((0, xF86XK_MonBrightnessDown), spawn $ bin </> "brightness -5")
    , ((shiftMask, xF86XK_MonBrightnessUp), spawn $ bin </> "brightness +1")
    , ((shiftMask, xF86XK_MonBrightnessDown), spawn $ bin </> "brightness -1")
    , ((0, xF86XK_AudioRaiseVolume), spawn $ bin </> "volume +1")
    , ((0, xF86XK_AudioLowerVolume), spawn $ bin </> "volume -1")
    , ((shiftMask, xF86XK_AudioRaiseVolume), spawn $ bin </> "volume +5")
    , ((shiftMask, xF86XK_AudioLowerVolume), spawn $ bin </> "volume -5")
    , ((0, xF86XK_AudioMute), spawn $ bin </> "volume m")
    , ((0, xF86XK_Tools), spawn $ localBin </> "bluetooth-fix")
    , ((0, xF86XK_LaunchA), spawn $ localBin </> "displayswitcheroo flip")
    , ((0, xF86XK_Explorer), spawn $ localBin </> "displayswitcheroo switch")
    ]

bars :: XConfig l -> IO (XConfig l)
bars conf = try f >>= \case
  Left (e :: SomeException) -> printf "bars failed: %s\n" (show e) $> conf
  Right conf' -> return conf'
  where f = do
            h <- spawnPipe "statusbar run -f"
            threadDelay 1000000
            return $ docks $ conf { logHook = logHook conf >> dynamicLogWithPP pp { ppOutput = hPutStrLn h } }
        pp = def { ppCurrent         = dzenColor "#ebac54" "#1B1D1E"
                 , ppVisible         = dzenColor "white" "#1B1D1E"
                 , ppHidden          = dzenColor "white" "#1B1D1E"
                 , ppHiddenNoWindows = dzenColor "#7b7b7b" "#1B1D1E"
                 , ppUrgent          = dzenColor "#ff0000" "#1B1D1E"
                 , ppLayout          = dzenColor "#7b7b7b" "#1B1D1E"
                 , ppWsSep           = " "
                 , ppSep             = " | "
                 , ppTitle           = dzenColor "white" "#1B1D1E" . dzenEscape
                 }

main :: IO ()
main = do
  home <- getHomeDirectory
  let (bin, localBin) = (home </> "bin", home </> ".local" </> "bin")
  xdgState <- getXdgDirectory XdgState "xmonad"
  bw <- let fn = xdgState </> "border-width" in
            ifM (doesFileExist fn) (return 0) (read <$> readFile fn)
  myKeys <- makeKeys bin localBin
  xmonad <=< bars $
    def { terminal = "st"
        , workspaces = fst <$> myWorkspaces
        , layoutHook = avoidStruts $ smartBorders (Tall 1 (2/100) (1/2)) ||| noBorders Full -- ||| simpleFloat
        , startupHook = composeAll [ setWMName "LG3D"
                                   ] <+> startupHook def
        , manageHook = composeAll [ className =? "scidDialog" --> doFloat
                                  ] <+> manageHook def
        , keys = myKeys
        , focusFollowsMouse = False
        , focusedBorderColor = "red"
        , borderWidth = bw
        }
