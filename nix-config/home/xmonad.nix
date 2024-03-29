''
  -- owen-d's XMonad config
  -- This file is managed by NixOS, don't edit it directly!
  -- lovingly crafted from https://github.com/fortuneteller2k/nix-config/blob/master/nixos/config/xmonad.nix
  import XMonad

  import XMonad.Actions.CycleWS
  import XMonad.Actions.GridSelect

  import XMonad.Hooks.DynamicLog
  import XMonad.Hooks.EwmhDesktops
  import XMonad.Hooks.InsertPosition
  import XMonad.Hooks.ManageDocks
  import XMonad.Hooks.ManageHelpers
  import XMonad.Hooks.Place
  import XMonad.Hooks.SetWMName

  import XMonad.Layout.DraggingVisualizer
  import XMonad.Layout.Grid
  import XMonad.Layout.NoBorders
  import XMonad.Layout.Renamed
  import XMonad.Layout.ShowWName
  import XMonad.Layout.Spacing
  import XMonad.Layout.Tabbed
  import XMonad.Layout.ThreeColumns
  import XMonad.Layout.ToggleLayouts

  import XMonad.Prompt
  import XMonad.Prompt.FuzzyMatch
  import XMonad.Prompt.Shell

  import XMonad.Util.EZConfig
  import XMonad.Util.Run
  import XMonad.Util.SpawnOnce
  import XMonad.Util.Paste (sendKey)

  import Graphics.X11.Types
  import Graphics.X11.ExtraTypes.XF86

  import Data.Char
  import Data.Monoid

  import System.IO
  import System.Exit

  import qualified DBus                     as D
  import qualified DBus.Client              as D
  import qualified Codec.Binary.UTF8.String as UTF8
  import qualified XMonad.StackSet          as W
  import qualified Data.Map                 as M
  import qualified XMonad.StackSet as W


  -- mod key - use super (windows key)
  modkey = mod4Mask

  -- 2 workspaces should be enough -- not ready for more than my monitors
  ws = ["A","B", "C"]
  fontFamily = "xft:FantasqueSansMono Nerd Font:size=10:antialias=true:hinting=true"
  fontFamilyLarge = "xft:FantasqueSansMono Nerd Font:size=16:style=Bold:antialias=true:hinting=true"
  keybindings =
    [ ("M-S-<Return>",               spawn "alacritty")
    , ("M-q",                        kill)
    , ("M-c",                        clipboardCopy)
    , ("M-v",                        clipboardPaste)
    , ("M-o",                        goToSelected def) -- shows open windows in grid
    , ("M-S-h",                      prevScreen)
    , ("M-S-l",                      nextScreen)
    , ("M-S-r",                      spawn $ "xmonad --restart && systemctl --user restart polybar")
    , ("M-S-C-4",                    spawn $ "gnome-screenshot -a -f /tmp/screenshot.png && xclip -selection clipboard -t image/png -i /tmp/screenshot.png")
    ]
    where
      clipboardCopy :: X ()
      clipboardCopy =
        withFocused $ \w -> do
          b <- isTerminal w
          if b
            then (sendKey noModMask xF86XK_Copy)
            else (sendKey controlMask xK_c)

      clipboardPaste :: X ()
      clipboardPaste =
        withFocused $ \w -> do
          b <- isTerminal w
          if b
            then (sendKey noModMask xF86XK_Paste)
            else (sendKey controlMask xK_v)

      isTerminal :: Window -> X Bool
      isTerminal =
        fmap (== "Alacritty") . runQuery className


  layouts = avoidStruts $ tabs ||| mtiled ||| tiled ||| centeredMaster ||| grid
    where
       tiled = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ toggleLayouts maximized (smartBorders (Tall 1 (3/100) (1/2)))
       mtiled = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ Mirror (toggleLayouts maximized (smartBorders (Tall 1 (3/100) (1/2))))
       centeredMaster = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ toggleLayouts maximized (smartBorders (ThreeColMid 1 (3/100) (1/2)))
       tabs = stripName 1 1 $ gaps 8 0 $ noBorders (tabbed shrinkText tabTheme)
       grid = stripName 2 0 $ gaps 4 4 $ draggingVisualizer $ toggleLayouts maximized (smartBorders Grid)
       maximized = smartBorders Full
       gaps n k = spacingRaw False (Border n n n n) True (Border k k k k) True
       stripName n k = renamed [Chain [CutWordsLeft n, CutWordsRight k]]

  tabTheme = def
    { fontName            = fontFamily
    , activeColor         = "#e95678"
    , inactiveColor       = "#16161c"
    , urgentColor         = "#ee64ae"
    , activeTextColor     = "#16161c"
    , inactiveTextColor   = "#fdf0ed"
    , urgentTextColor     = "#16161c"
    , activeBorderWidth   = 0
    , inactiveBorderWidth = 0
    , urgentBorderWidth   = 0
    }

  wnameTheme = def
    { swn_font    = fontFamilyLarge
    , swn_bgcolor = "#e95678"
    , swn_color   = "#16161c"
    , swn_fade    = 2
    }

  -- if trying to figure out the correct className, run `xprop | grep WM_CLASS` then click the appropriate window
  windowRules = composeAll
    [ className =? "Google-chrome" --> doShift "B"
    , className =? "Alacritty" --> doShift "A"
    , className =? "wowclassic.exe" --> doFullFloat'
    , className =? "wow.exe" --> doFullFloat'
    , className =? "dota2" --> doFullFloat'
    , className =? "mtga.exe" --> doFullFloat'
    , className =? "Wine" --> doFullFloat'
    ]
    where
      doMaster = doF W.shiftMaster --append this to all floats so new windows always go on top, regardless of the current focus
      doFloat' = doFloat <+> doMaster
      doFullFloat' = doFullFloat <+> doMaster

  autostart = do
    spawnOnce "xwallpaper --zoom ~/.dotfiles/nix-config/home/media/img/neighborhood.jpg &"
    spawnOnce "systemctl --user restart polybar &"
    spawnOnce "alacritty -e tmux"
    spawnOnce "slack"
    spawnOnce "google-chrome-stable"
    -- spawnOnce "xsetroot -cursor_name left_ptr &"
    -- spawnOnce "notify-desktop -u low 'xmonad' 'started successfully'"

  dbusClient = do
      dbus <- D.connectSession
      D.requestName dbus (D.busName_ "org.xmonad.log") opts
      return dbus
    where
      opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  dbusOutput dbus str =
    let
      opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
    in
      D.emit dbus $ signal { D.signalBody = body }

  polybarHook dbus = dynamicLogWithPP $ xmobarPP
    { ppOutput = dbusOutput dbus
    , ppOrder  = \(_:l:_:_) -> [l]
    }

  main' dbus = xmonad . docks . ewmh $ def
    { focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = 2
    , modMask            = modkey
    , workspaces         = ws
    , normalBorderColor  = "#2e303e"
    , focusedBorderColor = "#e95678"
    , layoutHook         = showWName' wnameTheme layouts
    , manageHook         = windowRules
    , logHook            = polybarHook dbus
    , handleEventHook    = fullscreenEventHook
    , startupHook        = autostart
    }
    `additionalKeysP` keybindings
  main = dbusClient >>= main' -- "that was easy, xmonad rocks!"
''
