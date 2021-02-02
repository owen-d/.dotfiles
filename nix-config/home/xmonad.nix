''
  -- owen-d's XMonad config
  -- This file is managed by NixOS, don't edit it directly!
  -- lovingly crafted from https://github.com/fortuneteller2k/nix-config/blob/master/nixos/config/xmonad.nix
  import XMonad

  import XMonad.Actions.CycleWS

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

  import Data.Char
  import Data.Monoid

  import System.IO
  import System.Exit

  import qualified DBus                     as D
  import qualified DBus.Client              as D
  import qualified Codec.Binary.UTF8.String as UTF8
  import qualified XMonad.StackSet          as W
  import qualified Data.Map                 as M


  -- mod key - use super (windows key)
  modkey = mod4Mask

  -- 4 workspaces should be enough
  ws = ["A","B","C","D"]
  fontFamily = "xft:FantasqueSansMono Nerd Font:size=10:antialias=true:hinting=true"
  fontFamilyLarge = "xft:FantasqueSansMono Nerd Font:size=16:style=Bold:antialias=true:hinting=true"
  keybindings =
    [ ("M-S-<Return>",               spawn "alacritty")
    , ("M-q",                        kill)
    , ("M-S-r",                      spawn $ "xmonad --restart && systemctl --user restart polybar")
    ]
    ++
    [ (otherModMasks ++ "M-" ++ key, action tag)
        | (tag, key) <- zip ws (map (\x -> show x) ([1..9] ++ [0]))
        , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                     , ("S-", windows . W.shift)]
    ]

  layouts = avoidStruts $ tiled ||| mtiled ||| tabs ||| centeredMaster ||| grid
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
  autostart = do
    spawnOnce "xwallpaper --zoom ~/.dotfiles/nix-config/home/media/img/akira0.jpg &"
    return ()
    -- spawnOnce "xsetroot -cursor_name left_ptr &"
    -- spawnOnce "systemctl --user restart polybar &"
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
    , logHook            = polybarHook dbus
    , startupHook        = autostart
    }
    `additionalKeysP` keybindings
  main = dbusClient >>= main' -- "that was easy, xmonad rocks!"
''
