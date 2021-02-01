let
  dark = {
    primary = {
      background = "0x16161c";
      foreground = "0xfdf0ed";
    };
    normal = {
      black = "0x232530";
      red = "0xe95678";
      green = "0x29d398";
      yellow = "0xfab795";
      blue = "0x26bbd9";
      magenta = "0xee64ae";
      cyan = "0x59e3e3";
      white = "0xfadad1";
    };
    bright = {
      black = "0x2e303e";
      red = "0xec6a88";
      green = "0x3fdaa4";
      yellow = "0xfbc3a7";
      blue = "0x3fc6de";
      magenta = "0xf075b7";
      cyan = "0x6be6e6";
      white = "0xfdf0ed";
    };
  };

  pastel = {
    primary = {
      background ="0x322957";
      foreground ="0xe5e5e5";

      dim_foreground ="0xe5e5e5";
      bright_foreground ="0xe5e5e5";
      dim_background ="0x322957";
      bright_background ="0x322957";
    };

    # Normal colors
    normal = {
      black ="0x000000";
      red ="0xff7092";
      green ="0x00faac";
      yellow ="0xfef96a";
      blue ="0x00beff";
      magenta ="0xde95ff";
      cyan ="0x85cbfd";
      white ="0xffffff";
    };
    # Bright colors
    bright = {
      black ="0x000000";
      red ="0xff89a3";
      green ="0x20f6bb";
      yellow ="0xfff687";
      blue ="0x1bcbfc";
      magenta ="0xe5adfd";
      cyan ="0x99d6fc";
      white ="0xfefff";
    };

    # Dim colors
    dim = {
      black ="0x000000";
      red ="0xff7092";
      green ="0x00faac";
      yellow ="0xfef96a";
      blue ="0x00beff";
      magenta ="0xde95ff";
      cyan ="0x85cbfd";
      white ="0xffffff";
    };
  };

in

{
  env.TERM = "xterm-256color";
  font = {
    size = 10.5;
  };
  window.padding = {
    x = 8;
    y = 8;
  };
  colors = dark;

  bell = {
    animation = "EaseOutExpo";
    duration = 0;
  };

  mouse = {
    hide_when_typing = true;
  };

  key_bindings = [
    {
      # M-x
      key = "x";
      mods = "Alt";
      chars = "\x1bx";
    }
  ];
}
