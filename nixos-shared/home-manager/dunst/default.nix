{ bash
, mutate
, jo
, systemd
, source-code-pro
, rofi
}:

let
  dunstLogger = mutate ./dunst-logger.sh { inherit jo systemd bash; };
in
{
  value = {
    enable = true;
    settings = {
      global = {
        font = "Source Code Pro 11";
        allow_markup = "yes";
        plain_text = "no";
        format = ''<b>%s (%a)</b>\n%b'';
        sort = "true";
        indicate_hidden = "yes";
        alignment = "center";
        bounce_freq = 0;
        show_age_threshold = "-1";
        word_wrap = "yes";
        ignore_newline = "no";
        stack_duplicates = "yes";
        hide_duplicates_count = "yes";
        geometry = "400x60-30-45";
        shrink = "no";
        transparency = 20;
        idle_threshold = "10m";
        follow = "keyboard";
        sticky_history = "yes";
        history_length = 42;
        dmenu = "${rofi}/bin/rofi -dmenu";
        show_indicators = "yes";
        line_height = 3;
        separator_height = 2;
        padding = 6;
        horizontal_padding = 6;
        separator_color = "frame";
        startup_notification = "false";
        icon_position = "off";
        max_icon_size = 80;
        mouse_left = "do_action";
        mouse_middle = "do_action";
        mouse_right = "close_current";
      };

      frame = {
        width = 3;
        color = "#2B313C";
      };

      shortcuts = {
        close = "ctrl+space";
        history = "ctrl+grave";
        context = "ctrl+shift+space";
      };

      urgency_low = {
        frame_color = "#3B9700";
        foreground = "#4B9700";
        background = "#191311";
        timeout = 3;
      };

      urgency_normal = {
        frame_color = "#FF8500";
        foreground = "#FF8500";
        background = "#191311";
        timeout = 10;
      };

      urgency_critical = {
        frame_color = "#D40500";
        foreground = "#D40500";
        background = "#191311";
        timeout = 10;
      };

      logging = {
        summary = "*";
        script = "${dunstLogger}";
      };
    };
  };
}
