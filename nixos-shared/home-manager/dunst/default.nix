{ bash
, mutate
, jo
, systemd
, source-code-pro
, rofi
, xdg-utils
}:

let
  dunstLogger = mutate ./dunst-logger.sh { inherit jo systemd bash; };
in
{
  value = {
    enable = true;
    settings = {
      global = {
        monitor = "0";
        follow = "none";
        width = "400";
        height = "60";
        origin = "bottom-right";
        notification_limit = "5";
        transparency = 20;
        separator_height = 2;
        padding = 6;
        horizontal_padding = 6;
        separator_color = "frame";
        idle_threshold = "5m";
        font = "Source Code Pro 11";
        line_height = 3;
        format = ''<b>%s (%a)</b>\n%b'';
        show_age_threshold = "5m";
        ignore_newline = "false";
        stack_duplicates = "true";
        show_indicators = "true";
        icon_position = "right";
        max_icon_size = 80;
        sticky_history = "true";
        history_length = 99;
        dmenu = "${rofi}/bin/rofi -dmenu";
        browser = "${xdg-utils}/bin/xdg-open";
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
