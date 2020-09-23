{ lib, writeScriptBin, secrets }:

let
  urlsFile = ./urls;
  urlLines = lib.splitString "\n" (builtins.readFile urlsFile);
  addToPocketScript = writeScriptBin "add-to-pocket.sh" ''
    script_pocket_consumer_key=${secrets.pocket.consumer_key}
    script_pocket_access_token=${secrets.pocket.access_token}

    main() {
        echo "Body is $(jo url="''${1}" consumer_key="''${script_pocket_consumer_key}" access_token="''${script_pocket_access_token}")" >> /tmp/log

        curl -XPOST https://getpocket.com/v3/add -H 'content-type: application/json' -d "$(jo url="''${1}" consumer_key="''${script_pocket_consumer_key}" access_token="''${script_pocket_access_token}")"
    }

    main "$1"
  '';
in {
  value = {
    enable = true;
    autoReload = true;
    reloadTime = 10;
    urls = map (url: { inherit url; }) urlLines;
    extraConfig = ''
      show-read-feeds no
      confirm-exit yes

      color background white black bold
      color listnormal white black
      color listnormal_unread white black bold
      color listfocus black color214
      color listfocus_unread black color214 bold
      color info color214 black bold
      color article white black

      bind-key j down
      bind-key k up
      bind-key g home
      bind-key G end

      macro , bookmark ; toggle-article-read
      macro A mark-feed-read ; quit

      bookmark-cmd "${addToPocketScript}/bin/add-to-pocket.sh"
      bookmark-autopilot yes

      define-filter "this week" "age <= 7"
    '';
  };
}
