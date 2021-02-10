{ }: {
  value = {
    enable = true;
    profiles = {
      "managed" = {
        id = 0;
        extraConfig = "";
        isDefault = true;
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.tabs.closeWindowWithLastTab" = false;
          "browser.sessionstore.warnOnQuit" = true;
          "browser.>warnOnQuit" = true;
        };
        userChrome = "";
        userContent = ''
          @-moz-document domain("news.ycombinator.com") {
              .hnuser { font-size: 20px; color: black !important }
              .score { font-size: 15px }
          }
        '';
        path = "managed";
      };
    };
  };
}
