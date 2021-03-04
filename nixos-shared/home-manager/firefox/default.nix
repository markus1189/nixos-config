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
          @-moz-document domain("google.com"), domain("google.de") {
              span > .f { color: darkorange !important; font-weight: bold !important }
          }

          @-moz-document domain("amazon.de") {
              #merchant-info {
                  font-size: 15px; color: darkorange !important;
                  font-weight: bold !important;
                  color: darkorange !important;
              }

              #ddmDeliveryMessage {
                  font-size: 15px !important;
                  font-weight: bold !important;
                  color: darkorange !important;

              }

              #ddmDeliveryMessage > a {
                  font-size: 15px !important;
                  font-weight: bold !important;
                  color: black !important;
              }

              #acrCustomerReviewText {
                  font-size: 15px !important;
                  font-weight: bold !important;
                  color: darkorange !important;
              }

              #sellerProfileTriggerId {
                  font-size: 15px !important;
                  font-weight: bold !important;
                  color: black !important;
              }

              .a-price-whole {
                 font-weight: bold !important;
              }

              span .a-color-price {
                 color: gray !important;
              }
          }
        '';
        path = "managed";
      };
    };
  };
}
