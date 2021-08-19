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
          "browser.warnOnQuit" = true;
          "network.dnsCacheEntries" = 0;
          "network.dnsCacheExpiration" = 0;
          "devtools.cache.disabled" = true;
          "devtools.netmonitor.persistlog" = true;
        };
        userChrome = "";
        userContent = ''
          /* add '[pdf]' next to links to PDF files */
          a[href$=".pdf"]:after {
            font-size: smaller;
            content: " [pdf]";
          }

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

          @-moz-document domain("jenkins.ft1.cloud.otto.de") {
            .tab > a[href$="Order_Core_Pipeline/"] {
              background: orange !important;
              color: black !important;
            }

            .tab > a[href$="Checkout_Core_Pipeline/"] {
              background: orange !important;
              color: black !important;
            }

            .tab > a[href$="Order_Up_Pipeline/"] {
              background: orange !important;
              color: black !important;
            }

            .tab > a[href$="Order_Common_Pipeline/"] {
              background: orange !important;
              color: black !important;
            }
          }
        '';
        path = "managed";
      };
    };
  };
}
