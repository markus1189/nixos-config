{ lib, writeScript, writeScriptBin, secrets, curl, jq, pup }:

let
  jsonToRssScript = writeScript "json-to-rss" ''
    # Expects the following format:

    # [
    #   {
    #     "pubDate": "",
    #     "title": "",
    #     "link": "",
    #     "description": ""
    #   }
    # ]

    TITLE="''${1:?no-title-given}"
    DESCRIPTION="''${2:?no-description-given}"
    LINK="''${3:?no-link-given}"

    cat <<EOF
<?xml version="1.0" encoding="utf-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
<channel>
<title>$TITLE</title>
<description>$DESCRIPTION</description>
<link>$LINK</link>

$(
  ${jq}/bin/jq -r 'map([
    "<item>",
    "<title>\(.title)</title>",
    "<link>\(.link)</link>",
    "<guid>\(.link)</guid>",
    "<pubDate>\(.pubDate)</pubDate>",
    "</item>"
  ] | join("\n")) | join("\n")'
)

</channel>
</rss>
EOF
  '';
  scrapeHgonScript = writeScript "scrape-hgon.sh" ''
    getItems() {
        ${curl}/bin/curl -s --fail https://www.hgon.de/entdecken/ | ${pup}/bin/pup '.main__content .main__content article json{}'
    }

    buildItems() {
        ${jq}/bin/jq 'map({pubDate: .. | select(try .tag == "time") | .datetime | strptime("%Y.%m.%d ") | strftime("%a, %d %b %Y %H:%M:%S GMT"), title: .. | select(try .tag == "h3") | .text, link: "https://www.hgon.de/\(.. | select(try .tag == "a") | .href)", description: .. | select(try .class == "card__content") | .children | map(.text) | join(" ")})'
    }

    getItems | buildItems | ${jsonToRssScript} "HGON Entdecken (Manual Scrape)" "Manual scrape of HGON news" "https://www.hgon.de/entdecken"
  '';
  scrapeErlebnisHessen = writeScript "scrape-erlebnis-hessen.sh" ''
    getItems() {
        ${curl}/bin/curl -s --fail 'https://www.hr-fernsehen.de/sendungen-a-z/erlebnis-hessen/sendungen/index.html' | ${pup}/bin/pup '.c-teaser__headlineLink json{}'
    }

    buildItems() {
        ${jq}/bin/jq 'map({link: .href, title: [.. | select(.tag? == "span" and (.class? | contains("c-teaser"))) | .. | .text?] | join(" - "), pubDate: .. | select(.datetime?) | .datetime | strptime("%Y-%m-%dT%H:%M+0200") | strftime("%a, %d %b %Y %H:%M:%S GMT"), description: ""})'
    }

    getItems | buildItems | ${jsonToRssScript} "Erlebnis Hessen (Manual Scrape)" "Manual scrape of Erlebnis Hessen" "https://www.hr-fernsehen.de/sendungen-a-z/erlebnis-hessen/sendungen/index.html"
  '';
  urlsFile = ./urls;
  urlLines = lib.splitString "\n" (builtins.readFile urlsFile);
  urlExecs = [
    "exec:${scrapeHgonScript}"
    "exec:${scrapeErlebnisHessen}"
  ];
  addToPocketScript = writeScript "add-to-pocket.sh" ''
    script_pocket_consumer_key=${secrets.pocket.consumer_key}
    script_pocket_access_token=${secrets.pocket.access_token}

    main() {
      unset c
      until ${curl}/bin/curl -s --fail -XPOST https://getpocket.com/v3/add -H 'content-type: application/json' -d "$(jo url="''${1}" consumer_key="''${script_pocket_consumer_key}" access_token="''${script_pocket_access_token}")"; do
        ((c++)) && ((c==4)) && break
        sleep 1
      done
      unset c
    }

    main "$1"
  '';
in {
  value = {
    enable = true;
    autoReload = true;
    reloadTime = 10;
    urls = map (url: { inherit url; }) urlLines ++ map (url: { inherit url; tags = ["execurl"]; }) urlExecs;
    extraConfig = ''
      cleanup-on-quit no
      delete-read-articles-on-quit no
      show-read-feeds no
      confirm-exit yes
      download-full-page yes
      history-limit 9999

      color background white default bold
      color listnormal white default
      color listnormal_unread white default bold
      color listfocus color0 color172
      color listfocus_unread color0 color172 bold
      color info color172 black bold
      color article white default

      bind-key j down
      bind-key k up
      bind-key g home
      bind-key G end

      macro . bookmark ; toggle-article-read
      macro , mark-feed-read ; next-unread-feed

      bookmark-cmd "${addToPocketScript}"
      bookmark-autopilot yes

      define-filter "this week" "age <= 7"
    '';
  };
}
