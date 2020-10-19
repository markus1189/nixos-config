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
  scrapeTaunusNachrichtenUmwelt =
    writeScript "scrape-taunus-nachrichten-umwelt.sh" ''
      getItems() {
          ${pup}/bin/pup '.views-row json{}'
      }

      buildItems() {
          ${jq}/bin/jq 'map({title: .. | select(try .class == "title") | .children[0].text, link: "https://www.taunus-nachrichten.de\(.. | select(try .class == "title") | .children[0] | .href)", pubDate: .. | select(try .class == "timestamp") | .text | strptime("%d. %B %Y") | strftime("%a, %d %b %Y %H:%M:%S GMT"), description: .. | select(try .class == "teaser") | .children | map(.text) | join(" ")})'
      }

      getItems | buildItems | ${jsonToRssScript} "Taunus Nachrichten Umwelt (Manual Scrape)" "Manual scrape of Umwelt on Taunus Nachrichten" "https://www.taunus-nachrichten.de/nachrichten/umwelt"
    '';
  scrapeHgonScript = writeScript "scrape-hgon.sh" ''
    getItems() {
        ${pup}/bin/pup '.main__content .main__content article json{}'
    }

    buildItems() {
        ${jq}/bin/jq 'map({pubDate: .. | select(try .tag == "time") | .datetime | strptime("%Y.%m.%d ") | strftime("%a, %d %b %Y %H:%M:%S GMT"), title: .. | select(try .tag == "h3") | .text, link: "https://www.hgon.de/\(.. | select(try .tag == "a") | .href)", description: .. | select(try .class == "card__content") | .children | map(.text) | join(" ")})'
    }

    getItems | buildItems | ${jsonToRssScript} "HGON Entdecken (Manual Scrape)" "Manual scrape of HGON news" "https://www.hgon.de/entdecken"
  '';
  scrapeErlebnisHessen = writeScript "scrape-erlebnis-hessen.sh" ''
    getItems() {
        ${pup}/bin/pup '.c-teaser__headlineLink json{}'
    }

    buildItems() {
        ${jq}/bin/jq 'map({link: .href, title: [.. | select(.tag? == "span" and (.class? | contains("c-teaser"))) | .. | .text?] | join(" - "), pubDate: .. | select(.datetime?) | .datetime | strptime("%Y-%m-%dT%H:%M+0200") | strftime("%a, %d %b %Y %H:%M:%S GMT"), description: ""})'
    }

    getItems | buildItems | ${jsonToRssScript} "Erlebnis Hessen (Manual Scrape)" "Manual scrape of Erlebnis Hessen" "https://www.hr-fernsehen.de/sendungen-a-z/erlebnis-hessen/sendungen/index.html"
  '';
  urlsFile = ./urls;
  urlLines = lib.splitString "\n" (builtins.readFile urlsFile);
  urlExecs = [ ];
  urlFilters = [
    "filter:${scrapeHgonScript}:https://www.hgon.de/entdecken/"
    "filter:${scrapeErlebnisHessen}:https://www.hr-fernsehen.de/sendungen-a-z/erlebnis-hessen/sendungen/index.html"
    "filter:${scrapeTaunusNachrichtenUmwelt}:https://www.taunus-nachrichten.de/nachrichten/umwelt"
  ];
  addToPocketScript = writeScript "add-to-pocket.sh" ''
    TAGS="newsboat"
    URL="''${1}"

    script_pocket_consumer_key=${secrets.pocket.consumer_key}
    script_pocket_access_token=${secrets.pocket.access_token}

    if echo "''${URL}" | grep 'youtube.com/watch'; then
        TAGS="$TAGS,youtube,video"
    fi

    if echo "''${URL}" | grep 'reddit.com'; then
        TAGS="$TAGS,reddit"
    fi

    if echo "''${URL}" | grep 'news.ycombinator.com'; then
        TAGS="$TAGS,hackernews"
    fi

    main() {
      unset c
      until ${curl}/bin/curl -s --fail -XPOST https://getpocket.com/v3/add -H 'content-type: application/json' -d "$(jo url="''${1}" consumer_key="''${script_pocket_consumer_key}" access_token="''${script_pocket_access_token}" tags="''${TAGS}")"; do
        ((c++)) && ((c==4)) && break
        sleep 1
      done
      unset c
    }

    main "$1"
  '';
  tagFromInfix = infix: tags: url: lib.optionals (lib.strings.hasInfix infix url) tags;
  tagYoutube = tagFromInfix "youtube.com" [ "youtube" "!hide" ];
  tagReddit = tagFromInfix "reddit.com" [ "reddit" ];
  tagify = url: tagYoutube url ++ tagReddit url;
in {
  value = {
    enable = true;
    autoReload = true;
    reloadTime = 10;
    urls = map (url: {
      inherit url;
      tags = tagify url;
    }) urlLines ++ map (url: {
      inherit url;
      tags = [ "execurl" ];
    }) urlExecs ++ map (url: {
      inherit url;
      tags = [ "filter" ];
    }) urlFilters;
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
