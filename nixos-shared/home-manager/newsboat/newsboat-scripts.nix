{ writeScript, writeScriptBin, jq, gzip, pup, secrets, curl, cacert, yq, ... }: rec {
  jsonToRssScript = writeScriptBin "json-to-rss" ''
        # Expects the following format:

        # [
        #   {
        #     "pubDate": "",
        #     "title": "",
        #     "link": "",
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
        if has("pubDate") then "<pubDate>\(.pubDate)</pubDate>" else "" end,
        if has("description") then "<description>\(.description)</description>" else "" end,
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

      getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss "Taunus Nachrichten Umwelt (Manual Scrape)" "Manual scrape of Umwelt on Taunus Nachrichten" "https://www.taunus-nachrichten.de/nachrichten/umwelt"
    '';
  scrapeTaunusNachrichtenSuche = search:
    writeScript "scrape-taunus-nachrichten-suche.sh" ''
      getItems() {
        ${pup}/bin/pup 'div .buildmode-3 json{}'
      }

      buildItems() {
          ${jq}/bin/jq 'map({title: .. | select(try .class | contains("field-title")) | .children[].children[].text, link: "https://www.taunus-nachrichten.de\(.. | select(try .class == "field field-title") | .children[].children[].href)",pubDate: .. | select(try .class | contains("field-post-date")) | .text | strptime("%d. %B %Y") | strftime("%a, %d %b %Y %H:%M:%S GMT"), description: .. | select(try .class | contains("-teaser")) | .text})'
      }

      getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss "Taunus Nachrichten - Search for \"${search}\"" "Manual scrape of a search onTaunus Nachrichten" "https://www.taunus-nachrichten.de"
    '';
  scrapeHgonScript = writeScript "scrape-hgon.sh" ''
    getItems() {
        ${pup}/bin/pup '.main__content .main__content article json{}'
    }

    buildItems() {
        ${jq}/bin/jq 'map({pubDate: .. | select(try .tag == "time") | .datetime | strptime("%Y.%m.%d ") | strftime("%a, %d %b %Y %H:%M:%S GMT"), title: .. | select(try .tag == "h3") | .text, link: "https://www.hgon.de/\(.. | select(try .tag == "a") | .href)", description: .. | select(try .class == "card__content") | .children | map(.text) | join(" ")})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss "HGON Entdecken (Manual Scrape)" "Manual scrape of HGON news" "https://www.hgon.de/entdecken"
  '';
  scrapeErlebnisHessen = writeScript "scrape-erlebnis-hessen.sh" ''
    getItems() {
        ${pup}/bin/pup '.c-teaser__headlineLink json{}'
    }

    buildItems() {
        ${jq}/bin/jq 'map({link: .href, title: [.. | select(.tag? == "span" and (.class? | contains("c-teaser"))) | .. | .text?] | join(" - "), pubDate: .. | select(.datetime?) | .datetime | strptime("%Y-%m-%dT%H:%M+0200") | strftime("%a, %d %b %Y %H:%M:%S GMT"), description: ""})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss "Erlebnis Hessen (Manual Scrape)" "Manual scrape of Erlebnis Hessen" "https://www.hr-fernsehen.de/sendungen-a-z/erlebnis-hessen/sendungen/index.html"
  '';
  scrapeDriveThruRPG = writeScriptBin "scrape" ''
    getItems() {
       ${pup}/bin/pup 'table:first-child h3 a:nth-child(odd) json{}'
    }

    buildItems() {
       ${jq}/bin/jq -r '.[:10] | map({link: .href[0:43],title: .text, description: ""})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss 'DriveThruRPG Top 10' "Bestselling 10 titles from DriveThruRPG Top 100" 'https://www.drivethrurpg.com/top_100.php'
  '';

  scrapeDragonPlusMagaine = writeScriptBin "scrape" ''
    getItems() {
       ${pup}/bin/pup 'article json{}'
    }

    buildItems() {
       ${jq}/bin/jq 'map({title: .children[0].children[1].children[0].children[0].text, link: .children[0].children[1].children[0].children[0].href,description: .children[0].children[1].children[1].children[0].text | gsub("^:\\s+"; "Release Date: ")})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss 'Dragon+ Magazine' "Dragon+ Magazine Issues" 'https://dnd.wizards.com/content/dragon'
  '';

  scrapeFnpMtk = writeScriptBin "scrape" ''
    getItems() {
       ${pup}/bin/pup '.id-LinkOverlay-link json{}'
    }

    buildItems() {
       ${jq}/bin/jq 'map(select(.href != "") | {title: .title,link: "https://www.fnp.de\(.href)"})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss 'FNP: Main Taunus Kreis' "Nachrichten von fnp.de für MTK" 'https://www.fnp.de/lokales/main-taunus/'
  '';

  scrapePatreonBigClive = writeScriptBin "scrape" ''
    getItems() {
      ${curl}/bin/curl -s 'https://www.patreon.com/api/posts?include=user.null%2Caccess_rules.tier.null%2Cattachments.null%2Caudio.null%2Cimages.null%2Cpoll.choices.null%2Cpoll.current_user_responses.null&fields[user]=full_name%2Cimage_url%2Curl&fields[post]=comment_count%2Ccontent%2Ccontent_teaser_text%2Ccurrent_user_can_view%2Cembed%2Cimage%2Cis_paid%2Clike_count%2Cmin_cents_pledged_to_view%2Cpatreon_url%2Cpledge_url%2Cpost_file%2Cpost_type%2Cpost_metadata%2Cpublished_at%2Cteaser_text%2Cthumbnail%2Ctitle%2Cupgrade_url%2Curl%2Cvideo_preview&fields[reward]=[]&fields[access-rule]=access_rule_type%2Camount_cents%2Cpost_count&fields[media]=download_url%2Cimage_urls%2Cmetadata&filter[campaign_id]=187993&filter[contains_exclusive_posts]=true&filter[is_draft]=false&page[size]=50&sort=-published_at&json-api-use-default-includes=false&json-api-version=1.0' --globoff -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:108.0) Gecko/20100101 Firefox/108.0' -H 'Accept: */*' -H 'Accept-Language: en-US,en;q=0.5' -H 'Accept-Encoding: gzip, deflate, br' -H 'Referer: https://www.patreon.com/bigclive' -H 'Content-Type: application/vnd.api+json' -H 'DNT: 1' -H 'Connection: keep-alive' -H 'Sec-Fetch-Dest: empty' |
        ${gzip}/bin/gunzip |
        ${jq}/bin/jq '.data | map({id, attributes: .attributes | {title,url,teaser_text,published_at}})'
    }

    buildItems() {
       ${jq}/bin/jq 'map({title: .attributes.title, link: .attributes.url, guid: .attributes.url, pubDate: .attributes.published_at, description: .attributes.teaser_text})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss 'Patreon: BigClive' "Creating Technical teardowns and creations." 'https://www.patreon.com/bigclive'
  '';

  scrapeGoethlingKaufmann = writeScriptBin "scrape" ''
    getItems() {
      ${curl}/bin/curl -s 'https://www.goethling-kaufmann.de/wp-json/ws/v1/stock/listing/?orderField=enteredInStockDate&orderMode=desc&type=USED&fuelType=Benzin&gearType=Automatik&hp.min=100&price.min=16000&city=Hofheim%7CEschborn%7CKelkheim&price.max=35000&year.min=2015&bodyType=Kombi%7CSUV%7CVan' \
          --globoff \
          -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/110.0' \
          -H 'Accept: application/json' |
          ${jq}/bin/jq '.response.vehiclesCollection | map({hp,makeModel,registration:"\(.registration.month)/\(.registration.year)",km,price: .price.current,link:.url})'
    }

    buildItems() {
       ${jq}/bin/jq 'map({title: "\(.makeModel) for \(.price) at \(.km) (\(.hp), \(.registration))", link})'
    }

    getItems | buildItems | ${jsonToRssScript}/bin/json-to-rss 'Goethling und Kaufmann' "Autohaus Göthling und Kaufmann" 'https://www.goethling-kaufmann.de/'
  '';

  addToPocketScript = writeScript "add-to-pocket.sh" ''
    TAGS="newsboat"
    URL="''${1}"

    script_pocket_consumer_key=${secrets.pocket.consumer_key}
    script_pocket_access_token=${secrets.pocket.access_token}

    if echo "''${URL}" | grep 'youtube.com/watch'; then
        TAGS="$TAGS,youtube,video"
    fi

    if echo "''${URL}" | grep 'hr-fernsehen.de/sendungen'; then
        TAGS="$TAGS,hr,video"
    fi

    if echo "''${URL}" | grep 'reddit.com'; then
        TAGS="$TAGS,reddit"
        if echo "''${URL}" | grep -o 'r/[^/]*'; then
          TAGS="$TAGS,$(echo "''${URL}" | grep -o 'r/[^/]*')"
        fi
    fi

    if echo "''${URL}" | grep 'news.ycombinator.com'; then
        TAGS="$TAGS,hackernews"
    fi

    if echo "''${URL}" | grep -e 'xkcd.com' -e 'monkeyuser.com'; then
        TAGS="$TAGS,comic"
    fi

    main() {
      unset c
      until ${curl}/bin/curl --cacert "${cacert}/etc/ssl/certs/ca-bundle.crt" -s --fail -XPOST https://getpocket.com/v3/add -H 'content-type: application/json' -d "$(jo url="''${1}" consumer_key="''${script_pocket_consumer_key}" access_token="''${script_pocket_access_token}" tags="''${TAGS}")"; do
        ((c++)) && ((c==6)) && break
        sleep 1
      done
      unset c
      exit "$?"
    }

    main "$1"
  '';

  redditUseCommentsAsLink = writeScript "reddit-comments-link" ''
    ${yq}/bin/xq --xml-dtd -x '.rss.channel.item |= map(.link = try .comments catch .link)'
  '';

  filterItems = term: writeScript "newsboat-filter-items" ''
    ${yq}/bin/xq --xml-dtd -x '.rss.channel.item |= map(select(.title | test("${term}"; "i")))'
  '';
}
