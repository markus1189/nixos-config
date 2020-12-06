{ lib, cacert, writeScript, secrets, curl, jq, pup, newScope }@args:

let
  scripts =
    (lib.makeScope newScope (self: args)).callPackage ./newsboat-scripts.nix
    { };
  taunusNachrichtenSearch = search: {
    url =
      "https://www.taunus-nachrichten.de/search/content/${search}?solrsort=ds_created%20desc";
    filter = scripts.scrapeTaunusNachrichtenSuche search;
  };
  urlsFile = ./urls;
  urlLines = lib.splitString "\n" (builtins.readFile urlsFile);
  urlFilters = map (attrs: "filter:${attrs.filter}:${attrs.url}") (with scripts;
    [
      {
        url = "https://www.hgon.de/entdecken/";
        filter = scrapeHgonScript;
      }
      {
        url =
          "https://www.hr-fernsehen.de/sendungen-a-z/erlebnis-hessen/sendungen/index.html";
        filter = scrapeErlebnisHessen;
      }
      {
        url = "https://www.taunus-nachrichten.de/nachrichten/umwelt";
        filter = scrapeTaunusNachrichtenUmwelt;
      }
      {
        url = "https://api.rocketbeans.tv/v1/blog/feed/rss2";
        filter = filterItems "morriton";
      }
    ] ++ (map taunusNachrichtenSearch [
      "waldkauz"
      "eisvogel"
      "schleiereule"
      "wildkatze"
    ]));
  taggingRules = {
    "youtube.com" = [ "youtube" "!hide" ];
    "rssbox.herokuapp.com/twitter" = [ "twitter" ];
    "reddit.com" = [ "reddit" ];
    "reddit-top-rss" = [ "reddit" ];
  };
  tagify = url:
    lib.lists.flatten (lib.attrValues
      (lib.filterAttrs (n: v: lib.strings.hasInfix n url) taggingRules));

  subredditToRss = args:
    "filter:${scripts.redditUseCommentsAsLink}:https://reddit-top-rss.herokuapp.com/?subreddit=${args.name}&threshold=${
      toString (args.threshold or 20)
    }&view=rss";
  subreddits = [
    { name = "books"; }
    { name = "commandline"; }
    { name = "compsci"; }
    { name = "dailyprogrammer"; }
    { name = "dataisbeautiful"; }
    { name = "emacs"; }
    { name = "fantasy"; }
    { name = "fountainpens"; }
    { name = "functionalprogramming"; }
    { name = "geb"; }
    { name = "haskell"; }
    { name = "internetisbeautiful"; }
    { name = "luciddreaming"; }
    { name = "malazan"; }
    { name = "nixos"; }
    { name = "notebooks"; }
    {
      name = "scala";
      threshold = 100;
    }
    { name = "ultrarunning"; }
    { name = "writingprompts"; }
    {
      name = "reverseengineering";
      threshold = 30;
    }
    { name = "howtohack"; }
    { name = "liveoverflow"; }
    { name = "netsec"; }
    { name = "exploitdev"; }

    { name = "rpg"; }
    { name = "worldbuilding"; }
    { name = "musicforrpg"; }
    { name = "papertowns"; }
    { name = "maps"; }
    { name = "roll20"; }
    { name = "mrvalor"; }
    { name = "penandpaper"; }
    { name = "podcasts"; }
  ];
in {
  value = {
    enable = true;

    autoReload = true;

    reloadTime = 15;

    urls = map (url: {
      inherit url;
      tags = tagify url;
    }) (urlLines ++ map subredditToRss subreddits) ++ map (url: {
      inherit url;
      tags = [ "filter" ];
    }) urlFilters;

    queries = { "Youtube Videos" = ''tags # "youtube"''; };

    extraConfig = ''
      cleanup-on-quit no
      delete-read-articles-on-quit no
      show-read-feeds no
      show-read-articles no
      confirm-exit yes
      download-full-page yes
      history-limit 9999
      keep-articles-days 15

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

      bookmark-cmd "${scripts.addToPocketScript}"
      bookmark-autopilot yes

      define-filter "this week" "age <= 7"
    '';
  };
}
