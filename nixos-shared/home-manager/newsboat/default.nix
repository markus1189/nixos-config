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
        url = "https://www.drivethrurpg.com/top_100.php";
        filter = "${scrapeDriveThruRPG}/bin/scrape";
      }
      {
        url = "https://dnd.wizards.com/content/dragon";
        filter = "${scrapeDragonPlusMagaine}/bin/scrape";
      }
      {
        url = "https://www.fnp.de/lokales/main-taunus/";
        filter = "${scrapeFnpMtk}/bin/scrape";
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
    "reddit.com" = [ "reddit" "!hide" ];
    "reddit-top-rss" = [ "reddit" "reddit-top" "!hide" ];
  };
  tagify = url:
    lib.lists.flatten (lib.attrValues
      (lib.filterAttrs (n: v: lib.strings.hasInfix n url) taggingRules));

  fromGitHubRelease = { owner, repo }: {
    url = "https://github.com/${owner}/${repo}/releases.atom";
    tags = [ "github-release" "!hide" ];
  };

  githubReleases = [
    {
      owner = "CMB";
      repo = "edbrowse";
    }
    {
      owner = "pwmt";
      repo = "zathura";
    }
    {
      owner = "mpv-player";
      repo = "mpv";
    }
    {
      owner = "mwh";
      repo = "dragon";
    }
    {
      owner = "jarun";
      repo = "ddgr";
    }
    {
      owner = "solemnwarning";
      repo = "rehex";
    }
    {
      owner = "eXeC64";
      repo = "imv";
    }
    {
      owner = "BestImageViewer";
      repo = "geeqie";
    }
    {
      owner = "Duncaen";
      repo = "OpenDoas";
    }
    {
      owner = "nushell";
      repo = "nushell";
    }
    {
      owner = "elves";
      repo = "elvish";
    }
    {
      owner = "tmux";
      repo = "tmux";
    }
    {
      owner = "kmonad";
      repo = "kmonad";
    }
    {
      owner = "saulpw";
      repo = "visidata";
    }
  ];

  subredditToRss = args:
    "filter:${scripts.redditUseCommentsAsLink}:https://reddit-top-rss.herokuapp.com/?subreddit=${args.name}&threshold=${
      toString (args.threshold or 50)
    }&view=rss";

  subreddits = [
    { name = "androidgaming"; }
    { name = "deadcells"; }
    { name = "books"; }
    { name = "commandline"; }
    { name = "compsci"; }
    { name = "emacs"; }
    { name = "fantasy"; }
    { name = "functionalprogramming"; }
    { name = "DistributedSystems"; }
    { name = "ProgrammingLanguages"; }
    {
      name = "ExperiencedDevs";
      threshold = 60;
    }
    {
      name = "garminfenix";
      threshold = 60;
    }
    { name = "geb"; }
    { name = "haskell"; }
    { name = "internetisbeautiful"; }
    { name = "malazan"; }
    { name = "nixos"; }
    { name = "notebooks"; }
    { name = "netsec"; }
    {
      name = "scala";
      threshold = 100;
    }
    { name = "ultrarunning"; }
    { name = "trailrunning"; }
    {
      name = "writingprompts";
      threshold = 80;
    }

    # RPG
    { name = "ironsworn"; }
    { name = "starforged"; }
    { name = "musicforrpg"; }
    { name = "osr"; }
    { name = "penandpaper"; }
    { name = "PretendingToBePeople"; }
    { name = "rpg"; }
    { name = "rpgnews"; }
    { name = "solo_roleplaying"; }
    { name = "worldbuilding"; }
    { name = "swn"; }
    { name = "wwn"; }
    {
      name = "soloboardgaming";
    }

    # Crypto
    { name = "CryptoCurrencies"; }
    { name = "CryptoMarkets"; }

    {
      name = "worldnews";
      threshold = 70;
    }
  ];
  fromYoutubeChannel = args: {
    url = "https://www.youtube.com/feeds/videos.xml?channel_id=${args.id}";
    tags = [ "youtube" "!hide" ];
  };
  youtubeChannels = [
    {
      title = "Chris Kaula";
      id = "UCl_5s2WDFi38LiXUQA7CTWQ";
      enabled = false;
    }
    {
      title = "Iron Home";
      id = "UCt8REhn8USXlxhiTc9H_F3w";
    }
    {
      title = "Daily MTB Rider";
      id = "UCHMfbHsJZCqRtSCVLV-RnYA";
    }
    {
      title = "Aiko Sukdolak";
      id = "UC4hJTeF0QvnsC7bIshU_K2w";
    }
    {
      title = "Steve Mattheis";
      id = "UCDIr0UgrBJ3lGfs0eeKV6Tw";
    }
    {
      title = "Figboot on Pens";
      id = "UCI1aF4MNqSzKIS2t0KHS1gw";
    }
    {
      title = "Trond Westby";
      id = "UCJV7ONWjegVFOlHpAGgjGMQ";
    }
    {
      title = "Stefano Ianiro Wildlife";
      id = "UCKq3tXnvXnA0feJYmOx9MPw";
    }
    {
      title = "Ben Fitzcosta";
      id = "UCMYHM19nuv892Qo8ZlLvUaw";
    }
    {
      title = "Morten Hilmer";
      id = "UCYzMdQa5aD6dnKHLdBxDMkw";
    }
    {
      title = "SuperTragopan";
      id = "UCa51ED7iENUjtadDnqPuoWw";
    }
    {
      title = "Simon Baxter";
      id = "UCcGPU4A6xJ1OYOkvfMoo25w";
    }
    {
      title = "exurb1a";
      id = "UCimiUgDLbi6P17BdaCZpVbg";
    }
    {
      title = "Ornithologie für Anfänger";
      id = "UCkJQqECiHJ3UmTeztifoFpg";
    }
    {
      title = "Mark Smith";
      id = "UCyGYUrC2IvaHWoX6dwEsrMA";
    }
    {
      title = "Simon Wantling";
      id = "UCzbbkYQUqeGNKSRwoyWB9IA";
    }
    {
      title = "Global Mountain Bike Network";
      id = "UC_A--fhX5gea0i4UtpD99Gg";
    }
    {
      title = "GOLDEN TRAIL SERIES";
      id = "UC8szqVDJF60HueoqJrD50qw";
    }
    {
      title = "Saul Pwanson - Visidata";
      id = "UCDw36yB-ZXJ_FnqEH7o2HfQ";
    }
    {
      title = "MotionTwin (Dead Cells)";
      id = "UCqeVdbCP-fVjSGEVnY-3lWQ";
    }
  ];
in {
  value = {
    enable = true;

    autoReload = true;

    reloadTime = 15;
    reloadThreads = 10;

    urls = map (url: {
      inherit url;
      tags = tagify url;
    }) (urlLines ++ map subredditToRss subreddits) ++ map (url: {
      inherit url;
      tags = [ "filter" ];
    }) urlFilters ++ map fromGitHubRelease githubReleases
      ++ map fromYoutubeChannel
      (lib.filter (arg: arg.enabled or true) youtubeChannels);

    queries = {
      "Youtube Videos" = ''tags # "youtube"'';
      "Top Posts From Subreddits" = ''tags # "reddit-top"'';
      "Github Releases" = ''tags # "github-release"'';
    };

    extraConfig = ''
      cleanup-on-quit no
      delete-read-articles-on-quit no
      show-read-feeds no
      show-read-articles no
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

      bookmark-cmd "${scripts.addToPocketScript}"
      bookmark-autopilot yes

      define-filter "this week" "age <= 7"
    '';
  };
}
