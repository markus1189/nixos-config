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
  urlScripts = map (script: "exec:${script}/bin/scrape")
    (with scripts; [ scrapePatreonBigClive scrapeGoethlingKaufmann ]);
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
    "localhost:9999" = [ "reddit" "reddit-top" "!hide" ]; # my local container
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
    {
      owner = "johnwarne";
      repo = "reddit-top-rss";
    }
    {
      owner = "mbnuqw";
      repo = "sidebery";
    }
    {
      owner = "ast-grep";
      repo = "ast-grep";
    }
  ];

  subredditToRss = let
    # baseUrl = "https://reddit-top-rss.herokuapp.com"; # offline heroku instance...
    baseUrl = "http://localhost:9999";
  in args:
  "filter:${scripts.redditUseCommentsAsLink}:${baseUrl}/?subreddit=${args.name}&threshold=${
    toString (args.threshold or 60)
  }&view=rss";

  subreddits = [
    { name = "androidgaming"; }
    { name = "books"; }
    { name = "commandline"; }
    { name = "compsci"; }
    { name = "emacs"; }
    { name = "fantasy"; }
    { name = "functionalprogramming"; }
    { name = "DistributedSystems"; }
    { name = "ProgrammingLanguages"; }
    { name = "ExperiencedDevs"; }
    { name = "garminfenix"; }
    { name = "geb"; }
    { name = "haskell"; }
    { name = "internetisbeautiful"; }
    { name = "malazan"; }
    { name = "nixos"; }
    { name = "notebooks"; }
    { name = "planneraddicts"; }
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
    {
      name = "startrek";
    }

    # RPG
    { name = "ironsworn"; }
    { name = "starforged"; }
    { name = "osr"; }
    { name = "penandpaper"; }
    { name = "pretendingtobepeople"; }
    { name = "rpg"; }
    { name = "rpgnews"; }
    { name = "solo_roleplaying"; }
    { name = "theglasscannonpodcast"; }
    { name = "traveller"; }
    { name = "worldbuilding"; }
    { name = "swn"; }
    { name = "wwn"; }
    { name = "soloboardgaming"; }

    {
      name = "worldnews";
      threshold = 70;
    }

    { name = "tools"; }
    { name = "esp32"; }
    { name = "usbchardware"; }
    { name = "askelectronics"; }
    { name = "gadgets"; }
    { name = "electronics"; }
    { name = "cataclysmdda"; }

    { name = "kettlebell"; }
    { name = "flexibility"; }
    { name = "bodyweightfitness"; }
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
      enabled = false;
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
      title = "Simon Wantling";
      id = "UCzbbkYQUqeGNKSRwoyWB9IA";
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
    {
      title = "Computerphile";
      id = "UC9-y-6csu5WGm29I7JiwpnA";
    }
    {
      title = "NorCal Cycling";
      id = "UCIfRR1N2Gm1vjj9X955iWSQ";
    }
    {
      title = "Questing Beast";
      id = "UCvYwePdbWSEwUa-Pk02u3Zw";
    }
    {
      title = "Seth Skorkowsky";
      id = "UCQs8-UJ7IHsrzhQ-OQOYBmg";
    }
    {
      title = "Glasscannon Network";
      id = "UC83CJFLyDe72XgkKBd5a9IA";
    }
    {
      title = "Great Scott";
      id = "UC6mIxFTvXkWQVEHPsEdflzQ";
    }
    {
      title = "BigCliveDotCom";
      id = "UCtM5z2gkrGRuWd0JQMx76qA";
    }
    {
      title = "ProblemLoeser";
      id = "UCosVFjW2FecfJE3I12-fZag";
    }
    {
      title = "DIY Perks";
      id = "UCUQo7nzH1sXVpzL92VesANw";
    }
    {
      title = "Learn Electronics Repair";
      id = "UCFX1Z9N6aPWuCN_KR8UZ2vg";
    }
    {
      title = "Postapocalyptic Inventor";
      id = "UCDbWmfrwmzn1ZsGgrYRUxoA";
    }
    {
      title = "Jeff Pelletier";
      id = "UCNKMpnM_Yvf6E-Hhf9btYqA";
    }
    {
      title = "EEVblog";
      id = "UC2DjFE7Xf11URZqWBigcVOQ";
    }
    {
      title = "Pine Hollow Auto Diagnostics";
      id = "UCn4Ifss-t3wMT6VBzQoKPUA";
    }
    {
      title = "EJ's Training Camp";
      id = "UCDvIKQseajsYER98pcn46BA";
    }
    {
      title = "Jetpens";
      id = "UCVbn813ctsoChuTT4LuLqrA";
    }
    {
      title = "Basti HW";
      id = "UCsZNco1cxrspWzlPX3Kim9g";
    }
    {
      title = "Sally McRae";
      id = "UC_zyfHGL6MWzm36l9TEWFRg";
    }
    {
      title = "MKMe Lab";
      id = "UCTo55-kBvyy5Y1X_DTgrTOQ";
    }
    {
      title = "Buy it Fix it";
      id = "UChwnFBBtasi2kn2TDK5OsWg";
    }
    {
      title = "My Mate Vince";
      id = "UChY9Cgv-iyPDvf1Bkyx20OQ";
    }
    {
      title = "TechDregs";
      id = "UCSoOJTknGqXQSeamRjEE8aA";
    }
    {
      title = "Worm Girl CDDA";
      id = "UCNQJqvSXfDBOd9spve8doWw";
    }
    {
      title = "ParkNotes";
      id = "UCt1ES-_FMXQfM3JeO_FrOXw";
    }
  ];
  fromKtn = args: {
    url =
      ''https://kill-the-newsletter.com/feeds/${args.id}.xml "~${args.title}"'';
    tags = [ "ktn" ];
  };
  killTheNewsletters = [
    {
      title = "Bike Components";
      id = "bgfqc0awgchwumud";
    }
    {
      title = "Chaosium";
      id = "o2bsigatmzdvp2t2";
    }
    {
      title = "Tor.com";
      id = "sbpv9agei99dsegl";
    }
    {
      title = "Money Stuff";
      id = "appeyt9qbgbukkqh";
    }
    {
      title = "5-Bullet Friday";
      id = "0edslwsaoudstyp9";
    }
    {
      title = "Thinking About Things";
      id = "tc8vjbiw33og592y";
    }
    {
      title = "Leuchtturm1917";
      id = "1ema4onuqfab8l4o";
    }
    {
      title = "Lamy";
      id = "fxshdjutxkvhp4uq";
    }
    {
      title = "Salomon DE";
      id = "xb9ujr6s9d3ed1w1";
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
    }) urlFilters ++ map (exec: {
      url = exec;
      tags = [ "exec" ];
    }) urlScripts ++ map fromGitHubRelease githubReleases
      ++ map fromYoutubeChannel
      (lib.filter (arg: arg.enabled or true) youtubeChannels)
      ++ map fromKtn killTheNewsletters;

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
      confirm-mark-feed-read no
      download-full-page yes
      history-limit 9999
      keep-articles-days 7

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
