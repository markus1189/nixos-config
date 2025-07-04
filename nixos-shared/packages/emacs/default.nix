{ emacs, mutate, runCommandLocal, fetchurl, emacsWithPackages, fasd, plantuml
, pandoc, git, ndtSources }:

let
  secrets = import ../../secrets.nix;
  mutatedEmacsConfig = mutate ./emacs-config.el {
    inherit fasd plantuml pandoc;
    yesSound = ./yes.wav;
    noSound = ./no.wav;
    popSound = ./pop.wav;
    gptelPerplexityApiKey = secrets.gptel.perplexity;
    gptelGeminiApiKey = secrets.gptel.gemini;
    gptelOpenAiApiKey = secrets.gptel.openai;
    gptelAnthropicApiKey = secrets.gptel.anthropic;
    gptelDeepSeekApiKey = secrets.gptel.deepseek;
    gptelXAIApiKey = secrets.gptel.xai;
    gptelOpenRouterApiKey = secrets.gptel.openrouter;
    pocketConsumerKey = secrets.pocket.consumer_key;
    pocketAccessToken = secrets.pocket.access_token;
    raindropTestToken = secrets.raindrop.test_token;
    mcp_el = ndtSources.mcp-el;
  };

  myEmacsConfig = (runCommandLocal "create-my-emacs-config" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${mutatedEmacsConfig} $out/share/emacs/site-lisp/default.el
  '');
  quick-yes = runCommandLocal "install-quick-yes" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${./quick-yes.el} $out/share/emacs/site-lisp/quick-yes.el
  '';
  dired-plus = runCommandLocal "install-dired-plus" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${ndtSources.emacs-dired-plus} $out/share/emacs/site-lisp/dired+.el
  '';
  iy-go-to-char = runCommandLocal "install-iy-go-to-char" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${ndtSources.iy-go-to-char}/iy-go-to-char.el $out/share/emacs/site-lisp/iy-go-to-char.el
  '';
  hurl-mode = runCommandLocal "hurl-mode" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${ndtSources.emacs-hurl-mode} $out/share/emacs/site-lisp/hurl-mode.el
  '';
in emacs.pkgs.withPackages (epkgs:
  (with epkgs.melpaPackages;
    with epkgs.elpaPackages;
    with epkgs;
    let
      my_copilot = epkgs.callPackage
        ({ dash, editorconfig, f, fetchFromGitHub, nodejs, s, trivialBuild }:
          trivialBuild {
            pname = "copilot";
            version = ndtSources.copilot-emacs.date;
            src = ndtSources.copilot-emacs;
            packageRequires = [ dash editorconfig f nodejs s ];

            meta = {
              description = "An unofficial copilot plugin for Emacs";
              homepage = "https://github.com/copilot-emacs/copilot.el";
              platforms = [ "x86_64-darwin" "x86_64-linux" "x86_64-windows" ];
            };
          }) { };
      my_gptel = epkgs.gptel.overrideAttrs (old: rec {
        version = builtins.replaceStrings [ ".00" ".0" ] [ "." "." ]
          (builtins.replaceStrings [ "-" "T" ":" ] [ "" "." "" ]
            (builtins.substring 0 16 ndtSources.gptel.date));
        src = ndtSources.gptel.outPath;
      });
    in [
      (treesit-grammars.with-all-grammars)

      aidermacs
      avy
      annotate
      beacon
      consult
      consult-project-extra
      company
      dash
      deadgrep
      dhall-mode
      diff-hl
      dired-plus
      dired-filter
      direnv
      dogears
      dockerfile-mode
      docker
      doom-themes
      dumb-jump
      dyalog-mode
      eat
      elfeed
      elfeed-summary
      elfeed-score
      embark
      embark-consult
      eros
      evil
      evil-numbers
      expand-region
      f
      format-all
      epkgs.melpaPackages.fasd
      find-temp-file
      flycheck
      flycheck-haskell
      flycheck-yamllint
      fullframe
      git-link
      git-timemachine
      go-mode
      go-complete
      go-autocomplete
      goto-chg
      groovy-mode
      graphviz-dot-mode
      haskell-mode
      hl-anything
      hledger-mode
      hurl-mode
      hydra
      ialign
      ibuffer-vc
      ibuffer-projectile
      iedit
      indent-guide
      itail
      iy-go-to-char
      js2-refactor
      js2-mode
      json-mode
      jsonnet-mode
      just-mode
      jump-char
      jq-mode
      lua-mode
      log4j-mode
      liso-theme
      ##########
      # LSP mode
      lsp-mode
      lsp-haskell
      lsp-metals
      lsp-treemacs
      which-key
      lsp-ui
      dap-mode
      ##########
      markdown-mode
      markdown-preview-mode
      marginalia
      mvn
      magit
      move-text
      multiple-cursors
      nix-mode
      orderless
      org-drill
      ormolu
      ox-jira
      ox-clip
      pcre2el
      plantuml-mode
      projectile
      protobuf-mode
      quick-yes
      restclient
      rg
      s
      sbt-mode
      scala-ts-mode
      string-inflection
      strace-mode
      persistent-scratch
      pdf-tools
      pocket-reader
      posframe
      rust-mode
      rainbow-delimiters
      smartparens
      solarized-theme
      systemd
      terraform-mode
      transpose-frame
      treemacs
      treemacs-projectile
      typescript-mode
      use-package
      verb
      vertico
      visual-regexp
      which-key
      web-mode
      yaml-mode
      yasnippet
      undo-tree
      csv-mode
      rainbow-mode
      myEmacsConfig
    ] ++ [ my_copilot my_gptel ]))
