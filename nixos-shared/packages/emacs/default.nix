{ emacs, mutate, runCommandLocal, fetchurl, fetchzip, fasd, plantuml
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
  emacsPackages = emacs.pkgs.overrideScope (self: super: {
    # flycheck 20260223.1754 has no sha256 in the emacs-overlay yet → src = null → build failure.
    # Pin to last working version (20260219.1225) until the overlay catches up.
    # When upstream fixes it, this block will warn on every build — then remove it.
    flycheck =
      if (super.flycheck.src or null) != null
      then builtins.trace
        "FIXME emacs/default.nix: flycheck override no longer needed, upstream src is available — remove the overrideScope block"
        super.flycheck
      else super.flycheck.overrideAttrs (_: {
        src = fetchzip {
          url = "https://github.com/flycheck/flycheck/archive/ebddfd89b1eea91b8590f542908672569942fb82.tar.gz";
          sha256 = "0gndi96ijxqj6k9qy5d4l0cwqh0ky7w1p27z90ipkn05xz4j3zp5";
        };
        meta = { broken = false; };
      });
  });
in emacsPackages.withPackages (epkgs:
  (with epkgs.melpaPackages;
    with epkgs.elpaPackages;
    with epkgs;
    let
      my_gptel = epkgs.gptel.overrideAttrs (old: rec {
        version = builtins.replaceStrings [ ".00" ".0" ] [ "." "." ]
          (builtins.replaceStrings [ "-" "T" ":" ] [ "" "." "" ]
            (builtins.substring 0 16 ndtSources.gptel.date));
        src = ndtSources.gptel.outPath;
      });
    in [
      (treesit-grammars.with-all-grammars)

      avy
      annotate
      bats-mode
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
      gcmh
      git-link
      git-timemachine
      go-mode
      go-complete
      go-autocomplete
      goto-chg
      groovy-mode
      # graphviz-dot-mode
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
    ] ++ [ my_gptel ]))
