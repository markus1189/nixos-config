{ emacs, mutate, runCommandLocal, fetchurl, emacsPackagesFor, fasd, plantuml
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
  };

  myEmacsConfig = (runCommandLocal "create-my-emacs-config" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${mutatedEmacsConfig} $out/share/emacs/site-lisp/default.el
  '');
  emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
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
in emacsWithPackages (epkgs:
  (with epkgs.melpaPackages;
    with epkgs;
    let
      my_copilot = epkgs.callPackage
        ({ dash, editorconfig, f, fetchFromGitHub, nodejs, s, trivialBuild, }:
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
        version = builtins.replaceStrings [ ".00" ".0"] [ "." "."]
          (builtins.replaceStrings [ "-" "T" ":" ] [ "" "." "" ]
            (builtins.substring 0 16 ndtSources.gptel.date));
        src = ndtSources.gptel.outPath;
      });
    in [
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
      go-mode
      go-complete
      go-autocomplete
      goto-chg
      groovy-mode
      graphviz-dot-mode
      haskell-mode
      # helm
      # helm-flyspell
      # helm-projectile
      # helm-rg
      # helm-swoop
      hl-anything
      hledger-mode
      hurl-mode
      hydra
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
      (kubernetes.overrideAttrs
        (old: { buildInputs = old.buildInputs ++ [ git ]; }))
      # kubel
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
      # helm-lsp
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
      plantuml-mode
      projectile
      protobuf-mode
      quick-yes
      restclient
      rg
      s
      sbt-mode
      scala-mode
      string-inflection
      strace-mode
      persistent-scratch
      pdf-tools
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
      # wgrep-helm
      which-key
      web-mode
      yaml-mode
      yasnippet

      myEmacsConfig
    ] ++ [ my_copilot my_gptel ]) ++ (with epkgs.elpaPackages; [
      # auctex
      #pabbrev
      undo-tree
      csv-mode
      rainbow-mode
    ]))
