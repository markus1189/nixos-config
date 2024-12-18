{ emacs, mutate, runCommandLocal, fetchurl, emacsPackagesFor, mplayer, fasd
, plantuml, pandoc, git, ndtSources }:

let
  mutatedEmacsConfig = mutate ./emacs-config.el {
    inherit mplayer fasd plantuml pandoc;
    yesSound = ./yes.wav;
    noSound = ./no.wav;
    popSound = ./pop.wav;
  };
  myEmacsConfig = (runCommandLocal "create-my-emacs-config" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${mutatedEmacsConfig} $out/share/emacs/site-lisp/default.el
  '');
  emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
  quick-yes = runCommandLocal "install-quick-yes" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${ndtSources.emacs-quick-yes} $out/share/emacs/site-lisp/quick-yes.el
  '';
  dired-plus = runCommandLocal "install-dired-plus" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${ndtSources.emacs-dired-plus} $out/share/emacs/site-lisp/dired+.el
  '';
in emacsWithPackages (epkgs:
  (with epkgs.melpaPackages;
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
    in [
      avy
      annotate
      beacon
      my_copilot
      counsel-jq
      company
      dash
      deadgrep
      dhall-mode
      diff-hl
      dired-plus
      dired-filter
      direnv
      dockerfile-mode
      docker
      doom-themes
      dumb-jump
      dyalog-mode
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
      gptel
      groovy-mode
      graphviz-dot-mode
      haskell-mode
      helm
      helm-flyspell
      helm-projectile
      helm-rg
      helm-swoop
      hl-anything
      hledger-mode
      hydra
      ibuffer-vc
      ibuffer-projectile
      iedit
      indent-guide
      itail
      js2-refactor
      js2-mode
      json-mode
      jsonnet-mode
      just-mode
      jq-mode
      (kubernetes.overrideAttrs
        (old: { buildInputs = old.buildInputs ++ [ git ]; }))
      # kubel
      # lua-mode
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
      helm-lsp
      dap-mode
      ##########
      markdown-mode
      markdown-preview-mode
      mvn
      magit
      move-text
      multiple-cursors
      nix-mode
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
      wgrep-helm
      which-key
      yaml-mode
      yasnippet

      myEmacsConfig
    ]) ++ (with epkgs.elpaPackages; [
      # auctex
      #pabbrev
      undo-tree
      csv-mode
      rainbow-mode
    ]))
