{ emacs, mutate, runCommandLocal, fetchurl, fetchzip, fasd, plantuml
, pandoc, git, ndtSources }:

let
  mutatedEmacsConfig = mutate ./emacs-config.el {
    inherit fasd plantuml pandoc;
    yesSound = ./yes.wav;
    noSound = ./no.wav;
    popSound = ./pop.wav;
  };

  myEmacsConfig = (runCommandLocal "create-my-emacs-config" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${mutatedEmacsConfig} $out/share/emacs/site-lisp/default.el
  '');
  quick-yes = runCommandLocal "install-quick-yes" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${./quick-yes.el} $out/share/emacs/site-lisp/quick-yes.el
  '';
  # Single-file elisp packages, pinned by commit (previously moving-branch
  # URLs in ndt/sources.json).
  dired-plus-el = fetchurl {
    url = "https://raw.githubusercontent.com/emacsmirror/dired-plus/56f76725b5f151ed8a4ad17a62edf2fd592edb3a/dired+.el";
    sha256 = "0r5cyrra6q7w0cdv140pkn7y5hivlmj60bv1pniqfl2p40b67a5l";
  };
  iy-go-to-char-el = fetchurl {
    url = "https://raw.githubusercontent.com/doitian/iy-go-to-char/04ab4f5f3a241cbbc9b8c178a22b412a62f632f9/iy-go-to-char.el";
    sha256 = "0gs7d39s602ypvxgwmi93jskmx0vzkwmg5ryai9m30zdp8q881cl";
  };
  hurl-mode-el = fetchurl {
    url = "https://raw.githubusercontent.com/Orange-OpenSource/hurl/7009ffc52238dc46b8d4073a447590ddb694413e/contrib/emacs/hurl-mode.el";
    sha256 = "1aibnicrlsncs16nlcfgv1n84h5y3zb949ba5wzqpa4q6xsfn1lv";
  };
  dired-plus = runCommandLocal "install-dired-plus" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${dired-plus-el} $out/share/emacs/site-lisp/dired+.el
  '';
  iy-go-to-char = runCommandLocal "install-iy-go-to-char" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${iy-go-to-char-el} $out/share/emacs/site-lisp/iy-go-to-char.el
  '';
  hurl-mode = runCommandLocal "hurl-mode" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${hurl-mode-el} $out/share/emacs/site-lisp/hurl-mode.el
  '';
  emacsPackages = emacs.pkgs.overrideScope (self: super: {
    # WORKAROUND (2026-06-28): the 2026-06-27 projectile snapshot ships
    # projectile-consult.el, which does (require 'consult) at byte-compile
    # time. Its MELPA recipe omits consult from :reqs, so consult is absent
    # from the load path during compilation and the build aborts with
    # "Cannot open load file: ... consult", taking the whole system build
    # with it. We inject consult into projectile's compile/runtime inputs.
    #
    # The tripwire below fails the build with a note once consult lands in
    # projectile's packageRequires upstream, so this hack gets removed
    # rather than silently lingering forever.
    projectile =
      let
        reqNames = map (p: p.pname or p.ename or p.name or "")
          (super.projectile.packageRequires or []);
        fixedUpstream = builtins.elem "consult" reqNames;
      in
      if fixedUpstream
      then builtins.throw ''
        nixos-config: the projectile consult byte-compile workaround is no
        longer needed — 'consult' is now declared in projectile's
        packageRequires upstream. Remove the projectile override in
        nixos-shared/packages/emacs/default.nix.
      ''
      else super.projectile.overrideAttrs (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ [ self.consult ];
        propagatedUserEnvPkgs = (old.propagatedUserEnvPkgs or []) ++ [ self.consult ];
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

      annotate
      auctex
      avy
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
      emmet-mode
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
      with-editor
      yaml-mode
      yasnippet
      undo-tree
      csv-mode
      rainbow-mode
      myEmacsConfig
    ] ++ [ my_gptel ]))
