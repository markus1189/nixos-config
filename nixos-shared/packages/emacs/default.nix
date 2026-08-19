{
  emacs,
  replaceVars,
  runCommandLocal,
  plantuml,
  elispSrcs,
}:

let
  emacsConfig = replaceVars ./emacs-config.el {
    inherit plantuml;
  };

  myEmacsConfig = (
    runCommandLocal "create-my-emacs-config" { } ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${emacsConfig} $out/share/emacs/site-lisp/default.el
    ''
  );
  quick-yes = runCommandLocal "install-quick-yes" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${./quick-yes.el} $out/share/emacs/site-lisp/quick-yes.el
  '';
  # Single-file elisp packages without a MELPA recipe; sources are flake
  # inputs (see flake.nix), so they move with `nix flake update`.
  iy-go-to-char = runCommandLocal "install-iy-go-to-char" { } ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${elispSrcs.iy-go-to-char}/iy-go-to-char.el $out/share/emacs/site-lisp/iy-go-to-char.el
  '';
in
emacs.pkgs.withPackages (
  epkgs:
  (
    with epkgs.melpaPackages;
    with epkgs.elpaPackages;
    with epkgs;
    let
      my_gptel = epkgs.gptel.overrideAttrs (old: rec {
        # MELPA-style date version (YYYYMMDD.HMM) from the flake input's
        # lastModifiedDate (YYYYMMDDHHMMSS).
        version =
          builtins.replaceStrings [ ".00" ".0" ] [ "." "." ]
            "${builtins.substring 0 8 elispSrcs.gptel.lastModifiedDate}.${
              builtins.substring 8 4 elispSrcs.gptel.lastModifiedDate
            }";
        src = elispSrcs.gptel;
      });
    in
    [
      (treesit-grammars.with-all-grammars)

      annotate
      auctex
      avy
      bats-mode
      beacon
      cape
      consult
      consult-project-extra
      corfu
      dash
      deadgrep
      dhall-mode
      diff-hl
      dirvish
      envrc
      dogears
      dockerfile-mode
      docker
      doom-themes
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
      flymake-collection
      gcmh
      git-link
      git-timemachine
      go-mode
      go-complete
      groovy-mode
      haskell-mode
      hl-anything
      hledger-mode
      hydra
      ialign
      ibuffer-vc
      iedit
      itail
      iy-go-to-char
      js2-refactor
      js2-mode
      json-mode
      jsonnet-mode
      just-mode
      jq-mode
      kind-icon
      lua-mode
      log4j-mode
      ##########
      # LSP mode
      lsp-mode
      lsp-haskell
      lsp-metals
      lsp-treemacs
      lsp-ui
      dap-mode
      ##########
      markdown-mode
      markdown-preview-mode
      marginalia
      magit
      move-text
      multiple-cursors
      nerd-icons
      nix-mode
      orderless
      org-drill
      ormolu
      ox-jira
      ox-clip
      pcre2el
      plantuml-mode
      protobuf-mode
      quick-yes
      restclient
      rg
      s
      sbt-mode
      scala-mode
      scala-ts-mode
      string-inflection
      strace-mode
      persistent-scratch
      pdf-tools
      posframe
      rust-mode
      rainbow-delimiters
      smartparens
      systemd
      terraform-mode
      transpose-frame
      treemacs
      typescript-mode
      verb
      vertico
      visual-regexp
      web-mode
      with-editor
      yaml-mode
      yasnippet
      undo-fu
      undo-fu-session
      vundo
      csv-mode
      rainbow-mode
      myEmacsConfig
    ]
    ++ [ my_gptel ]
  )
)
