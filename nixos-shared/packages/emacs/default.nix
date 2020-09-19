{ emacs, mutate, runCommandLocal, fetchurl, emacsPackagesNgGen, mplayer, fasd, plantuml, pandoc, git, ndtSources }:

let
  mutatedEmacsConfig = mutate ./emacs-config.el {
    inherit mplayer fasd plantuml pandoc;
    yesSound = ./yes.wav;
    noSound = ./no.wav;
    popSound = ./pop.wav;
  };
  myEmacsConfig = (runCommandLocal "create-my-emacs-config" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${mutatedEmacsConfig} $out/share/emacs/site-lisp/default.el
  '');
  emacsWithPackages = (emacsPackagesNgGen emacs).emacsWithPackages;
  quick-yes = runCommandLocal "install-quick-yes" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${ndtSources.emacs-quick-yes} $out/share/emacs/site-lisp/quick-yes.el
  '';
  dired-plus = runCommandLocal "install-dired-plus" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${ndtSources.emacs-dired-plus} $out/share/emacs/site-lisp/dired+.el
    '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    avy
    beacon
    company
    dash
    deadgrep
    dired-plus
    dired-filter
    direnv
    dockerfile-mode
    docker
    dumb-jump
    dyalog-mode
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
    git-commit
    git-link
    gitlab-ci-mode
    gitlab-ci-mode-flycheck
    go-mode
    go-complete
    go-autocomplete
    goto-chg
    groovy-mode
    graphviz-dot-mode
    haskell-mode
    helm
    helm-flyspell
    helm-projectile
    helm-swoop
    hl-anything
    hydra
    ibuffer-vc
    ibuffer-projectile
    iedit
    indent-guide
    itail
    iy-go-to-char
    json-mode
    jsonnet-mode
    jq-mode
    (kubernetes.overrideAttrs (old: { buildInputs = old.buildInputs ++ [ git ]; }))
    # kubel
    # lua-mode
    log4j-mode
    liso-theme
    markdown-mode
    markdown-preview-mode
    mvn
    magit
    move-text
    multiple-cursors
    nix-mode
    ormolu
    ox-jira
    plantuml-mode
    projectile
    protobuf-mode
    quick-yes
    restclient
    s
    sbt-mode
    scala-mode
    string-inflection
    strace-mode
    persistent-scratch
    pdf-tools
    smartparens
    solarized-theme
    terraform-mode
    typescript-mode
    use-package
    wgrep-helm
    which-key
    yaml-mode
    yasnippet

    myEmacsConfig
  ]) ++ (with epkgs.elpaPackages; [
    auctex
    pabbrev
    undo-tree
    csv-mode
    rainbow-mode
  ]))
