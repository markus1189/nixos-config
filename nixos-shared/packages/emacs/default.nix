{ emacs, mutate, runCommand, fetchurl, emacsPackagesNgGen, mplayer, fasd, plantuml, pandoc, git }:

let
  mutatedEmacsConfig = mutate ./emacs-config.el {
    inherit mplayer fasd plantuml pandoc;
    yesSound = ./yes.wav;
    noSound = ./no.wav;
    popSound = ./pop.wav;
  };
  myEmacsConfig = (runCommand "create-my-emacs-config" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${mutatedEmacsConfig} $out/share/emacs/site-lisp/default.el
  '');
  emacsWithPackages = (emacsPackagesNgGen emacs).emacsWithPackages;
  quick-yes = let
    src = fetchurl {
      url = "https://download.tuxfamily.org/user42/quick-yes.el";
      sha256 = "0xs2y0hyw947g0m42fnyw0b4rxi85wmd5d5w0xwh9ic0qbq1mq8r";
    };
  in
    runCommand "install-quick-yes" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${src} $out/share/emacs/site-lisp/quick-yes.el
  '';
  dired-plus = let
    src = fetchurl {
      url = "https://www.emacswiki.org/emacs/download/dired+.el";
      sha256 = "sha256:15g6nbfkb0p4irgk3jjmbaayrvqp39jyhd2yg361hy4gjh9gl8ln";
    };
  in
    runCommand "install-dired-plus" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${src} $out/share/emacs/site-lisp/dired+.el
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
    dyalog-mode
    evil-numbers
    expand-region
    f
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
