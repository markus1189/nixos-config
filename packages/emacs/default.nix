{ emacs, mutate, runCommand, fetchurl, emacsPackagesNgGen, mplayer, fasd, plantuml, pandoc }:

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
      sha256 = "17imjcw331wyachnwl1lcbpj85s5wziliy0fzcqncniywrzlvclk";
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
    command-log-mode
    company
    dash
    dired-plus
    evil-numbers
    expand-region
    f
    epkgs.melpaPackages.fasd
    find-temp-file
    flycheck
    flycheck-yamllint
    fullframe
    git-commit
    git-link
    goto-chg
    groovy-mode
    graphviz-dot-mode
    haskell-mode
    helm
    helm-flyspell
    helm-projectile
    helm-swoop
    hydra
    iedit
    indent-guide
    iy-go-to-char
    jq-mode
    liso-theme
    mvn
    magit
    move-text
    multiple-cursors
    nix-mode
    ox-jira
    plantuml-mode
    projectile
    quick-yes
    restclient
    s
    sbt-mode
    scala-mode
    persistent-scratch
    pdf-tools
    smartparens
    solarized-theme
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
  ]))
