{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  # mutatedEmacsConfig = pkgs.mutate ./emacs-config.el { inherit (pkgs) fasd; };
  myEmacsConfig = (pkgs.runCommand "create-my-emacs-config" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${./emacs-config.el} $out/share/emacs/site-lisp/default.el
  '');
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  quick-yes = let
    src = pkgs.fetchurl {
      url = "https://download.tuxfamily.org/user42/quick-yes.el";
      sha256 = "0xs2y0hyw947g0m42fnyw0b4rxi85wmd5d5w0xwh9ic0qbq1mq8r";
    };
  in
    pkgs.runCommand "install-quick-yes" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${src} $out/share/emacs/site-lisp/quick-yes.el
  '';
  dired-plus = let
    src = pkgs.fetchurl {
      url = "https://www.emacswiki.org/emacs/download/dired+.el";
      sha256 = "1kj6f081f60sbgvy903hfhf2y4bb0g48xk5wl5m8yaibmi5vnyzi";
    };
  in
    pkgs.runCommand "install-dired-plus" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${src} $out/share/emacs/site-lisp/dired+.el
    '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    avy
    company
    dash
    dired-plus
    evil-numbers
    expand-region
    f
    fasd
    find-temp-file
    flycheck
    fullframe
    git-commit
    git-link
    goto-chg
    haskell-mode
    helm
    helm-flyspell
    helm-projectile
    helm-swoop
    iedit
    indent-guide
    iy-go-to-char
    liso-theme
    mvn
    magit
    move-text
    multiple-cursors
    nix-mode
    ox-jira
    projectile
    quick-yes
    s
    sbt-mode
    scala-mode
    persistent-scratch
    smartparens
    solarized-theme
    undo-tree
    use-package
    wgrep-helm
    yasnippet
    myEmacsConfig
  ]) ++ (with epkgs.elpaPackages; [
    # auctex
  ]) ++ [
    pkgs.mu
  ])
