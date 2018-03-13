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
    pkgs.runCommand "download-quick-yes" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${src} $out/share/emacs/site-lisp/quick-yes.el
  '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    avy
    company
    dash
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
