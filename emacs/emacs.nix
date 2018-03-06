{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  myEmacsConfig = (pkgs.runCommand "create-my-emacs-config" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${./emacs-config.el} $out/share/emacs/site-lisp/default.el
  '');
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  quickYes = let
    src = pkgs.fetchurl {
      url = "https://download.tuxfamily.org/user42/quick-yes.el";
      sha256 = "0xs2y0hyw947g0m42fnyw0b4rxi85wmd5d5w0xwh9ic0qbq1mq8r";
    };
  in
    pkgs.runCommand "download-quick-yes" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${src} $out/share/emacs/site-lisp/
  '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    myEmacsConfig
    magit
    use-package
    projectile
    counsel
    company
    flycheck
    solarized-theme
    avy
    fasd
    quickYes
    undo-tree
    goto-chg
    yasnippet
    indent-guide
    git-link
  ]) ++ (with epkgs.elpaPackages; [
    # auctex
  ]) ++ [
    pkgs.mu
  ])
