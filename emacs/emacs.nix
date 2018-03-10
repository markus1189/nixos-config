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
    cp ${src} $out/share/emacs/site-lisp/default.el
  '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    myEmacsConfig
    magit
    use-package
    projectile
    company
    flycheck
    solarized-theme
    avy
    fasd
    quick-yes
    undo-tree
    goto-chg
    yasnippet
    indent-guide
    git-link
    helm
    helm-projectile
    wgrep-helm
    nix-mode
    liso-theme
    expand-region
    scala-mode
    sbt-mode
    smartparens
    move-text
    find-temp-file
    f
    s
    haskell-mode
    ox-jira
  ]) ++ (with epkgs.elpaPackages; [
    # auctex
  ]) ++ [
    pkgs.mu
  ])
