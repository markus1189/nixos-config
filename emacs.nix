{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    # "dired+"
    # ace-jump-mode
    # avy
    # clojure-mode
    # company
    # dash
    # dash-functional
    # diminish
    # dired-filter
    # dired-hacks-utils
    # dired-imenu
    # dired-subtree
    # dockerfile-mode
    # elpy
    # emms
    # ensime
    # ess
    # evil
    # evil-numbers
    # expand-region
    # f
    # fasd
    # flycheck
    # fullframe
    # gh
    # ghc
    # gist
    # git-commit
    # groovy-mode
    # guide-key
    # haskell-mode
    # helm
    # helm-dash
    # helm-flyspell
    # helm-hayoo
    # helm-nixos-options
    # helm-proc
    # helm-projectile
    # htmlize
    # ibuffer-vc
    # iedit
    # ivy
    # iy-go-to-char
    # json-mode
    # json-reformat
    # magit-popup
    # magit-svn
    magit
    # markdown-mode
    # mc-extras
    # move-text
    # multiple-cursors
    # nix-mode
    # nixos-options
    # org-pdfview
    # paredit
    # pdf-tools
    # projectile
    # restclient
    # s
    # sbt-mode
    # scala-mode
    # shm
    # smartparens
    # string-inflection
    # tablist
    # magit
    # undo-tree
    # visible-mark
    # visual-regexp
    # w3m
    # wgrep
    # wgrep-helm
    # whitespace-cleanup-mode
    # with-editor
    # yaml-mode
    # yasnippet
  ]) ++ (with epkgs.elpaPackages; [
    # auctex
  ]) ++ [
    pkgs.mu
  ])
