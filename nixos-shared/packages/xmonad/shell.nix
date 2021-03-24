with import <nixpkgs> { };

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithHoogle (ps: with ps; [ xmonad xmonad-contrib haskell-language-server ]))
  ];
}
