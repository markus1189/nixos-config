with import <nixpkgs> { };

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [ xmonad xmonad-contrib ]))
  ];
}
