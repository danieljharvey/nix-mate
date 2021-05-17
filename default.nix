let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./foo.nix { }
