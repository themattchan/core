let pkgs = import <nixpkgs>{};
in
  { core = pkgs.haskellPackages.callPackage ./project1.nix {};
  }