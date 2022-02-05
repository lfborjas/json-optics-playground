let
  pkgs = import ./packages.nix {};
in
  { json-optics-practice = pkgs.haskellPackages.json-optics-practice; }
