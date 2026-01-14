{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        sr-errors = final.callCabal2nix "sr-errors" ./. { };
      };
      myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in
    {
      packages.${system}.default = myHaskellPackages.sr-errors;
    };
}
