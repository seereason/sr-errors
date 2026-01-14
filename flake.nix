{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    userid.url = "github:seereason/userid";
  };
  outputs = { self, nixpkgs, userid }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        sr-errors = final.callCabal2nix "sr-errors" ./. { };
        # userid = userid;
      };
      myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in
    {
      packages.${system}.default = myHaskellPackages.sr-errors;
    };
}
