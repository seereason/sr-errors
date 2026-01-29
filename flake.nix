{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    userid.url = "github:seereason/userid";
  };
  outputs = { self, nixpkgs, userid }:
    let
      system = "x86_64-linux";
      overlay = final: prev: {
        sr-errors = final.callCabal2nix "sr-errors" ./. { };
        userid = userid;
      };
      pkgs = nixpkgs.legacyPackages.${system};
      version="ghc9122";
    in with pkgs;
      let ghcPackages = haskell.packages.${version};
          ghcjsPackages = pkgsCross.ghcjs.haskell.packages.${version};
          myGHCPackages = ghcPackages.extend overlay;
          myGHCJSPackages = ghcjsPackages.extend overlay;
      in
        {
          packages.${system}.default = myGHCPackages.sr-errors;
          devShells.${system}.default = with pkgs;
            mkShell
              {
                packages = [ (ghcPackages.ghcWithPackages (p: (with p; [mtl happstack-server])))
                             cabal-install cabal2nix
                             (ghcjsPackages.ghcWithPackages (p: with p; [ghcjs-dom mtl]))
                           ];
                shellHook = ''
            alias ghcjs=javascript-unknown-ghcjs-ghc
            alias ghcjs-pkg=javascript-unknown-ghcjs-ghc-pkg
        '';
                
              };

        };
}
