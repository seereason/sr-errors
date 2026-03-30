{
  description = "sr-errors";

  inputs = {
    sr-pkgs.url   = "git+ssh://git@github.com/seereason/sr-flake?dir=sr-nixpkgs&ref=main";
    sr-libs.url   = "git+ssh://git@github.com/seereason/sr-flake?dir=sr-libs&ref=main";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, sr-pkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs  = import sr-pkgs.inputs.nixpkgs { inherit system; };
        hpkgs = pkgs.haskellPackages.extend (sr-libs.srOverlay pkgs);

        # ── GHC options ────────────────────────────────────────────────
        # Add / remove flags here. They are threaded through via
        # `configureFlags` so they apply to every cabal invocation.
        ghcOptions = [
          "-Wall"
          "-Wunused-imports"
          "-ddump-minimal-imports"  # uncomment to inspect Core
          # "-O2"          # uncomment for benchmarks
        ];

        # ── Package definition ─────────────────────────────────────────
        sr-errors = hpkgs.callCabal2nix "sr-errors" ./. {
          # extra Haskell deps not in the .cabal file go here, e.g.:
          # some-dep = hpkgs.some-dep;
        };

        # Apply GHC options via `overrideCabal`
        sr-errors-dev = pkgs.haskell.lib.overrideCabal sr-errors (old: {
          configureFlags = (old.configureFlags or [])
            ++ map (o: "--ghc-option=${o}") ghcOptions;
        });

      in {
        # `nix build` → release build (no extra flags)
        packages.default = sr-errors;

        # `nix build .#dev` → build with the GHC options above
        packages.dev = sr-errors-dev;

        # `nix develop` → drop into a shell with cabal + GHC available
        devShells.default = hpkgs.shellFor {
          packages = _: [ sr-errors ];
          buildInputs = with pkgs; [
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.hlint
          ];
          # GHC options visible to cabal inside the shell
          shellHook = ''
            export CABAL_CONFIG=/dev/null   # avoid $HOME pollution
            echo "GHC: $(ghc --version)"
          '';
        };
      });
}
