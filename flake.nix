{
  inputs.pkgs.url = "git+ssh://git@github.com/seereason/sr-flake?dir=sr-nixpkgs&ref=main";
  inputs.libs.url = "git+ssh://git@github.com/seereason/sr-flake?dir=sr-libs&ref=main";

  outputs = { self, pkgs, libs }: {
    devShells = libs.devShells;
  };
}
