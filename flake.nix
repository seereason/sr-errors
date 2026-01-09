{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = {nixpkgs} @inputs: {
    packages = { };
  };
}
