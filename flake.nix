{
    description = "ucg a configuration language compiler";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs";
        flake-utils.url = "github:numtide/flake-utils";
        rust-overlay = {
            url = "github:oxalica/rust-overlay";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        crane.url = "github:ipetkov/crane";
        flake-compat = {
            url = "github:edolstra/flake-compat";
            flake = false;
        };
    };

    outputs = {nixpkgs, flake-utils, rust-overlay, crane, ...}:
    flake-utils.lib.eachDefaultSystem (system:
    let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        craneLib = (crane.mkLib pkgs);
        ucg = craneLib.buildPackage rec {
            pname = "ucg";
            version = "0.7.3";
            src = ./.;
        };
    in
    {
        packages = {
            inherit ucg;
        };
        defaultPackage = ucg;
        defaultApp = {
            type = "app";
            program = "${ucg}/bin/ucg";
        };
        devShells.default = craneLib.devShell {
          packages = with pkgs; [ gnumake rust-analyzer cargo-tarpaulin];
        };
    });
}
