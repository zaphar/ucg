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
        rust-bin = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "rust-src" ];
        };
        craneLib = (crane.mkLib pkgs).overrideToolchain rust-bin;
        ucg = craneLib.buildPackage {
            pname = "ucg";
            version = "0.8.2";
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
          packages = with pkgs; [ gnumake rust-analyzer cargo-tarpaulin zola jq];
        };
    });
}
