{
    description = "ucg a configuration language compiler";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs";
        flake-utils.url = "github:numtide/flake-utils";
        rust-overlay = {
            url = "github:oxalica/rust-overlay";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        naersk.url = "github:nix-community/naersk";
        flake-compat = {
            url = github:edolstra/flake-compat;
            flake = false;
        };
    };

    outputs = {self, nixpkgs, flake-utils, rust-overlay, naersk, flake-compat}:
    flake-utils.lib.eachDefaultSystem (system:
    let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
        rust-bin = pkgs.rust-bin.stable."1.64.0".default.override {
            extensions = [ "rust-src" ];
            # Add wasm32 as an extra target besides the native target.
            targets = [ "wasm32-unknown-unknown" ];
        };
        naersk-lib = pkgs.callPackage naersk {
            rustc = rust-bin;
            cargo = rust-bin;
        };
        ucg = with pkgs;
            naersk-lib.buildPackage rec {
                pname = "ucg";
                version = "0.7.3";
                src = ./.;
                cargoBuildOptions = opts: opts ++ ["-p" "${pname}" ];
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
    });
}