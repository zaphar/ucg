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
            url = "github:edolstra/flake-compat";
            flake = false;
        };
    };

    outputs = {nixpkgs, flake-utils, rust-overlay, naersk, ...}:
    flake-utils.lib.eachDefaultSystem (system:
    let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
        rust-bin = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "rust-src" ];
        };
        naersk-lib = pkgs.callPackage naersk {
            rustc = rust-bin;
            cargo = rust-bin;
        };
        ucg = naersk-lib.buildPackage rec {
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
        devShells.default = pkgs.mkShell {
          buildInputs = [ rust-bin pkgs.rust-analyzer pkgs.cargo-tarpaulin ];
          packages = with pkgs; [ gnumake ];
        };
    });
}
