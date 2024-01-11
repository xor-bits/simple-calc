{
  description = "devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        # `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            pkg-config
            openssl
            (rust-bin.nightly.latest.default.override {
              extensions = [ "rust-src" ];
              targets = [ ];
            })
            rust-analyzer
            rustup
            lldb
            cargo-udeps
            cargo-nextest
            cargo-expand
            cargo-make
            act
            lld_16
            clang_16
            bacon
          ];
        };
      }
    );
}
