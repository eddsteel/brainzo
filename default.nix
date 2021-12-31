{ nixpkgs ? import <nixpkgs> {} }:

let
  hpkgs = nixpkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      brainzo = hself.callCabal2nix "brainzo" ./. {};
      happstack-lite = nixpkgs.haskell.lib.doJailbreak hsuper.happstack-lite;
      happstack-server = nixpkgs.haskell.lib.doJailbreak hsuper.happstack-server;
    };
  };
in hpkgs.brainzo
