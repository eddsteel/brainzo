{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs.haskell.lib) doJailbreak;
  hpkgs = nixpkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      brainzoH = hself.callPackage ./brainzo.nix {};
      happstack-lite = doJailbreak hsuper.happstack-lite;
      happstack-server = doJailbreak hsuper.happstack-server;
    };
  };
  brainzo = hpkgs.brainzoH.overrideAttrs (attrs: {
    propagatedBuildInputs = attrs.propagatedBuildInputs ++ (with nixpkgs; [
      xdg-utils xdotool mplayer pulseaudio pavucontrol playerctl consul psmisc]);
    postConfigure = ''
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"consul"' '"${nixpkgs.consul}/bin/consul"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"killall"' '"${nixpkgs.psmisc}/bin/killall"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"mplayer"' '"${nixpkgs.mplayer}/bin/mplayer"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"pactl"' '"${nixpkgs.pulseaudio}/bin/pactl"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"pavucontrol"' '"${nixpkgs.pavucontrol}/bin/pavucontrol"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"playerctl"' '"${nixpkgs.playerctl}/bin/playerctl"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"xdotool"' '"${nixpkgs.xdotool}/bin/xdotool"'
      substituteInPlace src/Brainzo/Paths.hs --replace \
        '"xdg-open"' '"${nixpkgs.xdg-utils}/bin/xdg-open"'
      '';
  });
in brainzo
