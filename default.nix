{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  overrides = self: super: let
    reflex-dom-canvas = import ./nix/reflex-dom-canvas.nix;
  in
  {
    reflex-dom-canvas = self.callCabal2nix "reflex-dom-canvas" reflex-dom-canvas {};
  };

  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
