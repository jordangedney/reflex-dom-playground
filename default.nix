{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  overrides = self: super: let
    reflexDomCanvas = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-dom-canvas";
      rev = "aa1cf4c83dbcab1e59e5b607abf13497e912cd81";
      sha256 = "1r4fdj02qljy4b8pv05law4jkrgwdpi6l36zmm4951410hkxpppa";
    };
  in
  {
    reflex-dom-canvas = self.callCabal2nix "reflex-dom-canvas" reflexDomCanvas {};
  };

  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
