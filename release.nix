let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        friday-devil = self.haskell.lib.doJailbreak hsuper.friday-devil;
        clay = self.haskell.lib.doJailbreak hsuper.clay;
        tomland = self.haskell.lib.dontCheck hsuper.tomland;

        lambdamap = hself.callPackage ./default.nix {};
      };
    };
  };

  pkgs = import <nixpkgs> { overlays = [ overlay ]; };

in {
  lambdamap = pkgs.haskellPackages.lambdamap;
}
