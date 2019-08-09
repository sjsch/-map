let
  overlay = self: super: {
    haskellPackages =
      let
        markUnbroken = drv: self.haskell.lib.overrideCabal drv (drv: { broken = false; });
      in
        super.haskellPackages.override {
          overrides = hself: hsuper: with self.haskell.lib; {
            friday-devil = markUnbroken (doJailbreak hsuper.friday-devil);
            clay = markUnbroken (doJailbreak hsuper.clay);
            tomland = markUnbroken (dontCheck hsuper.tomland);

            lambdamap = hself.callPackage ./default.nix {};
          };
        };
  };

  pkgs = import <nixpkgs> { overlays = [ overlay ]; };

in {
  lambdamap = pkgs.haskellPackages.lambdamap;
}
