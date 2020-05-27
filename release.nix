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

  pkgs = import
    (fetchTarball https:///github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz)
    { overlays = [ overlay ]; };

  config = pkgs.writeText "lambdamap-config.toml" ''
    [server]
      port = 3000
      root = "/"

    [storage]
      database = "/data/lambdamap.sqlite"
      tiles = "/data/tiles"
  '';

in

pkgs.dockerTools.buildLayeredImage {
  name = "lambdamap";

  config = {
    Cmd = [ "${pkgs.haskellPackages.lambdamap}/bin/lambdamap-web" "--config" config ];
    ExposedPorts = {
      "3000/tcp" = {};
    };
    WorkingDir = "/data";
    Volumes = {
      "/data" = {};
    };
  };
}
