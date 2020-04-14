{ sources ? import ./nix/sources.nix,

pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.haskellPackages.dhall-json_1_4_0.override {
  dhall = pkgs.haskellPackages.dhall_1_25_0;
},

nix-prefetch-git ? pkgs.nix-prefetch-git

}:
pkgs.mkShell {
  buildInputs =

    [ dhall-json nix-prefetch-git ];
}
