{

sources ? import ./nix/sources.nix,

}:
let api = import ./nix/api.nix { inherit sources; };
in {
  spago2nix-ree-cli =
    import ./nix/pkgs/spago2nix-ree-cli.nix { inherit sources; };
  inherit (api) buildProject;
  inherit (api) buildProjectDependencies;
}
