let spago2nix-ree = import ../default.nix { };
in spago2nix-ree.buildProjectDependencies { src = ./.; }
