# spago2nix-ree

[![Build Status](https://travis-ci.com/thought2/spago2nix-ree.svg?branch=master)](https://travis-ci.com/thought2/spago2nix-ree)

`spago2nix-ree`

- re-uses already built purescript dependencies.
- so you don't have to re-compile them for every change in your project.

## Usage

- Create spago project

  ```
  spago init
  ```

- Add a `default.nix` like:

  ```nix
  { pkgs ? import <nixpkgs> { } }:
  let
    spago2nix-ree = import
      (builtins.fetchGit { url = "https://github.com/thought2/spago2nix-ree"; })
      { };
  in pkgs.stdenv.mkDerivation {

    name = "my-project";

    buildCommand = ''
      ln -s ${spago2nix-ree.buildProject { src = ./.; }} $out
    '';

    buildInputs = [ spago2nix-ree.spago2nix-ree ];
  }
  ```

- Build it with nix

  ```
  nix-shell

  spago2nix-ree

  # creates spago-lock.json

  nix-build
  ```

You can also use `buildProjectDependencies` to just build the dependencies instead.

To see the main feature, edit one of your project source files and run `nix-build` again. Only your local modules get recompiled.

## Prior Work

- [justinwoo/spago2nix](https://github.com/justinwoo/spago2nix)

  Aweseome tool that I used in many projects. However, for my usecase CI builds were not fast enough.
