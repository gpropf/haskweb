# Haskweb

### Haskell demos using GHCJS and the Reflex framework. Mostly exists so that I can learn about FRP and GHCJS.

## Setup the Environment

The only way I've been able to get this work so far is with the
`reflex-platform` package at
https://github.com/reflex-frp/reflex-platform. running on NixOS. I
haven't been able to make it work yet on Arch or Mint.

This package has been developed using the instructions found here: https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md To build it one should be able to just use the nix-build and nix-shell commands towards the end of that file.

You can build standalone apps too.

`$ nix-build -o backend-result -A ghc.backend`
`$ nix-build -o frontend-result -A ghcjs.frontend`

You'll find the executable at `./frontend-result/bin/frontend`.

## Debug and Develop

The best way to test new changes is

`nix-shell -A shells.ghc`

This gets you into a nix-shell with paths to things like ghci among other things. Now do

`cd frontend/src`

then `ghci` and withing the ghci REPL do `:load Main.hs`


