# Haskweb

### Haskell demos using GHCJS and the Reflex framework. Mostly exists so that I can learn about FRP and GHCJS.

## Setup the Environment

The only way I've been able to get this work so far is with the
`reflex-platform` package at
https://github.com/reflex-frp/reflex-platform. running on NixOS. I
haven't been able to make it work yet on Arch or Mint.

This package has been developed using the instructions found here: https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md To build it one should be able to just use the nix-build and nix-shell commands towards the end of that file.


## Pushing changes

There's some weirdness right now involving this VM and pushing to
github under NixOS. The way to push right now is `git push
https://gpropf:mygithubpasswd@github.com/gpropf/haskweb.git`. This
should push changes without asking for username and password or
popping up that KWallet dialog.
