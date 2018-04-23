# Haskweb

### Haskell demos using GHCJS and the Reflex framework. Mostly exists so that I can learn about FRP and GHCJS.

## Setup the Environment

The only way I've been able to get this work so far is with the
`reflex-platform` package at
https://github.com/reflex-frp/reflex-platform. running on NixOS. I
haven't been able to make it work yet on Arch or Mint. I've tested
only using this commit in the "develop" branch. I don't know if later
or earlier commits work:

```
commit 2e2fb7f0d15385a712face0c9cdcece4f3981a90
Author: Ryan Trinkle <ryan.trinkle@gmail.com>
Date:   Fri Feb 16 01:29:01 2018 -0500
```
   
So the steps to build this package are

1. `git clone https://github.com/reflex-frp/reflex-platform`
2. `cd reflex-platform`
3. `./try-reflex`: This step puts you in a nix-shell environment with
   (hopefully) all the paths you need to make ghcjs work. Without this
   step ghcjs is not in the $PATH. The `try-reflex` command sets up a
   *large* number of paths.
4. Finally cd into this package directory and just do `ghcjs --make reflexamples.hs`
5. Aim the browser at file:///home/greg/Reflexamples/reflexamples.jsexe/index.html

## Pushing changes

There's some weirdness right now involving this VM and pushing to
github. The way to push right now is `git push
https://gpropf:mygithubpasswd@github.com/gpropf/haskweb.git`. This
should push changes without asking for username and password or
popping up that KWallet dialog.
