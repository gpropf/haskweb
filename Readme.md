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

To rebuild the JavaScript from the Haskell source do:

`nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"`

## Commit Changes

`git push https://gpropf:password@github.com/gpropf/haskweb.git` -- password is not actual password of course ;)

# Tutorial Abstract
_This is a brief abstract for what might be a tutorial on how to use
this Reflex-Dom Widgets_

## Tips
* Make lots of smaller functions to create components rather than
  trying to have large blocks of `el` tags run together. This seems to
  lead to a lot of type errors if you fail to end with something of
  type `m ()` for instance.
  
* All your component functions should have the type description
  `MonadWidget t m => m ()` or you will get that confusing error
  message such as the below:

```
Ambiguous type variables ‘t0’, ‘m0’ arising from a use of ‘tabDisplay’
      prevents the constraint ‘(DomBuilder t0 m0)’ from being solved.
      Relevant bindings include
        tabA :: m0 () (bound at Main.hs:39:7)
        tabDemo :: m0 () (bound at Main.hs:38:1)
```

