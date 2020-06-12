# Gadoc

Gadoc is package docs browser for Haskell. It's written in [Haskell](https://www.haskell.org/), [Purescript](https://www.purescript.org/) and [nix](https://nixos.org/).

It's in very early experimental development, a lot of things don't work yet, for example Haskell projects created with [stack](https://docs.haskellstack.org/en/stable/README/).

[![Video](https://raw.githubusercontent.com/smatting/gadoc/master/imgs/gadoc-demo.png)](https://xahv0eel.s3.eu-central-1.amazonaws.com/gadoc-demo.webm)

Some of Gadoc's features are:
  - Gadoc works with `file://` urls. There is no need to start a local web server.
  - Gadoc is suitable for browsing _local_ package docs, because it needs to load multiple MB of index data to your browser

## Install
Prerequisites:
-
- Install [nix](https://nixos.org/nix/)

To install the latest of Gadoc version, run:
```sh
nix-env -if https://github.com/smatting/gadoc/archive/master.tar.gz
```

## Usage

NOTE: I've only tested Gadoc with Haskell environments which are set up by nix!  Projects build with Stack don't work (yet)!

1. Enter the development environment of your project, e.g. `nix-shell`
2. Run `gadoc`. Gadoc will create a `generated-docs` directory and open the file `generated-docs/html/index.html` in your browser.
   Running `gadoc` again (with no changes to packages) will just open the `index.html` again.



##  Development

To develop the command line tool run `nix-shell` and build the project with `cabal`. `ghcid` is also available in the dev environment.
The cabal projects expects the frontend built assets in `html/`. See [project.nix](https://github.com/smatting/gadoc/blob/master/project.nix) for a tarball of a frontend built, or build the frontend yourself and copy the contents of `frontend/dist/` to `html`.

To develop the frontend install [spago](https://github.com/purescript/spago) and [parcel](https://parceljs.org/).
Run `spago build -w` and in a separate terminal run `parcel serve --no-source-maps index.html`.
The frontend also expects the index files that the `gadoc` tool crates (`docstate.js`, `targets.js`, `modules.js`) in the `dist/` directory, so copy them there. 

## Gadidae

The name Gadoc derives from fish family [Gadidae](https://en.wikipedia.org/wiki/Gadidae). Gadidae include species such as cods and haddocks....
