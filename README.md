# Gadoc

Gadoc is a package docs generator for Haskell. Similar to Purescript (`purs docs`) and Rust (`cargo doc`) Gadoc generates a file `generated-docs/html/index.html` that can be opened with a Browser without starting a local server.

Gadoc is written in [Haskell](https://www.haskell.org/), [Purescript](https://www.purescript.org/) and [nix](https://nixos.org/).

Gadoc is in very early experimental development, a lot of things have not been test and might not work!

[![Video](https://raw.githubusercontent.com/smatting/gadoc/master/imgs/gadoc-demo.png)](https://xahv0eel.s3.eu-central-1.amazonaws.com/gadoc-demo.webm)

Gadoc is only suitable for browsing *local* package docs, because it needs to load multiple MBs of search index data to the browser.

## Install
Prerequisites:

- Install [nix](https://nixos.org/nix/)

To install the latest of Gadoc version, run:
```sh
nix-env -if https://github.com/smatting/gadoc/archive/master.tar.gz
```

## Usage

Run `gadoc` in your Haskell project directory. Gadoc will create a `generated-docs` directory and open the file `generated-docs/html/index.html` in your browser.
Running `gadoc` again (with no changes to packages) will just open the `index.html` again

If your project is built with stack, make sure you've compiled all your packages with their local haddocks: `stack build --haddock --only-dependencies`.

If your project is built with nix, run `gadoc` from within your nix shell.


##  Development

If you like the idea of Gadoc, please consider contributing! (Hi Zurihac!)

To develop the command line tool `gadoc` run `nix-shell` and build the project with `cabal`. `ghcid` is also available in the dev environment.
The cabal projects expects the frontend built assets in `html/`. See [project.nix](https://github.com/smatting/gadoc/blob/master/project.nix) for a tarball of a frontend built, or build the frontend yourself and copy the contents of `frontend/dist/` to `html`.

To develop the frontend in `frontend/` install [spago](https://github.com/purescript/spago) and [parcel](https://parceljs.org/).
Run `spago build -w` and in a separate terminal run `parcel serve --no-source-maps index.html`.
The frontend also expects the index files that the `gadoc` tool crates (`docstate.js`, `targets.js`, `modules.js`) in the `dist/` directory, so copy them there. 

## Gadidae

The name Gadoc derives from fish family [Gadidae](https://en.wikipedia.org/wiki/Gadidae). Gadidae include species such haddocks....
