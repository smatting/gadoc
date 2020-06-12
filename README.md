# gadoc

Gadoc is package docs browser for Haskell. [![Video](https://raw.githubusercontent.com/smatting/gadoc/master/imgs/gadoc-demo.png)](https://xahv0eel.s3.eu-central-1.amazonaws.com/gadoc-demo.webm)

Some of it's features bare:
  - The browser can be opened with `file://` urls. There is no need to start a local web server.
  - gadoc is suitable for browsing _local_ package docs, because it needs to load multiple MB of index data to your browser

## Install
### Prerequisites
- [nix](https://nixos.org/nix/) `curl -L https://nixos.org/nix/install | sh`

To install the latest version, run:
```sh
nix-env -if https://github.com/smatting/gadoc/archive/master.tar.gz
```
