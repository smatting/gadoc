let
  packageName = "gadoc";
  compiler = "ghc865";
  src = import ./nix/sources.nix;
  pkgs = import src.nixpkgs {};
  prebuiltFrontend = pkgs.fetchurl {
    url = "https://xahv0eel.s3.eu-central-1.amazonaws.com/8ab653e.tar.gz";
    sha256 = "1am5id5lq6x8ypyz4a1s9h3k15bqid349999x8ssj4hg9cczyffh";
  };

  source = pkgs.lib.sourceByRegex ./. [
    "^.*\\.md$"
    "^app.*$"
    "^data.*$"
    "^.*\\.cabal$"
    "^src.*$"
    "^tests.*$"
    "LICENSE"
    # "^html.*$"
    ];

  publishedAssets = pkgs.stdenv.mkDerivation {
    name = "gadoc-frontend-assets";
    dontInstall = true;
    src = prebuiltFrontend;
    buildPhase = ''
      mkdir -p $out
      cp * $out
    '';
  };

  localAssets = pkgs.lib.cleanSource ./frontend/dist;

  sourceWithLocalAssets = sourceWithAssets localAssets.outPath;

  sourceWithAssets = assets: pkgs.stdenv.mkDerivation {
    name = "gadoc-source-with-asset";
    src = source;
    dontInstall = true;
    buildPhase = ''
      mkdir -p $out
      mkdir -p $out/html
      cp -r $src/* $out/
      cp -r ${assets}/* $out/html/
    '';
  };

  sourceWithPublishedAssets = sourceWithAssets publishedAssets;

  overlay = self: super: {
    "${packageName}" = super.callCabal2nix "${packageName}" sourceWithLocalAssets { };
    "${packageName}-dev" = super.callCabal2nix "${packageName}" source { };
    "${packageName}-pinned-frontend" = super.callCabal2nix "${packageName}" sourceWithPublishedAssets { };

    "hoogle" = super.hoogle.overrideAttrs (oldAttrs: {patchPhase = ''
      sed -e 's/Action.CmdLine//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Action.CmdLine/' -i ./hoogle.cabal

      sed -e 's/Action.Generate//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Action.Generate/' -i ./hoogle.cabal

      sed -e 's/Action.Search//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Action.Search/' -i ./hoogle.cabal

      sed -e 's/Action.Server//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Action.Server/' -i ./hoogle.cabal

      sed -e 's/Action.Test//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Action.Test/' -i ./hoogle.cabal

      sed -e 's/Input.Cabal//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Input.Cabal/' -i ./hoogle.cabal

      sed -e 's/Input.Download//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Input.Download/' -i ./hoogle.cabal

      sed -e 's/Input.Haddock//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Input.Haddock/' -i ./hoogle.cabal

      sed -e 's/Input.Item//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Input.Item/' -i ./hoogle.cabal

      sed -e 's/Input.Reorder//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Input.Reorder/' -i ./hoogle.cabal

      sed -e 's/Input.Settings//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Input.Settings/' -i ./hoogle.cabal

      # sed -e 's/Input.Set//' -i ./hoogle.cabal
      # sed -e 's/exposed-modules:/exposed-modules:\n        Input.Set/' -i ./hoogle.cabal

      sed -e 's/Output.Items//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Output.Items/' -i ./hoogle.cabal

      sed -e 's/Output.Names//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Output.Names/' -i ./hoogle.cabal

      sed -e 's/Output.Tags//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Output.Tags/' -i ./hoogle.cabal

      sed -e 's/Output.Types//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Output.Types/' -i ./hoogle.cabal

      sed -e 's/Query//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        Query/' -i ./hoogle.cabal

      sed -e 's/General.Conduit//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Conduit/' -i ./hoogle.cabal

      sed -e 's/General.IString//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.IString/' -i ./hoogle.cabal

      sed -e 's/General.Log//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Log/' -i ./hoogle.cabal

      sed -e 's/General.Store//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Store/' -i ./hoogle.cabal

      sed -e 's/General.Str//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Str/' -i ./hoogle.cabal

      sed -e 's/General.Template//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Template/' -i ./hoogle.cabal

      sed -e 's/General.Timing//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Timing/' -i ./hoogle.cabal

      sed -e 's/General.Util//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Util/' -i ./hoogle.cabal

      sed -e 's/General.Web//' -i ./hoogle.cabal
      sed -e 's/exposed-modules:/exposed-modules:\n        General.Web/' -i ./hoogle.cabal

      sed -e 's/module Query(/module Query(lexer,/' -i ./src/Query.hs
    '';} );
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = overlay;
  };

in {
  dev = haskellPackages."${packageName}-dev";
  build = haskellPackages."${packageName}-local";
  build-pinned-frontend = haskellPackages."${packageName}-pinned-frontend";

  shell = haskellPackages.shellFor {
    packages = ps: [ ps."${packageName}-dev" ];
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  };

  inherit sourceWithLocalAssets localAssets publishedAssets;
}
