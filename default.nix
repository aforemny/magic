rec {

  pkgs = import <nixpkgs> {};

  ghc = pkgs.haskell.packages.ghc784.ghcWithPackages ( pkgs: with pkgs;
    [ elm-reactor elm-compiler elm-make elm-package ]
  );

  env = pkgs.buildEnv {
    name = "magic";
    paths = [ ghc ];
  };

  magic = pkgs.androidenv.buildApp {
    name = "magic";
    src = ./android;
    platformVersions = [ "22" ];
    useGoogleAPIs = false;
    release = false;
    #keyStore = ;
    #keyAlias = "";
    #keyStorePassword = "";
    #keyAliasPassword = "";
  };

  emulate-magic = pkgs.androidenv.emulateApp {
    name = "magic";
    app = magic;
    platformVersion = "22";
    useGoogleAPIs = false;
    package = "org.nomath.magic";
    activity = "MainActivity";
    enableGPU = true;
  };

}
