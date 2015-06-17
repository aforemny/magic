rec {

  pkgs = import <nixpkgs> {};

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
