{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
, androidIsRelease ? false
}:
with obelisk;
project ./. ({ pkgs, ... }: {
  shellToolOverrides = self: super: {
    haskell-language-server = pkgs.haskell.packages.ghc8107.haskell-language-server;
    implicit-hie = pkgs.haskell.packages.ghc8107.implicit-hie;
    hlint = pkgs.haskell.packages.ghc8107.hlint;
  };

  android = {
    applicationId = "org.yokop.hen";
    displayName = "HEN";
    isRelease = androidIsRelease;
    resources = reflex-platform.android.buildIcons {
      src = ./assets/chara.png;
    };
    version = {
      code = "1";
      name = "0.2";
    };
  };
  ios.bundleIdentifier = "org.yokop.hen";
  ios.bundleName = "HEN";
})
