let
  importOrElse = maybePath: otherwise:
    if builtins.pathExists maybePath then import maybePath else otherwise;
  pkgs = importOrElse ./nixpkgs.nix
    ( let
        haskellNix = import (
          builtins.fetchTarball
            https://github.com/lspitzner/haskell.nix/archive/4ad436d66d1a553d1a36d89fcab9329f10ae36e9.tar.gz
        ) { version = 2; };
        nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
      in
      import nixpkgsSrc haskellNix.nixpkgsArgs
    );
  gitignoreSrc = pkgs.fetchFromGitHub {
    # owner = "hercules-ci";
    owner = "lspitzner"; # TODO switch back to the above once PR is merged
                         # see https://github.com/hercules-ci/gitignore.nix/pull/44
    repo = "gitignore.nix";
    rev = "97d53665298d2b31b79e5fe4b60edb12a6661547";
    sha256 = "sha256:1b3z2ikpg32zsfrhv4fb17dqavgg7d4wahslxlm37w68y7adsdav";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource gitignoreFilter;
  cleanedSource = pkgs.lib.cleanSourceWith {
    name = "butcher";
    src = ./..;
    filter = p: t:
      let baseName = baseNameOf (toString p);
      in gitignoreFilter ./.. p t
      && baseName != ".gitignore"
      && baseName != "nix"
      && baseName != "shell.nix"
      && baseName != "default.nix";
  };
  localExtraDeps = importOrElse ./local-extra-deps.nix (_: []) {inherit pkgs;};
  args = {
    inherit pkgs;
    inherit cleanedSource;
    pkg-def-extras = localExtraDeps;
  };
  inherit (builtins) hasAttr;
in
assert pkgs.lib.assertMsg (hasAttr "haskell-nix" pkgs) "need iohk haskell-nix overlay!";
let
  versions = {
    # "stack-8.0" = import ./via-stack.nix (args // { resolver = "lts-9.21"; });
    # "stack-8.2" = import ./via-stack.nix (args // { resolver = "lts-11.22"; });
    "stackage-8.4" = import ./via-stackage.nix (args // {
      # resolver = "lts-12.26";
      stackFile = "stack-8.4.yaml";
    });
    "stackage-8.6" = import ./via-stackage.nix (args // {
      # resolver = "lts-14.27";
      stackFile = "stack-8.6.yaml";
    });
    "stackage-8.8" = import ./via-stackage.nix (args // {
      # resolver = "lts-15.12";
      stackFile = "stack-8.8.yaml";
    });
    "hackage-8.4" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc844";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "0s6rfanb6zxhr5zbinp7h25ahwasciwj3ambsr6zdxm1l782b3ap";
      # materialized = ./materialized/hackage-8.4;
      configureArgs = "--allow-newer multistate:*";
    });
    "hackage-8.6" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc865";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "01m95xirrh00dvdxrpsx8flhcwlwcvgr3diwlnkw7lj5f3i7rfrl";
      # materialized = ./materialized/hackage-8.6;
      configureArgs = "--allow-newer multistate:*";
    });
    "hackage-8.8" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc883";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "14qs7ynlf7p2qvdk8sf498y87ss5vab3ylnbpc8sacqbpv2hv4pf";
      # materialized = ./materialized/hackage-8.8;
      configureArgs = "--allow-newer multistate:*";
    });
  } // (if hasAttr "ghc8101" pkgs.haskell-nix.compiler
    then {
    "hackage-8.10" = import ./via-hackage.nix (args // { 
      ghc-ver = "ghc8101";
      index-state = "2020-06-06T00:00:00Z";
      # index-sha256 = "1h1x65840jl6w2qvyq9csc7b3ivadr933glarnmydk2b23vw2i77";
      # plan-sha256 = "1s8a6cb5qgf4ky5s750rzx6aa52slp1skazh8kbx0dbfjd6df7yw";
      # materialized = ./materialized/hackage-8.10;
      configureArgs = "--allow-newer multistate:* --constraint 'splitmix<0.1'";
    });
    } else builtins.trace "warn: ghc 8.10 is not avaiable, will not be tested!" {}
  );
  linkFarmFromDrvs = name: drvs:
    let mkEntryFromDrv = drv: { name = drv.name; path = drv; };
    in pkgs.linkFarm name (map mkEntryFromDrv drvs);
in
versions // {
  inherit cleanedSource;
  default = versions."stackage-8.8";
}