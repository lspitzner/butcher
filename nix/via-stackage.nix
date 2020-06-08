{ pkgs
, cleanedSource
, stackFile
, pkg-def-extras ? []
}:
let
  # package-desc = import ./plan.nix;
  # butcher-plan = {
  #   inherit resolver;
  #   extras = hackage:
  #     { butcher = args: package-desc args // {
  #       src = pkgs.haskell-nix.cleanSourceHaskell {
  #         src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; name = "butcher"; };
  #         name = "butcher";
  #       };
  #     };
  #   };
  # };
  # this does not work at all, does not use local package (!)
  # butcher-plan = (pkgs.haskell-nix.importAndFilterProject (
  #   (pkgs.haskell-nix.callStackToNix {
  #     name = "butcher-plan";
  #     src = ./..;
  #     stackYamlFile = builtins.toFile "stack.yaml" ''
  #       resolver: ${resolver}
  #       packages:
  #         - '.'
  #       extra-deps: []
  #       extra-package-dbs: []
  #     '';
  #     ignorePackageYaml = true;
  #   })
  # ));
  cleanedSource = pkgs.haskell-nix.cleanSourceHaskell { name = "butcher-"+stackFile; src = ./..; };
  butcher-nix = pkgs.haskell-nix.callStackToNix {
    name = "butcher";
    src = cleanedSource;
    stackYaml = stackFile;
  };
  butcher-plan = pkgs.haskell-nix.importAndFilterProject butcher-nix;
  # butcher-pkgs = {
  #   inherit (butcher-plan.pkgs) modules resolver;
  #   extras = butcher-plan.pkgs.extras ps;
  # };
  generatedCache = pkgs.haskell-nix.genStackCache {
    src = cleanedSource;
    stackYaml = stackFile;
  };
  hsPkgs = (pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = butcher-plan.pkgs;
    pkg-def-extras = pkg-def-extras;
    modules = pkgs.lib.singleton (pkgs.haskell-nix.mkCacheModule generatedCache);
  }).config.hsPkgs;
in {
  inherit butcher-plan hsPkgs pkgs;
  inherit (hsPkgs) butcher;
  inherit (hsPkgs.butcher) checks;
  shell = hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      butcher
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = false;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    # tools = { cabal = "3.2.0.0"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs.haskellPackages;
      [ cabal-install ghcid bash pkgs.nix ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  };
}
# pkgs.haskell-nix.stackProject {
#   src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "butcher"; };
#   pkg-def-extras = pkg-def-extras;
#   modules = [
#     { doHaddock = false; }
#   ];
# }
