
This nix setup expects the iohk haskell-nix overlay to be available/included
when importing `<nixpkgs>`. Also, you might need a specific commit if you
want to test against all supported ghcs (8.4 - 8.10, currently).

# Useful commands:

~~~~.sh
# enter a shell for a specific build-plan
# (cabal-solved with ghc-8.4 in this case)
nix-shell nix/all.nix -A '"hackage-8.4".shell'
# run tests against ghcs 8.4 through 8.10, both against hackage and stackage package sets
nix/ci.sh
~~~~


# Files in this directory:

all.nix          - main entrypoint into this package's nix world
via-hackage.nix  - how to build this via cabal-solved package-set
via-stackage.nix - how to build via stackage-based package set
nixpkgs.nix      - optional - if you want to use a custom nixpkgs channel
                   (the replacement needs to have haskell-nix overlay _and_
                   the cabal-check feature enabled though!)
local-extra-deps.nix - optional - for defining local addition deps for
                   dev testing

(plus some currently unused:)

materialized  - materializations of cabal-solved build-plans
plan.nix      - manual materialization of unsolved build-plan (used with
                stackage snapshot to build package set)
