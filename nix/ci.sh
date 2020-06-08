
OUTDIR="ci-out"
SUMMARY="$OUTDIR/0-summary"
CABAL_CHECK_ATTRPATH="hackage-8.10"

set -x

mkdir -p "$OUTDIR"
echo "# test summary" > "$SUMMARY"

function build-one {
  local ATTRPATH=$1
  # nix-build --no-out-link nix/all.nix -A "\"$ATTRPATH\".butcher.components.library"\
  #   2> >(tee "$OUTDIR/$ATTRPATH-1-build-lib.txt" >&2)
  # (($? == 0)) || { echo "$ATTRPATH: build src failed" >> "$SUMMARY"; return 1; }
  # nix-build --no-out-link nix/all.nix -A "\"$ATTRPATH\".butcher.components.tests"\
  #   2> >(tee "$OUTDIR/$ATTRPATH-2-build-test.txt" >&2)
  # (($? == 0)) || { echo "$ATTRPATH: build test failed" >> "$SUMMARY"; return 1; }
  OUT=$(nix-build -o "$OUTDIR/$ATTRPATH-test-result.txt" nix/all.nix -A "\"$ATTRPATH\".butcher.checks.tests"\
    2> >(tee "$OUTDIR/$ATTRPATH-build.txt" >&2))
  (($? == 0)) || { echo "$ATTRPATH: run test failed" >> "$SUMMARY"; return 1; }
  echo "$ATTRPATH: $(grep examples "$OUTDIR/$ATTRPATH-test-result.txt")" >> "$SUMMARY"
}

function cabal-check {
  nix-build --no-out-link nix/all.nix -A "\"$CABAL_CHECK_ATTRPATH\".checks.cabal-check"\
    2> >(tee "$OUTDIR/cabal-check.txt" >&2)
  (($? == 0)) || { echo "cabal-check: failed" >> "$SUMMARY"; return 1; }
  echo "cabal-check: success" >> "$SUMMARY"
}

find "$OUTDIR" -name "stackage*" -delete
find "$OUTDIR" -name "hackage*" -delete
rm "$OUTDIR/cabal-check.txt"
CLEANEDSOURCE=$(nix-instantiate --eval --read-write-mode nix/all.nix -A "cleanedSource.outPath")
(($? == 0)) || exit 1
( eval "cd $CLEANEDSOURCE; find" ) > "$OUTDIR/1-cleanedSource.txt"

build-one "stackage-8.4"
build-one "stackage-8.6"
build-one "stackage-8.8"

build-one "hackage-8.4"
build-one "hackage-8.6"
build-one "hackage-8.8"
build-one "hackage-8.10"

cabal-check

cat "$SUMMARY"
