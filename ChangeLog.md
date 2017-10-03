# Revision history for butcher

## 1.2.0.0  -- October 2017

* Rename some `Monadic.Param.*`, deprecate old versions.
    - `addReadParam` -> `addParamRead`
    - `addReadParamOpt` -> `addParamReadOpt`
    - `addStringParam` -> `addParamString`
    - `addStringParamOpt` -> `addParamStringOpt`
    - `addStringParams` -> `addParamStrings`
    - `addRestOfInputStringParam` -> `addParamRestOfInput`
* Add functions `addParamNoFlagString`, `addParamNoFlagStringOpt`,
  `addParamNoFlagStrings`
* Fix flag parsing behaviour (ignore initial spaces)

## 1.1.1.0  -- October 2017

* Add `addNullCmd` function that descends into childcommand on an epsilon match
* Add `addStringParams` function that reads all remaining words

## 1.1.0.2  -- September 2017

* Improve 'usage' pretty-printing

## 1.1.0.1  -- August 2017

* Adapt for ghc-8.2

## 1.1.0.0  -- May 2017

* First version. Released on an unsuspecting world.
