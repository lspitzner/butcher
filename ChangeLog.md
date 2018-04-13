# Revision history for butcher

## 1.3.0.1  -- April 2018

* Support ghc-8.4
* Drop support for ghc<8

## 1.3.0.0  -- February 2018

* Experimental: Hidden commandparts (do not appear in help)
* Experimental: Bash completion
* Add addHelpCommandWith to support user-defined column count
* Fix help document printing (ribbons)
* Fix completion behaviour

## 1.2.1.0  -- November 2017

* Fix bug in 'ppUsageWithHelp'
* some utilities for interactive usage in new module
  `UI.Butcher.Monadic.Interactive`

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
