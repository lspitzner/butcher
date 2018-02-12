-- | Utilities when writing interactive programs that interpret commands,
-- e.g. a REPL.
module UI.Butcher.Monadic.Interactive
  ( simpleCompletion
  , shellCompletionWords
  , interactiveHelpDoc
  , partDescStrings
  )
where



#include "prelude.inc"

import qualified Text.PrettyPrint as PP

import           UI.Butcher.Monadic.Internal.Types
import           UI.Butcher.Monadic.Internal.Core
import           UI.Butcher.Monadic.Pretty



-- | Derives a potential completion from a given input string and a given
-- 'CommandDesc'. Considers potential subcommands and where available the
-- completion info present in 'PartDesc's.
simpleCompletion
  :: String         -- ^ input string
  -> CommandDesc () -- ^ CommandDesc obtained on that input string
  -> String         -- ^ "remaining" input after the last successfully parsed
                    -- subcommand. See 'UI.Butcher.Monadic.runCmdParserExt'.
  -> String         -- ^ completion, i.e. a string that might be appended
                    -- to the current prompt when user presses tab.
simpleCompletion line cdesc pcRest = List.drop (List.length lastWord)
  $ longestCommonPrefix choices
 where
  longestCommonPrefix [] = ""
  longestCommonPrefix (c1:cr) =
    case find (\s -> List.all (s `isPrefixOf`) cr) $ reverse $ List.inits c1 of
      Nothing -> ""
      Just x  -> x
  nameDesc = case _cmd_mParent cdesc of
    Nothing -> cdesc
    Just (_, parent) | null pcRest && not (null lastWord) -> parent
        -- not finished writing a command. if we have commands abc and abcdef,
        -- we may want "def" as a completion after "abc".
    Just{}  -> cdesc
  lastWord = reverse $ takeWhile (not . Char.isSpace) $ reverse $ line
  choices :: [String]
  choices = join
    [ [ r
      | (Just r, _) <- Data.Foldable.toList (_cmd_children nameDesc)
      , lastWord `isPrefixOf` r
      , lastWord /= r
      ]
    , [ s
      | s <- partDescStrings =<< _cmd_parts nameDesc
      , lastWord `isPrefixOf` s
      , lastWord /= s
      ]
    ]


-- | Derives a list of completion items from a given input string and a given
-- 'CommandDesc'. Considers potential subcommands and where available the
-- completion info present in 'PartDesc's.
--
-- See 'addShellCompletion' which uses this.
shellCompletionWords
  :: String         -- ^ input string
  -> CommandDesc () -- ^ CommandDesc obtained on that input string
  -> String         -- ^ "remaining" input after the last successfully parsed
                    -- subcommand. See 'UI.Butcher.Monadic.runCmdParserExt'.
  -> [CompletionItem]
shellCompletionWords line cdesc pcRest = choices
 where
  nameDesc = case _cmd_mParent cdesc of
    Nothing -> cdesc
    Just (_, parent) | null pcRest && not (null lastWord) -> parent
        -- not finished writing a command. if we have commands abc and abcdef,
        -- we may want "def" as a completion after "abc".
    Just{}  -> cdesc
  lastWord = reverse $ takeWhile (not . Char.isSpace) $ reverse $ line
  choices :: [CompletionItem]
  choices = join
    [ [ CompletionString r
      | (Just r, _) <- Data.Foldable.toList (_cmd_children nameDesc)
      , lastWord `isPrefixOf` r
      , lastWord /= r
      ]
    , [ c
      | c <- partDescCompletions =<< _cmd_parts cdesc
      , case c of
        CompletionString s -> lastWord `isPrefixOf` s && lastWord /= s
        _                  -> True
      ]
    ]


-- | Produces a 'PP.Doc' as a hint for the user during interactive command
-- input. Takes the current (incomplete) prompt line into account. For example
-- when you have commands (among others) \'config set-email\' and
-- \'config get-email\', then on empty prompt there will be an item \'config\';
-- on the partial prompt \'config \' the help doc will contain the
-- \'set-email\' and \'get-email\' items.
interactiveHelpDoc
  :: String         -- ^ input string
  -> CommandDesc () -- ^ CommandDesc obtained on that input string
  -> String         -- ^ "remaining" input after the last successfully parsed
                    -- subcommand. See 'UI.Butcher.Monadic.runCmdParserExt'.
  -> Int            -- ^ max length of help text
  -> PP.Doc
interactiveHelpDoc cmdline desc pcRest maxLines = if
  | null cmdline             -> helpStrShort
  | List.last cmdline == ' ' -> helpStrShort
  | otherwise                -> helpStr
 where
  helpStr = if List.length optionLines > maxLines
    then
      PP.fcat $ List.intersperse (PP.text "|") $ PP.text . fst <$> optionLines
    else PP.vcat $ optionLines <&> \case
      (s, "") -> PP.text s
      (s, h ) -> PP.text s PP.<> PP.text h
   where
    nameDesc = case _cmd_mParent desc of
      Nothing                        -> desc
      Just (_, parent) | null pcRest -> parent
      Just{}                         -> desc

    lastWord = reverse $ takeWhile (not . Char.isSpace) $ reverse $ cmdline
    optionLines :: [(String, String)]
    optionLines = -- a list of potential words that make sense, given
                    -- the current input.
                  join
      [ [ (s, e)
        | (Just s, c) <- Data.Foldable.toList (_cmd_children nameDesc)
        , lastWord `isPrefixOf` s
        , let e = join $ join
                [ [ " ARGS" | not $ null $ _cmd_parts c ]
                , [ " CMDS" | not $ null $ _cmd_children c ]
                , [ ": " ++ show h | Just h <- [_cmd_help c] ]
                ]
        ]
      , [ (s, "")
        | s <- partDescStrings =<< _cmd_parts nameDesc
        , lastWord `isPrefixOf` s
        ]
      ]
  helpStrShort = ppUsageWithHelp desc


-- | Obtains a list of "expected"/potential strings for a command part
-- described in the 'PartDesc'. In constrast to the 'simpleCompletion'
-- function this function does not take into account any current input, and
-- consequently the output elements can in general not be appended to partial
-- input to form valid input.
partDescStrings :: PartDesc -> [String]
partDescStrings = \case
  PartLiteral  s      -> [s]
  PartVariable _      -> []
  -- TODO: we could handle seq of optional and such much better
  PartOptional x      -> partDescStrings x
  PartAlts     alts   -> alts >>= partDescStrings
  PartSeq      []     -> []
  PartSeq      (x:_)  -> partDescStrings x
  PartDefault    _  x -> partDescStrings x
  PartSuggestion ss x -> [ s | CompletionString s <- ss ] ++ partDescStrings x
  PartRedirect   _  x -> partDescStrings x
  PartReorder xs      -> xs >>= partDescStrings
  PartMany    x       -> partDescStrings x
  PartWithHelp _h x   -> partDescStrings x -- TODO: handle help
  PartHidden{}        -> []


-- | Obtains a list of "expected"/potential strings for a command part
-- described in the 'PartDesc'. In constrast to the 'simpleCompletion'
-- function this function does not take into account any current input, and
-- consequently the output elements can in general not be appended to partial
-- input to form valid input.
partDescCompletions :: PartDesc -> [CompletionItem]
partDescCompletions = \case
  PartLiteral  s      -> [CompletionString s]
  PartVariable _      -> []
  -- TODO: we could handle seq of optional and such much better
  PartOptional x      -> partDescCompletions x
  PartAlts     alts   -> alts >>= partDescCompletions
  PartSeq      []     -> []
  PartSeq      (x:_)  -> partDescCompletions x
  PartDefault    _  x -> partDescCompletions x
  PartSuggestion ss x -> ss ++ partDescCompletions x
  PartRedirect   _  x -> partDescCompletions x
  PartReorder xs      -> xs >>= partDescCompletions
  PartMany    x       -> partDescCompletions x
  PartWithHelp _h x   -> partDescCompletions x -- TODO: handle help
  PartHidden{}        -> []
