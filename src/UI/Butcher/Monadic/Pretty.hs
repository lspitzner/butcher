
-- | Pretty-print of CommandDescs. To explain what the different functions
-- do, we will use an example CmdParser. The CommandDesc derived from that
-- CmdParser will serve as example input to the functions in this module.
--
-- > main = mainFromCmdParserWithHelpDesc $ \helpDesc -> do
-- > 
-- >   addCmdSynopsis "a simple butcher example program"
-- >   addCmdHelpStr "a very long help document"
-- > 
-- >   addCmd "version" $ do
-- >     porcelain <- addSimpleBoolFlag "" ["porcelain"]
-- >       (flagHelpStr "print nothing but the numeric version")
-- >     addCmdHelpStr "prints the version of this program"
-- >     addCmdImpl $ putStrLn $ if porcelain
-- >       then "0.0.0.999"
-- >       else "example, version 0.0.0.999"
-- > 
-- >   addCmd "help" $ addCmdImpl $ print $ ppHelpShallow helpDesc
-- > 
-- >   short <- addSimpleBoolFlag "" ["short"] (flagHelpStr "make the greeting short")
-- >   name <- addStringParam "NAME"
-- >     (paramHelpStr "your name, so you can be greeted properly")
-- > 
-- >   addCmdImpl $ do
-- >     if short
-- >       then putStrLn $ "hi, " ++ name ++ "!"
-- >       else putStrLn $ "hello, " ++ name ++ ", welcome from butcher!"
module UI.Butcher.Monadic.Pretty
  ( ppUsage
  , ppUsageShortSub
  , ppUsageAt
  , ppHelpShallow
  , ppHelpDepthOne
  , ppUsageWithHelp
  , ppPartDescUsage
  , ppPartDescHeader
  , parsingErrorString
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS

import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint ( (<+>), ($$), ($+$) )

import           Data.HList.ContainsType

import           UI.Butcher.Monadic.Internal.Types
import           UI.Butcher.Monadic.Internal.Core



-- | ppUsage exampleDesc yields:
--
-- > example [--short] NAME [version | help]
ppUsage :: CommandDesc a -> PP.Doc
ppUsage (CommandDesc mParent _syn _help parts out children _hidden) =
  pparents mParent <+> PP.sep [PP.fsep partDocs, subsDoc]
 where
  pparents :: Maybe (Maybe String, CommandDesc out) -> PP.Doc
  pparents Nothing              = PP.empty
  pparents (Just (Just n , cd)) = pparents (_cmd_mParent cd) <+> PP.text n
  pparents (Just (Nothing, cd)) = pparents (_cmd_mParent cd)
  partDocs = Maybe.mapMaybe ppPartDescUsage parts
  visibleChildren =
    [ (n, c) | (Just n, c) <- children, _cmd_visibility c == Visible ]
  subsDoc = case out of
    _ | null visibleChildren -> PP.empty
    Nothing | null parts -> subDoc
            | otherwise  -> PP.parens $ subDoc
    Just{} -> PP.brackets $ subDoc
  subDoc =
    PP.fcat
      $ PP.punctuate (PP.text " | ")
      $ Data.Foldable.toList
      $ (PP.text . fst) <$> visibleChildren

-- | ppUsageShortSub exampleDesc yields:
--
-- > example [--short] NAME <command>
--
-- I.e. Subcommands are abbreviated using the @<command>@ label, instead
-- of being listed.
ppUsageShortSub :: CommandDesc a -> PP.Doc
ppUsageShortSub (CommandDesc mParent _syn _help parts out children _hidden) =
  pparents mParent <+> PP.sep [PP.fsep partDocs, subsDoc]
 where
  pparents :: Maybe (Maybe String, CommandDesc out) -> PP.Doc
  pparents Nothing              = PP.empty
  pparents (Just (Just n , cd)) = pparents (_cmd_mParent cd) <+> PP.text n
  pparents (Just (Nothing, cd)) = pparents (_cmd_mParent cd)
  partDocs = Maybe.mapMaybe ppPartDescUsage parts
  visibleChildren =
    [ (n, c) | (Just n, c) <- children, _cmd_visibility c == Visible ]
  subsDoc = case out of
    _ | null visibleChildren -> PP.empty
    Nothing                  -> subDoc
    Just{}                   -> PP.brackets $ subDoc
  subDoc = if null visibleChildren then PP.empty else PP.text "<command>"

-- | ppUsageWithHelp exampleDesc yields:
--
-- > example [--short] NAME
-- >         [version | help]: a simple butcher example program
--
-- And yes, the line break is not optimal in this instance with default print.
ppUsageWithHelp :: CommandDesc a -> PP.Doc
ppUsageWithHelp (CommandDesc mParent _syn help parts out children _hidden) =
  pparents mParent <+> PP.fsep (partDocs ++ [subsDoc]) PP.<> helpDoc
 where
  pparents :: Maybe (Maybe String, CommandDesc out) -> PP.Doc
  pparents Nothing              = PP.empty
  pparents (Just (Just n , cd)) = pparents (_cmd_mParent cd) <+> PP.text n
  pparents (Just (Nothing, cd)) = pparents (_cmd_mParent cd)
  partDocs = Maybe.mapMaybe ppPartDescUsage parts
  subsDoc  = case out of
    _ | null children -> PP.empty -- TODO: remove debug
    Nothing | null parts -> subDoc
            | otherwise  -> PP.parens $ subDoc
    Just{} -> PP.brackets $ subDoc
  subDoc =
    PP.fcat
      $ PP.punctuate (PP.text " | ")
      $ Data.Foldable.toList
      $ [ PP.text n | (Just n, c) <- children, _cmd_visibility c == Visible ]
  helpDoc = case help of
    Nothing -> PP.empty
    Just h  -> PP.text ":" PP.<+> h

-- | > ppUsageAt [] = ppUsage
--
-- fromJust $ ppUsageAt ["version"] exampleDesc yields:
--
-- > example version [--porcelain]
ppUsageAt :: [String] -- (sub)command sequence
          -> CommandDesc a
          -> Maybe PP.Doc
ppUsageAt strings desc =
  case strings of
    [] -> Just $ ppUsage desc
    (s:sr) -> find ((Just s==) . fst) (_cmd_children desc) >>= ppUsageAt sr . snd

-- | ppHelpShallow exampleDesc yields:
--
-- > NAME
-- > 
-- >   example - a simple butcher example program
-- > 
-- > USAGE
-- > 
-- >   example [--short] NAME [version | help]
-- > 
-- > DESCRIPTION
-- > 
-- >   a very long help document
-- > 
-- > ARGUMENTS
-- > 
-- >   --short             make the greeting short
-- >   NAME                your name, so you can be greeted properly
ppHelpShallow :: CommandDesc a -> PP.Doc
ppHelpShallow desc =
  nameSection
    $+$ usageSection
    $+$ descriptionSection
    $+$ partsSection
    $+$ PP.text ""
 where
  CommandDesc mParent syn help parts _out _children _hidden = desc
  nameSection = case mParent of
    Nothing -> PP.empty
    Just{} ->
      PP.text "NAME"
        $+$ PP.text ""
        $+$ PP.nest
              2
              ( case syn of
                Nothing -> pparents mParent
                Just s  -> pparents mParent <+> PP.text "-" <+> s
              )
        $+$ PP.text ""
  pparents :: Maybe (Maybe String, CommandDesc out) -> PP.Doc
  pparents Nothing              = PP.empty
  pparents (Just (Just n , cd)) = pparents (_cmd_mParent cd) PP.<+> PP.text n
  pparents (Just (Nothing, cd)) = pparents (_cmd_mParent cd)
  usageSection = PP.text "USAGE" $+$ PP.text "" $+$ PP.nest 2 (ppUsage desc)
  descriptionSection = case help of
    Nothing -> PP.empty
    Just h ->
      PP.text "" $+$ PP.text "DESCRIPTION" $+$ PP.text "" $+$ PP.nest 2 h
  partsSection = if null partsTuples
    then PP.empty
    else PP.text "" $+$ PP.text "ARGUMENTS" $+$ PP.text "" $+$ PP.nest
      2
      (PP.vcat partsTuples)
  partsTuples :: [PP.Doc]
  partsTuples = parts >>= go
   where
    go = \case
      PartLiteral{}      -> []
      PartVariable{}     -> []
      PartOptional p     -> go p
      PartAlts     ps    -> ps >>= go
      PartSeq      ps    -> ps >>= go
      PartDefault    _ p -> go p
      PartSuggestion _ p -> go p
      PartRedirect s p ->
        [PP.text s $$ PP.nest 20 (fromMaybe PP.empty $ ppPartDescUsage p)]
          ++ (PP.nest 2 <$> go p)
      PartReorder ps     -> ps >>= go
      PartMany    p      -> go p
      PartWithHelp doc p -> [ppPartDescHeader p $$ PP.nest 20 doc] ++ go p
      PartHidden{}       -> []

-- | ppHelpDepthOne exampleDesc yields:
--
-- > NAME
-- > 
-- >   example - a simple butcher example program
-- > 
-- > USAGE
-- > 
-- >   example [--short] NAME <command>
-- > 
-- > DESCRIPTION
-- > 
-- >   a very long help document
-- > 
-- > COMMANDS
-- > 
-- >   version
-- >   help
-- > 
-- > ARGUMENTS
-- > 
-- >   --short             make the greeting short
-- >   NAME                your name, so you can be greeted properly
ppHelpDepthOne :: CommandDesc a -> PP.Doc
ppHelpDepthOne desc =
  nameSection
    $+$ usageSection
    $+$ descriptionSection
    $+$ commandSection
    $+$ partsSection
    $+$ PP.text ""
 where
  CommandDesc mParent syn help parts _out children _hidden = desc
  nameSection = case mParent of
    Nothing -> PP.empty
    Just{} ->
      PP.text "NAME"
        $+$ PP.text ""
        $+$ PP.nest
              2
              ( case syn of
                Nothing -> pparents mParent
                Just s  -> pparents mParent <+> PP.text "-" <+> s
              )
        $+$ PP.text ""
  pparents :: Maybe (Maybe String, CommandDesc out) -> PP.Doc
  pparents Nothing              = PP.empty
  pparents (Just (Just n , cd)) = pparents (_cmd_mParent cd) PP.<+> PP.text n
  pparents (Just (Nothing, cd)) = pparents (_cmd_mParent cd)
  usageSection =
    PP.text "USAGE" $+$ PP.text "" $+$ PP.nest 2 (ppUsageShortSub desc)
  descriptionSection = case help of
    Nothing -> PP.empty
    Just h ->
      PP.text "" $+$ PP.text "DESCRIPTION" $+$ PP.text "" $+$ PP.nest 2 h
  visibleChildren =
    [ (n, c) | (Just n, c) <- children, _cmd_visibility c == Visible ]
  childDescs = visibleChildren <&> \(n, c) ->
    PP.text n $$ PP.nest 20 (fromMaybe PP.empty (_cmd_synopsis c))
  commandSection = if null visibleChildren
    then PP.empty
    else PP.text "" $+$ PP.text "COMMANDS" $+$ PP.text "" $+$ PP.nest
      2
      (PP.vcat $ Data.Foldable.toList childDescs)
  partsSection = if null partsTuples
    then PP.empty
    else PP.text "" $+$ PP.text "ARGUMENTS" $+$ PP.text "" $+$ PP.nest
      2
      (PP.vcat partsTuples)
  partsTuples :: [PP.Doc]
  partsTuples = parts >>= go
   where
    go = \case
      PartLiteral{}      -> []
      PartVariable{}     -> []
      PartOptional p     -> go p
      PartAlts     ps    -> ps >>= go
      PartSeq      ps    -> ps >>= go
      PartDefault    _ p -> go p
      PartSuggestion _ p -> go p
      PartRedirect s p ->
        [PP.text s $$ PP.nest 20 (fromMaybe PP.empty $ ppPartDescUsage p)]
          ++ (PP.nest 2 <$> go p)
      PartReorder ps     -> ps >>= go
      PartMany    p      -> go p
      PartWithHelp doc p -> [ppPartDescHeader p $$ PP.nest 20 doc] ++ go p
      PartHidden{}       -> []

-- | Internal helper; users probably won't need this.
ppPartDescUsage :: PartDesc -> Maybe PP.Doc
ppPartDescUsage = \case
  PartLiteral  s -> Just $ PP.text s
  PartVariable s -> Just $ PP.text s
  PartOptional p -> PP.brackets <$> rec p
  PartAlts ps ->
    [ PP.fcat $ PP.punctuate (PP.text ",") ds
    | let ds = Maybe.mapMaybe rec ps
    , not (null ds)
    ]
  PartSeq ps -> [ PP.fsep ds | let ds = Maybe.mapMaybe rec ps, not (null ds) ]
  PartDefault    _ p -> PP.brackets <$> rec p
  PartSuggestion s p -> rec p <&> \d ->
    PP.parens $ PP.fcat $ PP.punctuate (PP.text "|") $ fmap PP.text s ++ [d]
  PartRedirect s _ -> Just $ PP.text s
  PartMany p       -> rec p <&> (<> PP.text "+")
  PartWithHelp _ p -> rec p
  PartReorder ps ->
    let flags  = [ d | PartMany d <- ps ]
        params = filter
          ( \case
            PartMany{} -> False
            _          -> True
          )
          ps
    in  Just $ PP.sep
          [ (PP.fsep $ PP.brackets <$> Maybe.mapMaybe rec flags)
          , PP.fsep (Maybe.mapMaybe rec params)
          ]
  PartHidden{} -> Nothing
  where rec = ppPartDescUsage

-- | Internal helper; users probably won't need this.
ppPartDescHeader :: PartDesc -> PP.Doc
ppPartDescHeader = \case
  PartLiteral  s     -> PP.text s
  PartVariable s     -> PP.text s
  PartOptional ds'   -> rec ds'
  PartAlts     alts  -> PP.hcat $ List.intersperse (PP.text ",") $ rec <$> alts
  PartDefault    _ d -> rec d
  PartSuggestion _ d -> rec d
  PartRedirect   s _ -> PP.text s
  PartMany ds        -> rec ds
  PartWithHelp _ d   -> rec d
  PartSeq     ds     -> PP.hsep $ rec <$> ds
  PartReorder ds     -> PP.vcat $ rec <$> ds
  PartHidden  d      -> rec d
  where rec = ppPartDescHeader

-- | Simple conversion from 'ParsingError' to 'String'.
parsingErrorString :: ParsingError -> String
parsingErrorString (ParsingError mess remaining) =
  "error parsing arguments: " ++ messStr ++ remainingStr
 where
  messStr      = case mess of
    []    -> ""
    (m:_) -> m ++ " "
  remainingStr = case remaining of
    InputString ""  -> "at the end of input."
    InputString str -> case show str of
      s | length s < 42 -> "at: " ++ s ++ "."
      s                 -> "at: " ++ take 40 s ++ "..\"."
    InputArgs   []  -> "at the end of input"
    InputArgs   xs  -> case List.unwords $ show <$> xs of
      s | length s < 42 -> "at: " ++ s ++ "."
      s                 -> "at: " ++ take 40 s ++ "..\"."

