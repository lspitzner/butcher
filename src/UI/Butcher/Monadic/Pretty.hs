
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
  , ppUsageAt
  , ppHelpShallow
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
-- > playground [--short] NAME [version | help]
ppUsage :: CommandDesc a -> PP.Doc
ppUsage (CommandDesc mParent _help _syn parts out children) = pparents mParent
  <+> PP.fsep (partDocs ++ [subsDoc])
 where
  pparents :: Maybe (String, CommandDesc out) -> PP.Doc
  pparents Nothing        = PP.empty
  pparents (Just (n, cd)) = pparents (_cmd_mParent cd) <+> PP.text n
  partDocs = parts <&> ppPartDescUsage
  subsDoc  = case out of
    _ | null children -> PP.empty -- TODO: remove debug
    Nothing | null parts -> subDoc
            | otherwise  -> PP.parens $ subDoc
    Just{} -> PP.brackets $ subDoc
  subDoc =
    PP.fcat
      $   PP.punctuate (PP.text " | ")
      $   Data.Foldable.toList
      $   children
      <&> \(n, _) -> PP.text n

-- | ppUsageWithHelp exampleDesc yields:
--
-- > playground [--short] NAME
-- >            [version | help]: a simple butcher example program
--
-- And yes, the line break is not optimal in this instance with default print.
ppUsageWithHelp :: CommandDesc a -> PP.Doc
ppUsageWithHelp (CommandDesc mParent help _syn parts out children) =
  pparents mParent <+> PP.fsep (partDocs ++ [subsDoc]) PP.<> helpDoc
 where
  pparents :: Maybe (String, CommandDesc out) -> PP.Doc
  pparents Nothing        = PP.empty
  pparents (Just (n, cd)) = pparents (_cmd_mParent cd) <+> PP.text n
  partDocs = parts <&> ppPartDescUsage
  subsDoc  = case out of
    _ | null children -> PP.empty -- TODO: remove debug
    Nothing | null parts -> subDoc
            | otherwise  -> PP.parens $ subDoc
    Just{} -> PP.brackets $ subDoc
  subDoc =
    PP.fcat
      $   PP.punctuate (PP.text " | ")
      $   Data.Foldable.toList
      $   children
      <&> \(n, _) -> PP.text n
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
    (s:sr) -> find ((s==) . fst) (_cmd_children desc) >>= ppUsageAt sr . snd

-- | ppHelpShalloe exampleDesc yields:
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
ppHelpShallow :: CommandDesc a
              -> PP.Doc
ppHelpShallow desc@(CommandDesc mParent syn help parts _out _children) =
        nameSection
    $+$ usageSection
    $+$ descriptionSection
    $+$ partsSection
    $+$ PP.text ""
  where
    nameSection = case mParent of
      Nothing -> PP.empty
      Just{} ->
            PP.text "NAME"
        $+$ PP.text ""
        $+$ PP.nest 2 (case syn of
                        Nothing -> pparents mParent
                        Just s ->  pparents mParent <+> PP.text "-" <+> s)
        $+$ PP.text ""
    pparents :: Maybe (String, CommandDesc out) -> PP.Doc
    pparents Nothing = PP.empty
    pparents (Just (n, cd)) = pparents (_cmd_mParent cd) PP.<+> PP.text n
    usageSection =
            PP.text "USAGE"
        $+$ PP.text ""
        $+$ PP.nest 2 (ppUsage desc)
    descriptionSection = case help of
      Nothing -> PP.empty
      Just h ->
            PP.text ""
        $+$ PP.text "DESCRIPTION"
        $+$ PP.text ""
        $+$ PP.nest 2 h
    partsSection = if null partsTuples then PP.empty else
            PP.text ""
        $+$ PP.text "ARGUMENTS"
        $+$ PP.text ""
        $+$ PP.nest 2 (PP.vcat partsTuples)
    partsTuples :: [PP.Doc]
    partsTuples = parts >>= go
      where
        go = \case
          PartLiteral{} -> []
          PartVariable{} -> []
          PartOptional p -> go p
          PartAlts ps -> ps >>= go
          PartSeq  ps -> ps >>= go
          PartDefault _ p -> go p
          PartSuggestion _ p -> go p
          PartRedirect s p -> [PP.text s $$ PP.nest 20 (ppPartDescUsage p)]
                           ++ (PP.nest 2 <$> go p)
          PartReorder ps -> ps >>= go
          PartMany p -> go p
          PartWithHelp doc p -> [ppPartDescHeader p $$ PP.nest 20 doc]
                             ++ go p

-- | Internal helper; users probably won't need this.
ppPartDescUsage :: PartDesc -> PP.Doc
ppPartDescUsage = \case
  PartLiteral  s  -> PP.text s
  PartVariable s  -> PP.text s
  PartOptional p  -> PP.brackets $ rec p
  PartAlts     ps -> PP.fcat $ PP.punctuate (PP.text ",") $ rec <$> ps
  PartSeq      ps -> PP.fsep $ rec <$> ps
  PartDefault _ p -> PP.brackets $ rec p
  PartSuggestion s p ->
    PP.parens $ PP.fcat $ PP.punctuate (PP.text "|") $ fmap PP.text s ++ [rec p]
  PartRedirect s _ -> PP.text s
  PartMany p       -> rec p <> PP.text "+"
  PartWithHelp _ p -> rec p
  PartReorder ps ->
    let flags  = [ d | PartMany d <- ps ]
        params = filter
          ( \case
            PartMany{} -> False
            _          -> True
          )
          ps
    in  PP.sep
          [(PP.fsep $ PP.brackets . rec <$> flags), PP.fsep (rec <$> params)]

 where
  rec = ppPartDescUsage

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
 where
  rec = ppPartDescHeader

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

