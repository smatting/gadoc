-- This module tries to be a port of Query.hs from Hoogle

module Query

where

import Prelude
import Data.Maybe
import Data.Either
import Text.Parsing.StringParser as SP
import Text.Parsing.StringParser.CodePoints as SPCP
import Text.Parsing.StringParser.Combinators as SPC
import Data.List.Types (toList)
import Data.List (List(..), singleton, toUnfoldable, many, concat, null, (:), take, length)
import Data.String.CodePoints (singleton, codePointFromChar, fromCodePointArray) as CP
import Data.String.CodeUnits (uncons, toCharArray)
import Data.String (contains, Pattern(..))
import Data.Foldable (oneOf, foldMap, elem, notElem, foldr, any, all, intercalate)
import Data.Array as Arr
import Control.Alt ((<|>))
import Data.Char.Unicode (isSymbol, isPunctuation, isAlpha, isUpper)
import Data.Tuple
import Data.Functor
import Control.Category (identity)

import Text.Parsing.Parser (ParseError, ParseState(..), Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (try, tryRethrow, lookAhead, manyTill)
import Text.Parsing.Parser.Token as PT
import Text.Parsing.Parser.Pos (initialPos)
import Control.Monad.State (gets, modify)
import Data.Eq

import Effect
import Effect.Console


isSym :: Char -> Boolean
isSym x = ((isSymbol x || isPunctuation x) && x `notElem` special) || x `elem` ascSymbol
    where special = ['(', ')', ',', ';', '[', ']', '`', '{', '}', '"', '\'']
          ascSymbol = ['!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

sym :: SP.Parser Char
sym = SPCP.satisfy isSym

isSyms :: String -> Boolean
isSyms s
  | s `elem` allBrackets = false
  | otherwise =
      case uncons s of
        Nothing -> false
        Just x -> isSym x.head

l2s :: List Char -> String
l2s xs = CP.fromCodePointArray (toUnfoldable (CP.codePointFromChar <$> xs))

s2l :: String -> List Char
s2l s = Arr.toUnfoldable (toCharArray s)

tuple :: SP.Parser String
tuple = do
  open <- SPCP.char '('
  commas <- toList <$> SPC.many1 (SPCP.char ',')
  close <- SPCP.char ')'
  pure $ l2s (singleton open <> commas <> singleton close)

openBrackets :: Array String
openBrackets = ["(#","[:","(","["]

shutBrackets :: Array String
shutBrackets = ["#)",":]",")","]"]

allBrackets :: Array String
allBrackets =
    (Arr.zipWith (<>) openBrackets shutBrackets)
    <> openBrackets
    <> shutBrackets

brackets :: SP.Parser String
brackets = oneOf $ SPCP.string <$> allBrackets

someWhiteSpace :: SP.Parser String
someWhiteSpace = do
  cs <- SPC.many1 (SPCP.satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure " "

identifier :: SP.Parser String
identifier = do
  begin <- singleton <$> SPCP.anyLetter
  middle <- SPC.many (SPCP.alphaNum <|> SPCP.oneOf ['_', '\'', '#', '-'])
  end <- fromMaybe Nil <$> (optional (singleton <$> SPCP.char '-'))
  pure (l2s (begin <> middle <> end))

symbols :: SP.Parser String
symbols = l2s <<< toList <$> SPC.many1 sym

comma :: SP.Parser String
comma = SPCP.char ',' *> pure " "

lexeme :: SP.Parser String
lexeme =
      SP.try tuple
  <|> SP.try brackets
  <|> SP.try someWhiteSpace
  <|> SP.try identifier
  <|> SP.try symbols
  <|> SP.try comma


lexemes :: SP.Parser (List String)
lexemes = concat <$> many g
  where
    g = (singleton <$> lexeme) <|> (SPCP.anyChar *> pure Nil)

lexer :: String -> Array String
lexer s =
  either (const []) (Arr.fromFoldable) (SP.runParser lexemes s)


--------------------------------------------------------------------------------


data Query
    = QueryName {fromQueryName :: String}
    | QueryType {fromQueryType :: String}
    | QueryScope {scopeInclude :: Boolean, scopeCategory :: String, scopeValue :: String}
    | QueryNone String -- part of the query that is ignored

derive instance eqQuery :: Eq Query

instance showQuery :: Show Query
  where
    show (QueryName d) = "QueryName \"" <> d.fromQueryName <> "\""
    show (QueryScope d) = "QueryScope " <> show d.scopeInclude <> " \"" <> d.scopeCategory <> "\" \"" <> d.scopeValue <> "\""
    show (QueryNone d) = "QueryNone " <> d
    show (QueryType d) = "QueryType " <> d.fromQueryType

type P = Parser (List String)


m :: forall a b. (a -> Maybe b) -> (Array b -> Array b) -> Array a -> Array b
m f g xs = g (Arr.catMaybes (map f xs))

unwords :: Array String -> String
unwords xs = intercalate " " xs

m2a :: forall a. Maybe a -> Array a
m2a (Just x) = [x]
m2a nothing = []

bool :: forall a. a -> a -> Boolean -> a
bool x y b = if b then y else x


renderQuery :: Array Query -> String
renderQuery [] = "No query"
renderQuery qs = do
  unwords $
    m (\q -> case q of
           QueryName d -> Just d.fromQueryName
           _ -> Nothing)
      identity
      qs
    <> m (\q -> case q of
           QueryType t -> Just t.fromQueryType
           _ -> Nothing)
         identity
         qs
    <> m (\q -> case q of
            QueryScope scp -> Just $
              if scp.scopeCategory == "package"
                then bool "-" "+" scp.scopeInclude <> scp.scopeValue
                else (bool "-" "" scp.scopeInclude) <> scp.scopeCategory <> ":" <> scp.scopeValue

            _ -> Nothing)
         identity
         qs
    <> m (\q -> case q of
            QueryNone n -> Just $
              "(\" " <> n <> "\" ignored)"
            _ -> Nothing)
          identity
          qs


string :: String -> P String
string = PT.match (const initialPos)

anyToken :: P String
anyToken = PT.token (const initialPos)

pushBack :: String -> P Unit
pushBack x =
  void $ modify (\(ParseState s p b) -> ParseState (x:s) p b)


eol :: P Unit
eol = do
  input <- gets \(ParseState input _ _) -> input
  unless (null input) (fail "Expected EOF")

isAlpha1 :: String -> Boolean
isAlpha1 s =
  case uncons s of
    Nothing -> false
    Just d -> isAlpha d.head

alpha1 :: P String
alpha1 = PT.when (const initialPos) isAlpha1

scope_ :: P (Tuple (List Query) (List String))
scope_ =
      tryRethrow ( do
        pm <- readPM
        cat <- readCat
        void $ string ":"
        mod <- readMod
        add pm cat mod
      )
      <|> tryRethrow ( do
        pm <- readPM
        cat <- readCat
        void $ string ":-"
        mod <- readMod
        add false cat mod
      )
      <|> tryRethrow ( do
        pm <- readPM
        mod <- readMod
        add_ pm mod
      )
      <|> tryRethrow ( do
        cat <- readCat
        void $ string ":"
        mod <- readMod
        add true cat mod
      )
      <|> tryRethrow ( do
        cat <- readCat
        void $ string ":."
        mod <- readMod
        add true cat ("." <> mod)
      )
      <|> tryRethrow ( do
        cat <- readCat
        void $ string ":-"
        mod <- readMod
        add false cat mod
      )
      <|> tryRethrow ( do
        cat <- readCat
        void $ string ":-."
        mod <- readMod
        add false cat ("." <> mod)
      )
      <|> tryRethrow ( do
        void $ string "("
        scp <- readDots
        x <- anyToken
        void $ string ")"
        t <- add_ true scp
        out (Arr.toUnfoldable ["(", x, ")"]) t
      )
      <|> tryRethrow ( do
        scp <- readDots
        add_ true scp
      )
      <|> tryRethrow ( do
        void $ string "("
        void $ string "."
        scp <- readDots
        x <- anyToken
        void $ string ")"
        t <- add_ true ("." <> scp)
        out (Arr.toUnfoldable ["(", x, ")"]) t
      )
      <|> tryRethrow ( do
        void $ string "."
        scp <- readDots
        add_ true ("." <> scp)
      )
      <|> tryRethrow ( do
        x <- anyToken
        t <- scope_
        out (Arr.toUnfoldable [x]) t
      )
      <|> tryRethrow ( do
        eol
        pure (Tuple Nil Nil)
      )
  where
    add pm cat mod = do
      let query = QueryScope {scopeInclude: pm, scopeCategory: cat, scopeValue: mod}
      Tuple queries strings <- scope_
      pure $ Tuple (query:queries) strings

    add_ a c = add a b c
      where b = if (contains (Pattern ".") c || any isUpper (take 1 (s2l c)))
                  then "module"
                  else "package"

    out xs (Tuple queries strings) =
      pure (Tuple queries (xs <> strings))

readPM :: P Boolean
readPM =
  (string "+" *> pure true)
  <|> (string "-" *> pure false)

readCat :: P String
readCat = alpha1

dotAnd :: P String
dotAnd = tryRethrow $ do
  s <- anyToken
  case uncons s of
    Nothing -> fail "string is empty"
    Just d ->
      if d.head == '.'
        then pure d.tail
        else fail "string doesn't begin with '.'"


readDots :: P String
readDots = try $ do
  x <- alpha1
  try ((string "." *> readDots) <#> (\a -> x <> "." <> a))
   <|> try ( do
     rest <- dotAnd
     when (rest /= "") $ pushBack rest
     pure x
  )

readMod :: P String
readMod = do
  x <- alpha1
  (   try (dot *> readMod <#> (\a -> x <> "." <> a))
  <|> try (dot *> eol *> pure (x <> "."))
  <|> try (dot *> lookAhead space *> pure (x <> "."))
  <|> try (pure x))
  where
    dot = string "."
    space = string " "

scope' :: Array String -> Either ParseError (Tuple (List Query) (List String))
scope' arr =
    (runParser
      (Arr.toUnfoldable arr)
      scope_)

scope :: Array String -> Tuple (List Query) (List String)
scope arr = either (const (Tuple Nil Nil)) identity (scope' arr)

names_ :: P (List String)
names_ =
  try ( do
    void $ string "("
    x <- anyToken
    void $ string ")"
    (\xs -> f x <> xs) <$> names_
  )
  <|> try ( do
    void $ string "("
    x <- anyToken
    eol
    pure (x : Nil)
  )
  <|> try ( do
    x <- anyToken
    (\xs -> f x <> xs) <$> names_
  )
  <|> try ( do
    eol
    pure Nil
  )
  where
    f x = if x /= " " then x : Nil else Nil

names :: List String -> List String
names xs =
    either (const Nil) identity (runParser xs names_)

break :: (String -> Boolean) -> List String -> Tuple (List String) (List String)
break p Nil = Tuple Nil Nil
break p (x:xs) =
  if p x
    then Tuple Nil xs
    else
      let Tuple pre post = break p xs
        in Tuple (x:pre) post

divide :: List String -> Tuple (List String) (Maybe String)
divide xs = f (names xs) xs
  where
    f ns xs'
      | all isAlpha1 ns = Tuple ns Nothing
      | all isSyms ns = Tuple ns Nothing
      | length ns == 1 = Tuple ns Nothing
      | otherwise =
          case break (\x -> x == "::") xs' of
            Tuple n Nil -> Tuple Nil (Just (intercalate "" n))
            Tuple n t -> Tuple (names n) (Just (intercalate "" t))

maybeToList :: forall a. Maybe a -> List a
maybeToList (Just x) = x:Nil
maybeToList Nothing = Nil

parseQuery :: String -> Array Query
parseQuery x = Arr.fromFoldable $ map qn nam <> map qt (maybeToList typ) <> scp
    where
      qn nam = QueryName {fromQueryName: nam}
      qt t = QueryType {fromQueryType: t}
      Tuple scp rest = scope $ lexer x
      Tuple nam typ = divide rest

shouldEq :: String -> (Tuple (Array String) (Array Query) ->  Tuple (Array String) (Array Query)) -> Effect Unit
shouldEq a f =
  let
    q = parseQuery a
    msg = fst (f (Tuple [] q))
  in if Arr.length msg > 0
     then log (a <> " " <> show q <> " " <> show msg)
     else pure unit

infixr 0 shouldEq as ===

queryTest :: Effect Unit
queryTest = do
  let
    want :: String -> (Query -> Boolean) -> Tuple (Array String) (Array Query) -> Tuple (Array String) (Array Query)
    want s p (Tuple bad q) =
      Tuple
        (if (not (any p q)) then ["missing " <> s] else [])
        (Arr.filter (not <<< p) q)

  let
    wantEq :: Query -> Tuple (Array String) (Array Query) -> Tuple (Array String) (Array Query)
    wantEq v = want (show v) (_ == v)

  let scope b c v = wantEq $ QueryScope {scopeInclude: b, scopeCategory: c, scopeValue: v}

  let name = wantEq <<< (\x -> QueryName { fromQueryName: x})

  "" === identity
  "map" === name "map"
  "#"   === name "#"
  "c#"  === name "c#"
  "-"   === name "-"
  "/"   === name "/"
  "->"  === name "->"

  "foldl'" === name "foldl'"
  "fold'l" === name "fold'l"
  "Int#" === name "Int#"
  "concat map" === name "concat" <<< name "map"
  {-- "a -> b" === typ "a -> b" --}
  {-- "a->b" === typ "a -> b" --}
  {-- "(a b)" === typ "(a b)" --}
  {-- "map :: a -> b" === typ "a -> b" --}
  "+Data.Map map" === scope true "module" "Data.Map" <<< name "map"
  {-- "a -> b package:foo" === scope True "package" "foo" . typ "a -> b" --}
  {-- "a -> b package:foo-bar" === scope True "package" "foo-bar" . typ "a -> b" --}
  "Data.Map.map" === scope true "module" "Data.Map" <<< name "map"
  {-- "[a]" === typ "[a]" --}
  "++" === name "++"
  "(++)" === name "++"
  ":+:" === name ":+:"

  "bytestring-cvs +hackage" === scope true "package" "hackage" <<< name "bytestring-cvs"

  {-- "m => c" === typ "m => c" --}
  {-- "[b ()" === typ "[b ()]" --}
  {-- "[b (" === typ "[b ()]" --}
  {-- "_ -> a" === typpp "_ -> a" --}
  {-- "(a -> b) ->" === typpp "(a -> b) -> _" --}
  {-- "(a -> b) -" === typpp "(a -> b) -> _" --}
  {-- "Monad m => " === typpp "Monad m => _" --}
  "map is:exact" === name "map" <<< scope true "is" "exact"
  "sort set:hackage" === name "sort" <<< scope true "set" "hackage"
  "sort -set:hackage" === name "sort" <<< scope false "set" "hackage"
  "sort set:-hackage" === name "sort" <<< scope false "set" "hackage"
  "sort -set:-hackage" === name "sort" <<< scope false "set" "hackage"
  "package:bytestring-csv" === scope true "package" "bytestring-csv"
  "(>>=)" === name ">>="
  "(>>=" === name ">>="
  ">>=" === name ">>="
  "Control.Monad.mplus" === name "mplus" <<< scope true "module" "Control.Monad"
  "Control.Monad.>>=" === name ">>=" <<< scope true "module" "Control.Monad"
  "Control.Monad.(>>=)" === name ">>=" <<< scope true "module" "Control.Monad"
  "(Control.Monad.>>=)" === name ">>=" <<< scope true "module" "Control.Monad"
  "Control.Monad.(>>=" === name ">>=" <<< scope true "module" "Control.Monad"
  "(Control.Monad.>>=" === name ">>=" <<< scope true "module" "Control.Monad"
  "foo.bar" === name "bar" <<< scope true "package" "foo"
  "insert module:.Map" === name "insert" <<< scope true "module" ".Map"
  "insert module:Map." === name "insert" <<< scope true "module" "Map."
  "insert module:.Map." === name "insert" <<< scope true "module" ".Map."
  ".Map.insert" === name "insert" <<< scope true "module" ".Map"
  ".Map." === scope true "module" ".Map"
--  FIXME: ".Map" === scope true "module" ".Map" -- probably should work, but really needs to rewrite <<< fair bit
  "(.Monad.>>=" === name ">>=" <<< scope true "module" ".Monad"
--  FIXME: "author:Taylor-M.-Hedberg" === scope true "author" "Taylor-M.-Hedberg"
  "author:Bryan-O'Sullivan" === scope true "author" "Bryan-O'Sullivan"
  {-- "\8801" === name "\8801" --}
  {-- "( )" === id -- FIXME: Should probably be () --}
