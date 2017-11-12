{-# LANGUAGE OverloadedStrings, Rank2Types, RecordWildCards #-}

module Jsn (jsn) where

import System.IO
import Data.Monoid (mconcat)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (Maybe, fromMaybe)
import Data.Either (Either)
import Options.Applicative
import qualified Data.Aeson as J
import Data.Aeson.Lens (key, nth, values)
import Data.List (intercalate, foldl', (\\))
import Text.Read (readMaybe)
import Control.Lens ((^?), (^?!), (.~), ATraversal', Traversal', cloneTraversal)
import qualified Data.HashMap.Lazy as Map
import Data.HashMap.Lazy (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Tuple (swap)
import Debug.Trace (trace)
import Data.Aeson.Encode.Pretty
  ( encodePretty'
  , confIndent
  , confCompare
  , confNumFormat
  , confTrailingNewline
  , Config(..)
  , Indent(..)
  , NumberFormat(..)
  )

type OptionalJsonValue = Either ErrMsg J.Value
type OptionalJsonValues = Either ErrMsg (Vector J.Value)

type JsonMap = HashMap Text J.Value
type JsonVector = Vector J.Value
type JsonValueType = String
type JsonArrayKey = Int
type JsonKey = String
type JsonKeyList = String
type JsonConcretePath = String
type JsonPath = String
type JsonPathSegment = String
type JsonValue = ByteString
type JsonInput = ByteString
type JsonAction = J.Value -> OptionalJsonValue
type JsonActionMulti = J.Value -> OptionalJsonValues
type JsonTraversal = Traversal' J.Value J.Value
type JsonATraversal = ATraversal' J.Value J.Value
type JsonNamedATraversal = ([JsonPathSegment], JsonATraversal)
type JsonEncoder = J.Value -> ByteString

type CommandTitle = String

type ErrMsg = String

data Input = FileInput String | FileInPlace String | StdInput
data ValueInput = Raw String | StdInputValue

type ArrayOutput = Bool

type Reverse = Bool

data ActionOptions = ActionOptions
  { optTolerant :: Bool
  , optZeroPathsAllowed :: Bool
  , optModifiedPart :: Bool
  }

type Action = ActionOptions -> JsonAction
type ActionMulti = ArrayOutput -> ActionOptions -> JsonActionMulti

type HandleActionSuccess = JsonValue -> IO ()

data Command
  = Set JsonPath ValueInput
  | Get JsonPath ArrayOutput
  | New JsonPath ValueInput
  | Delete JsonPath
  | Insert JsonPath ValueInput Reverse
  | Concat JsonPath ValueInput Reverse
  | Merge JsonPath ValueInput Reverse
  | Pop JsonPath Int Reverse
  | Take JsonPath Int Int Reverse
  | Filter JsonPath [JsonKeyList] Reverse
  | Keys JsonPath
  | Values JsonPath
  | Length JsonPath
  | Reverse JsonPath

type ReversibleCommand = Reverse -> Command

data PrintFormat = PrintFormat
  { pfUgly :: Bool
  , pfIndent :: String
  }

data Options = Options Command Input ActionOptions PrintFormat

cloneT :: ATraversal' a a -> Traversal' a a
cloneT = cloneTraversal

putErr :: ErrMsg -> IO ()
putErr = BS.hPutStrLn stderr . BS.pack

splitOn :: Char -> String -> [String]
splitOn d str = go str ""
  where
    go ('\\':'\\':xs) acc = go xs ('\\' : acc)
    go ('\\':x:xs) acc
      | x == d = go xs (d : acc)
      | otherwise = go (x:xs) ('\\' : acc)
    go (x:xs) acc
      | x == d = reverse acc : go xs ""
      | otherwise = go xs (x : acc)
    go [] acc = [reverse acc]

escapeWildcards :: JsonPathSegment -> JsonPathSegment
escapeWildcards ('\\':'*':xs) = '*' : escapeWildcards xs
escapeWildcards (x:xs) = x : escapeWildcards xs
escapeWildcards [] = []

splitPath :: JsonPath -> [JsonPathSegment]
splitPath "" = []
splitPath ('.':p) = splitOn '.' p
splitPath p = splitPath $ '.' : p

splitKeyList :: JsonKeyList -> [JsonKey]
splitKeyList = splitOn ','

makePath :: [JsonPathSegment] -> JsonConcretePath
makePath [] = "[ROOT]"
makePath xs = '.' : (intercalate "." . map printSegment $ xs)

printSegment :: JsonPathSegment -> String
printSegment "" = "[EMPTY]"
printSegment xs = foldr f "" xs
  where
    f '.' = ("[DOT]" ++)
    f x = (x :)

pathHead :: [JsonPathSegment] -> JsonConcretePath
pathHead [] = "[ROOT]"
pathHead (x:_) = x

readArrayInd :: String -> Either ErrMsg JsonArrayKey
readArrayInd str = case readMaybe str :: Maybe Int of
  Nothing -> Left $ "invalid array index: " ++ printSegment str
  Just x
    | x < 0 -> Left $ "negative array index is not valid: " ++ str
    | otherwise -> Right x

typeObject  = "Object"
typeArray   = "Array"
typeString  = "String"
typeNumber  = "Number"
typeBool    = "Bool"
typeNull    = "Null"

jsonType :: J.Value -> JsonValueType
jsonType x = case x of
  J.Object _  -> typeObject
  J.Array _   -> typeArray
  J.String _  -> typeString
  J.Number _  -> typeNumber
  J.Bool _    -> typeBool
  J.Null      -> typeNull

prettyEncode :: PrintFormat -> Either ErrMsg JsonEncoder
prettyEncode PrintFormat {..}
  | pfUgly = Right J.encode
  | otherwise = indent >>= \i ->
      let
        config :: Config
        config = Config
          { confIndent = i
          , confCompare = mempty
          , confNumFormat = Generic
          , confTrailingNewline = False
          }
      in
        Right $ encodePretty' config
  where
    indent :: Either ErrMsg Indent
    indent
      | pfIndent == "t" = Right Tab
      | otherwise = withError $
          case (readMaybe pfIndent :: Maybe Int) of
            Nothing -> Left ""
            Just x
              | x < 0 -> Left ""
              | otherwise -> Right $ Spaces x

    withError = mapLeft . const $
      "-w value must be either a non-negative integer or 't' (for tabs): "
      ++ pfIndent


decodeVal :: JsonValue -> OptionalJsonValue
decodeVal = mapLeft (const "VALUE is not a valid JSON") . J.eitherDecode

deleteNth :: Vector a -> Int -> Vector a
deleteNth v i
  | i >= len = v
  | otherwise = let (l, r) = V.splitAt i v in l V.++ V.init r
  where
    len = V.length v

setNth :: Int -> a -> Vector a -> Either ErrMsg (Vector a)
setNth i val v
  | i > len = Left $
    "index " ++ show i
    ++ " is larger than the length of array: " ++ show len
  | i == len = Right $ V.snoc v val
  | otherwise = Right $ v V.// [(i, val)]
  where
    len = V.length v

errInvalidType :: [JsonValueType] -> J.Value -> ErrMsg
errInvalidType ex v = "has a type of " ++ jsonType v
  ++ ", while it was expected to be: " ++ intercalate "/" ex

errInvalidMemberType ::
  JsonKey
  -> [JsonValueType]
  -> J.Value
  -> ErrMsg
errInvalidMemberType member ex v =
  "member '" ++ member ++ "' " ++ errInvalidType ex v

errInvalidValType :: [JsonValueType] -> J.Value -> ErrMsg
errInvalidValType ex = ("VALUE " ++) . errInvalidType ex

errEmptyPath :: ErrMsg
errEmptyPath = "path can not be empty"

errInvalidPath :: JsonPath -> ErrMsg
errInvalidPath = ("invalid path for the command: " ++)

mapLeft :: (a -> a) -> Either a b -> Either a b
mapLeft f (Left l) = Left $ f l
mapLeft _ x = x

actionHeader :: CommandTitle -> String -> String
actionHeader t x = "jsn " ++ t ++ ": " ++ x

actionErrHeader :: CommandTitle -> Either ErrMsg r -> Either ErrMsg r
actionErrHeader t = mapLeft $ actionHeader t

traverseJson ::
  J.Value
  -> [JsonPathSegment]
  -> Vector JsonNamedATraversal
traverseJson json xs = go xs V.empty root
  where
    root :: JsonNamedATraversal
    root = ([], id)

    go ::
      [JsonPathSegment]                               -- Segments to be parsed
      -> Vector JsonNamedATraversal                   -- Results
      -> JsonNamedATraversal                          -- Previous lookup lens
      -> Vector JsonNamedATraversal
    go ("**":"**":xs) res l = go ("**":xs     ) res l
    go ("**":"*" :xs) res l = go ("*" :"**":xs) res l
    go [] res l = V.cons l res
    go (x:xs) res l@(n, lens)
      | x == "*" = wildcardLenses >>= go xs res
      | x == "**" =
          if null xs then
            V.snoc traverseWildcards l
          else
            let
              next :: JsonPathSegment
              next = escapeWildcards $ head xs

              nextResults :: Vector JsonNamedATraversal
              nextResults = case lookupSegment next of
                Nothing -> V.empty
                Just l -> go (tail xs) res (nameTraversal next l)
            in
               traverseWildcards V.++ nextResults

      | otherwise =
          let
            escaped :: JsonPathSegment
            escaped = escapeWildcards x
          in
            fromMaybe V.empty $
              lookupSegment escaped >>= \l ->
                go xs res (nameTraversal escaped l) <$
                  json ^? (cloneT l :: JsonTraversal)

        where
          val :: J.Value
          val = json ^?! (cloneT lens :: JsonTraversal)

          lookupSegment :: JsonPathSegment -> Maybe JsonATraversal
          lookupSegment x =
            let
              newL :: Maybe JsonATraversal
              newL = case val of
                J.Object _ -> Just $ lens . key (T.pack x)
                J.Array _ -> (\x -> lens . nth x) <$> (readMaybe x :: Maybe Int)
                _ -> Nothing
            in
              newL >>= (\l -> json ^? (cloneT l :: JsonTraversal)) >> newL

          nameTraversal ::
            JsonPathSegment
            -> JsonATraversal
            -> JsonNamedATraversal
          nameTraversal x t = (x : n, t)

          wildcardLenses :: Vector JsonNamedATraversal
          wildcardLenses = case val of
            J.Object m ->
              V.fromList
              $ map (\x -> nameTraversal (T.unpack x) $ lens . key x)
              $ Map.keys m
            J.Array v ->
              V.imap (\i _ -> nameTraversal (show i) $ lens . nth i) v
            _ ->
              V.empty

          traverseWildcards :: Vector JsonNamedATraversal
          traverseWildcards = wildcardLenses >>= go ("**" : xs) res

{- Common parsers -}

parseFileInput :: Parser Input
parseFileInput = FileInput <$> strOption (mconcat
  [ metavar "FILE"
  , short 'f'
  , help "File to read from"
  ])

parseFileInPlace :: Parser Input
parseFileInPlace = FileInPlace <$> strOption (mconcat
  [ metavar "FILE"
  , short 'i'
  , help "In-place file edit"
  ])

-- A dummy flag for reversed order
flagReverse :: Parser Reverse
flagReverse = flag True True mempty

-- A dummy flag for non-reversed order
flagNotReverse :: Parser Reverse
flagNotReverse = flag False False mempty

-- A dummy flag for input from stdin
flagStdInput :: Parser Input
flagStdInput = flag StdInput StdInput mempty

parseInput :: Parser Input
parseInput = parseFileInput <|> parseFileInPlace <|> flagStdInput

parsePath :: Parser JsonPath
parsePath = argument str $ mconcat
  [ metavar "PATH"
  , help "Path to JSON value"
  ]

parseValue :: Parser ValueInput
parseValue = Raw <$> argument str (mconcat
  [ metavar "VALUE"
  , help "Any valid JSON value"
  ])

-- A dummy flag for value from stdin
flagStdInputValue :: Parser ValueInput
flagStdInputValue = flag StdInputValue StdInputValue mempty

parseValueInput :: Parser ValueInput
parseValueInput = parseValue <|> flagStdInputValue

parseActionOptions :: Parser ActionOptions
parseActionOptions = ActionOptions
  <$> flag False True (mconcat
    [ short 't'
    , help $
        "Tolerant mode - "
        ++ "disable errors when command fails at one of the paths"
    ])
  <*> flag False True (mconcat
    [ short 'z'
    , help $
        "Zero paths allowed"
        ++ " - disable error when there are no paths matching a path pattern"
    ])
  <*> flag False True (mconcat
    [ short 'm'
    , help "Modified part - output only value of the last modified JSON member"
    ])

parsePrintFormat :: Parser PrintFormat
parsePrintFormat = PrintFormat
  <$> flag False True (mconcat
    [ short 'u'
    , help "Ugly mode - disable pretty output"
    ])
  <*> strOption (mconcat
    [ showDefault
    , metavar "SPACES"
    , short 'w'
    , value "2"
    , help $
        "Whitespace characters for indentation: "
        ++ "number of spaces or 't' for tabs"
    ])

{- Command parsers -}

parseSet :: Parser Command
parseSet = Set
  <$> parsePath
  <*> parseValueInput

parseGet :: Parser Command
parseGet = Get
  <$> parsePath
  <*> flag False True (mconcat
    [ short 'a'
    , help "Array as output"
    ])

parseKeys :: Parser Command
parseKeys = Keys
  <$> parsePath

parseValues :: Parser Command
parseValues = Values
  <$> parsePath

parseLength :: Parser Command
parseLength = Length
  <$> parsePath

parseReverse :: Parser Command
parseReverse = Reverse
  <$> parsePath

parseNew :: Parser Command
parseNew = New
  <$> parsePath
  <*> parseValueInput

parseDelete :: Parser Command
parseDelete = Delete
  <$> parsePath

parseInsert :: Parser ReversibleCommand
parseInsert = Insert
  <$> parsePath
  <*> parseValueInput

parseConcat :: Parser ReversibleCommand
parseConcat = Concat
  <$> parsePath
  <*> parseValueInput

parseMerge :: Parser ReversibleCommand
parseMerge = Merge
  <$> parsePath
  <*> parseValueInput

parsePop :: Parser ReversibleCommand
parsePop = Pop
  <$> parsePath
  <*> argument auto (mconcat
  [ showDefault
  , value 1
  , help "Number of elements to remove"
  ])

parseTake :: Parser ReversibleCommand
parseTake = Take
  <$> parsePath
  <*> argument auto (mconcat
  [ metavar "N"
  , help "Number of elements to take"
  ])
  <*> option auto (mconcat
  [ short 'b'
  , metavar "INDEX"
  , showDefault
  , value 0
  , help "Begin at - starting index"
  ])

parseFilter :: Parser ReversibleCommand
parseFilter = Filter
  <$> parsePath
  <*> many (argument str $ mconcat
  [ metavar "KEYS"
  , help "List of keys separated by commas"
  ])

{- Combine commands to one parser -}

parseCommands :: Parser Command
parseCommands = subparser $ mconcat
  [ command "s" $ info parseSet (
      progDesc "Set value of member"
    )
  , command "g" $ info parseGet (
      progDesc "Get value of member"
    )
  , command "n" $ info parseNew (
      progDesc
        "New value - like 's', but can create a new member"
    )
  , command "d" $ info parseDelete (
      progDesc "Delete member"
    )
  , command "k" $ info parseKeys (
      progDesc "Keys of object"
    )
  , command "v" $ info parseValues (
      progDesc "Values of object"
    )
  , command "l" $ info parseLength (
      progDesc "Length of member"
    )
  , command "r" $ info parseReverse (
      progDesc "Reverse array"
    )
  , command "i" $ info (parseInsert <*> flagNotReverse) (
      progDesc "Insert value to array"
    )
  , command "I" $ info (parseInsert <*> flagReverse) (
      progDesc "Insert value to the beginning of array"
    )
  , command "c" $ info (parseConcat <*> flagNotReverse) (
      progDesc "Concatenate original array and new array"
    )
  , command "C" $ info (parseConcat <*> flagReverse) (
      progDesc "Concatenate new array and original array"
    )
  , command "m" $ info (parseMerge <*> flagNotReverse) (
      progDesc "Merge new object into object"
    )
  , command "M" $ info (parseMerge <*> flagReverse) (
      progDesc "Merge new object into object, overwrite existing keys"
    )
  , command "p" $ info (parsePop <*> flagNotReverse) (
      progDesc "Pop (delete) N elements from array"
    )
  , command "P" $ info (parsePop <*> flagReverse) (
      progDesc "Pop (delete) N elements from the beginning of array"
    )
  , command "t" $ info (parseTake <*> flagNotReverse) (
      progDesc "Take N elements from array"
    )
  , command "T" $ info (parseTake <*> flagReverse) (
      progDesc "Take N elements from the beginning of array"
    )
  , command "f" $ info (parseFilter <*> flagNotReverse) (
      progDesc "Filter (keep) members by keys"
    )
  , command "F" $ info (parseFilter <*> flagReverse) (
      progDesc "Filter (discard) members by keys"
    )
  ]

{- Top-level parser -}

parseOptions :: Parser Options
parseOptions = Options
  <$> parseCommands
  <*> parseInput
  <*> parseActionOptions
  <*> parsePrintFormat

{- Actions -}

validateActionTraversals ::
  ActionOptions
  -> Vector JsonNamedATraversal
  -> Either ErrMsg (Vector JsonNamedATraversal)
validateActionTraversals opts xs
  | not (optZeroPathsAllowed opts) && V.null xs = Left
      "zero paths matched the path pattern\n  use flag -z to disable this error"
  | otherwise = Right xs

-- Create action which will perform a modification for every traversal
mkAction ::
  (JsonNamedATraversal -> JsonAction)
  -> [JsonPathSegment]
  -> Action
mkAction f xs opts json =
  validateActionTraversals opts (traverseJson json xs) >>= \ts ->
    let
      modifiedPart :: J.Value -> J.Value
      modifiedPart
        | optModifiedPart opts && not (V.null ts) = \val ->
            val ^?! (cloneT . snd . V.last $ ts :: JsonTraversal)
        | otherwise = id
    in
      modifiedPart <$> V.foldl' acc (Right json) ts

    where
      acc :: OptionalJsonValue -> JsonNamedATraversal -> OptionalJsonValue
      acc prev nl@(n, l) = prev >>= \v ->
        let
          newVal :: OptionalJsonValue
          newVal = f nl v

          op :: J.Value -> J.Value
          op new = (cloneT l :: JsonTraversal) .~ new $ v
        in
          if not $ optTolerant opts then
            mapLeft (++ "\n  at path: " ++ makePath (reverse n)) $
              op <$> newVal
          else
            case newVal of
              Left l -> Right v
              Right new -> Right $ op new

-- Create multi action - action which returns multiple values
mkActionMulti ::
  (JsonNamedATraversal -> JsonAction)
  -> [JsonPathSegment]
  -> ActionMulti
mkActionMulti f xs arrayOutput opts json =
  validateActionTraversals opts (traverseJson json xs) >>=
    (result <$>) . V.foldl' acc (Right V.empty)
  where
    result :: Vector J.Value -> Vector J.Value
    result
      | arrayOutput = V.singleton . J.Array
      | otherwise = id

    acc :: OptionalJsonValues -> JsonNamedATraversal -> OptionalJsonValues
    acc vals nl = vals >>= \vs ->
      V.snoc vs <$> f nl json

actionNew :: JsonPath -> JsonValue -> Action
actionNew path rawVal opts json = case segments of
  [] -> decoded
  _ -> mkAction f (init segments) opts json
  where
    segments :: [JsonPathSegment]
    segments = splitPath path

    decoded :: OptionalJsonValue
    decoded = decodeVal rawVal

    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Object m ->
        (\val -> J.Object . Map.insert (T.pack x) val $ m) <$> decoded
      J.Array v ->
        readArrayInd x >>= \i ->
          decoded >>= \val ->
            J.Array <$> setNth i val v
      val -> Left $
        errInvalidMemberType (pathHead p) [typeArray, typeObject] val
      where
        x = last segments

actionSet :: JsonPath -> JsonValue -> Action
actionSet path rawVal = mkAction f (splitPath path)
  where
    f _ _ = decodeVal rawVal

actionKeys :: JsonPath -> Action
actionKeys path = mkAction f (splitPath path)
  where
    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Object m -> Right . J.Array . V.fromList $ J.String <$> Map.keys m
      val -> Left $
        errInvalidMemberType (pathHead p) [typeObject] val

actionValues :: JsonPath -> Action
actionValues path = mkAction f (splitPath path)
  where
    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Object m -> Right . J.Array . V.fromList $ Map.elems m
      val -> Left $
        errInvalidMemberType (pathHead p) [typeObject] val

actionLength :: JsonPath -> Action
actionLength path = mkAction f (splitPath path)
  where
    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Object m -> Right . J.Number . fromIntegral . Map.size $ m
      J.Array v -> Right . J.Number . fromIntegral . V.length $ v
      val -> Left $
        errInvalidMemberType (pathHead p) [typeArray, typeObject] val

actionReverse :: JsonPath -> Action
actionReverse path = mkAction f (splitPath path)
  where
    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Array v -> Right . J.Array . V.reverse $ v
      val -> Left $
        errInvalidMemberType (pathHead p) [typeArray] val

actionDelete :: JsonPath -> Action
actionDelete path opts = case segments of
  [] -> const . Left $ errEmptyPath
  _
    | x == "**" -> const . Left $ "last path segment can not be a '**' wildcard"
    | otherwise -> mkAction f (init segments) opts
    where
      x :: JsonPathSegment
      x = last segments

      f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
        J.Object m -> Right . J.Object $ V.foldr delObject m (keys $ J.Object m)
        J.Array v -> J.Array <$> V.foldr delArray (Right v) (keys $ J.Array v)
        val -> Left $
          errInvalidMemberType (pathHead p) [typeArray, typeObject] val

      keys :: J.Value -> Vector JsonPathSegment
      keys v = (\(x:_, _) -> x) <$> traverseJson v [x]

      delObject :: JsonPathSegment -> JsonMap -> JsonMap
      delObject x = Map.delete (T.pack x)

      delArray ::
        JsonPathSegment
        -> Either ErrMsg JsonVector
        -> Either ErrMsg JsonVector
      delArray x = (>>= \v -> deleteNth v <$> readArrayInd x)
  where
    segments :: [JsonPathSegment]
    segments = splitPath path

actionGet :: JsonPath -> ActionMulti
actionGet path = mkActionMulti f (splitPath path)
  where
    f (p, l) json = Right $ json ^?! (cloneT l :: JsonTraversal)

actionInsert :: JsonPath -> JsonValue -> Reverse -> Action
actionInsert path rawVal r = mkAction f (splitPath path)
  where
    op :: Vector a -> a -> Vector a
    op
      | r = flip V.cons
      | otherwise = V.snoc

    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Array v -> J.Array . op v <$> decodeVal rawVal
      val -> Left $ errInvalidMemberType (pathHead p) [typeArray] val

actionConcat :: JsonPath -> JsonValue -> Reverse -> Action
actionConcat path rawVal r = mkAction f (splitPath path)
  where
    op :: Vector a -> Vector a -> Vector a
    op
      | r = (V.++)
      | otherwise = flip (V.++)

    f (p, l) json = decodeVal rawVal >>= \val ->
      case val of
        J.Array appV ->
          case json ^?! (cloneT l :: JsonTraversal) of
            J.Array v -> Right . J.Array . op appV $ v
            val -> Left $ errInvalidMemberType (pathHead p) [typeArray] val
        _ -> Left $ errInvalidValType [typeArray] val

actionMerge :: JsonPath -> JsonValue -> Reverse -> Action
actionMerge path rawVal r = mkAction f (splitPath path)
  where
    op :: JsonMap -> JsonMap -> JsonMap
    op
      | r = Map.union
      | otherwise = flip Map.union

    f (p, l) json = decodeVal rawVal >>= \val ->
      case val of
        J.Object appM ->
          case json ^?! (cloneT l :: JsonTraversal) of
            J.Object m -> Right . J.Object . op appM $ m
            val -> Left $ errInvalidMemberType (pathHead p) [typeObject] val
        _ -> Left $ errInvalidValType [typeObject] val

actionPop :: JsonPath -> Int -> Reverse -> Action
actionPop path n r
  | n < 0 = actionPop path (-n) (not r)
  | otherwise = mkAction f (splitPath path)
      where
        op :: Vector a -> Vector a
        op
          | r = V.drop n
          | otherwise = \v -> V.take (V.length v - n) v

        f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
          J.Array v -> Right . J.Array . op $ v
          val -> Left $ errInvalidMemberType (pathHead p) [typeArray] val

actionTake :: JsonPath -> Int -> Int -> Reverse -> Action
actionTake path n ind r
  | n < 0 = \_ _ -> Left $ "negative N is not allowed: " ++ show n
  | ind < 0 = \_ _ -> Left $ "negative index is not allowed: " ++ show ind
  | otherwise = mkAction f (splitPath path)
      where
        op :: Vector a -> Vector a
        op v
          | ind >= len = V.empty
          | r = V.slice ind (minimum [ind + n, len - ind]) v
          | not r = V.slice
              (maximum [0, len - ind - n])
              (minimum [n, len - ind]) v
          where
            len = V.length v

        f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
          J.Array v -> Right . J.Array . op $ v
          val -> Left $ errInvalidMemberType (pathHead p) [typeArray] val


actionFilter :: JsonPath -> [JsonKeyList] -> Reverse -> Action
actionFilter path ks r = mkAction f (splitPath path)
  where
    keys :: [JsonKey]
    keys = ks >>= splitKeyList

    f (p, l) json = case json ^?! (cloneT l :: JsonTraversal) of
      J.Object m ->
        let
          keys' :: [Text]
          keys' = k $ map T.pack keys

          k
            | r = id
            | otherwise = (Map.keys m \\)
        in
          Right . J.Object $ foldl' (flip Map.delete) m keys'

      -- Strategy to perform efficient filter on array:
      -- 1. Make every vector value optional (Maybe)
      -- 2. Insert `Nothing` in place of values to be removed
      -- 3. Strip `Nothing`s from array
      J.Array v ->
        let
          (mapAllTo, mapFilteredTo) =
            (if r then swap else id) (const Nothing, Just)

          len :: Int
          len = V.length v

          changes :: Either ErrMsg [(Int, Maybe J.Value)]
          changes = foldl' accChanges (Right []) keys

          accChanges res ind = res >>= \r ->
            readArrayInd ind >>= \i ->
              if i >= len then
                Left $
                  "array index '" ++ show i ++ "' is not smaller"
                  ++ " than array length: " ++ show len
              else
                Right $ (i, mapFilteredTo (v V.! i)) : r

          -- Remove `Nothing` values from vector
          acc :: Maybe J.Value -> JsonVector -> JsonVector
          acc x v = case x of
            Nothing -> v
            Just x -> V.cons x v
        in
          (\cs ->
            J.Array . V.foldr' acc V.empty . (V.// cs) $ V.map mapAllTo v) <$>
              changes

      val -> Left $
        errInvalidMemberType (pathHead p) [typeArray, typeObject] val

{- IO -}

writeFileLn :: String -> ByteString -> IO ()
writeFileLn f = BS.writeFile f . (`BS.snoc` '\n')

readValueInput :: ValueInput -> IO ByteString
readValueInput x = case x of
  StdInputValue -> BS.getContents
  Raw v -> return $ BS.pack v

readInput :: Input -> IO ByteString
readInput x = case x of
  StdInput -> BS.getContents
  FileInput f -> BS.readFile f
  FileInPlace f -> BS.readFile f

runOptions :: Options -> IO ()
runOptions (Options cmd inp opts format) = case cmd of
  Set     p vi      -> goV "set"    vi $ \v -> actionSet     p v opts
  New     p vi      -> goV "new"    vi $ \v -> actionNew     p v opts
  Insert  p vi r    -> goV "insert" vi $ \v -> actionInsert  p v r opts
  Concat  p vi r    -> goV "concat" vi $ \v -> actionConcat  p v r opts
  Merge   p vi r    -> goV "merge"  vi $ \v -> actionMerge   p v r opts
  Get     p a       -> goM "get"             $ actionGet     p a opts
  Delete  p         -> go  "delete"          $ actionDelete  p opts
  Pop     p n r     -> go  "pop"             $ actionPop     p n r opts
  Take    p n s r   -> go  "take"            $ actionTake    p n s r opts
  Filter  p ks r    -> go  "filter"          $ actionFilter  p ks r opts
  Values  p         -> go  "values"          $ actionValues  p opts
  Keys    p         -> go  "keys"            $ actionKeys    p opts
  Length  p         -> go  "length"          $ actionLength  p opts
  Reverse p         -> go  "reverse"         $ actionReverse p opts

  where
    encode :: Either ErrMsg JsonEncoder
    encode = prettyEncode format

    encodeMany = V.map <$> encode

    input :: IO OptionalJsonValue
    input = mapLeft (const "invalid JSON input") . J.eitherDecode <$>
      readInput inp

    -- Run action with respect to input type
    go :: CommandTitle -> JsonAction -> IO ()
    go t a = input >>= f . actionErrHeader t . (>>= a)
      where
        f :: OptionalJsonValue -> IO ()
        f x = case encode <*> x of
          Left err -> putErr err
          Right j -> output j

        output :: ByteString -> IO ()
        output = case inp of
          FileInPlace f -> writeFileLn f
          _ -> BS.putStrLn

    -- Run action with value input
    goV :: CommandTitle -> ValueInput -> (JsonValue -> JsonAction) -> IO ()
    goV t vi f = readValueInput vi >>= go t . f

    -- Run multi action with respect to input type
    goM :: CommandTitle -> JsonActionMulti -> IO ()
    goM t a = input >>= f . actionErrHeader t . (>>= a)
      where
        f :: OptionalJsonValues -> IO ()
        f xs = case encodeMany <*> xs of
          (Left err) -> putErr err
          (Right js) -> case inp of
            FileInPlace f
              | V.length js == 1 -> writeFileLn f . V.head $ js
              | otherwise -> putErr . actionHeader t $
                "in-place edit (-i): there must be exactly 1 output"
                ++ ", meanwhile there are: " ++ show (V.length js)
            _ -> V.foldl'
              (\acc j -> acc >> BS.putStrLn j) (return ()) js

jsn :: IO ()
jsn = execParser i >>= runOptions
  where
    i = info (helper <*> parseOptions) $ mconcat
      [ progDesc "jsn - simple JSON manipulations" ]
