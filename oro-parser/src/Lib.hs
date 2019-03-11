{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( normalize
    , normalizeWithConfig
    , config
    , NormalizerConfig (..)
    ) where

import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad.RWS.Lazy
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Lazy as S
import Data.Char (chr, ord)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Numeric (readHex)
import Text.Printf (printf)

data HandleType = ScalarHandle
                | SequenceHandle
                | MappingHandle
    deriving (Eq)

data NodeHandle = NodeHandle
    { htype :: HandleType
    , nodeId :: Integer
    }

instance Eq NodeHandle where
    NodeHandle{nodeId=a} == NodeHandle{nodeId=b} = a == b

instance Ord NodeHandle where
    compare NodeHandle{nodeId=a} NodeHandle{nodeId=b} = compare a b

data NormalizerConfig = NormalizerConfig
    { ncBreakdown :: Bool
    }

data NormalizerState a = NormalizerState
    { _nsTokensLeft :: [a]
    , _nsTagShortcuts :: M.Map String String
    , _nsAnchorMapping :: M.Map String Int
    , _nsAnchorsSeen :: Int
    , _nsNodeTag :: String
    , _nsInScalar :: Bool
    , _nsClosingParens :: [Int]
    , _nsJoiningOps :: [NormalizerOp a]
    , _nsBeforeNestedNode :: NormalizerOp a
    , _nsAccumulatedText :: String
    }

type NormalizerFun a = RWS NormalizerConfig String (NormalizerState a)
type NormalizerOp a = NormalizerFun a ()

data TokenResult    = DocumentComplete
                    | ConsumedTokens Int

makeLenses ''NormalizerState

config :: NormalizerConfig
config = NormalizerConfig
    { ncBreakdown = False
    }

initialNormalizerState :: (Show a) => [a] -> NormalizerState a
initialNormalizerState tokens = NormalizerState
    { _nsTokensLeft = tokens
    , _nsTagShortcuts = M.empty
    , _nsAnchorMapping = M.empty
    , _nsAnchorsSeen = 0
    , _nsNodeTag = "?"
    , _nsInScalar = False
    , _nsClosingParens = []
    , _nsJoiningOps = [(noJoining)]
    , _nsBeforeNestedNode = (return ())
    , _nsAccumulatedText = ""
    }

normalize :: (Show a) => [a] -> (String ,[a])
normalize tokens = normalizeWithConfig config tokens

normalizeWithConfig :: (Show a) => NormalizerConfig -> [a] -> (String, [a])
normalizeWithConfig cfg tokens = let
    (state', output) = execRWS processTokens (cfg) (initialNormalizerState tokens)
    in (output, state' ^. nsTokensLeft)
    where
        processTokens :: (Show a) => NormalizerOp a
        processTokens = do
            tt <- use nsTokensLeft
            tr <- processToken tt
            case tr of
                DocumentComplete -> do
                    dropL 1 nsTokensLeft
                    return ()
                ConsumedTokens i -> do
                    dropL i nsTokensLeft
                    processTokens

processToken :: (Show a) => [a] -> NormalizerFun a TokenResult
processToken [] = return DocumentComplete
processToken tt@(t:ts) = do
    whenDoing ncBreakdown $ tell $ printf "\n%c: " (yeastCode t)

    case yeastCode t of
        -- EndDocument
        'o' -> return DocumentComplete

        -- BeginDirective
        'D' -> processDirective

        -- BeginNode
        'N' -> do
            beforeAction <- use nsBeforeNestedNode
            beforeAction
            nsNodeTag .= "?"
            nsInScalar .= False
            pushL 0 nsClosingParens
            return $ ConsumedTokens 1

        -- BeginAnchor
        'A' -> do
            let (_:nt:_) = ts
            let n = yeastContent nt
            anchorId <- nsAnchorsSeen <+= 1
            nsAnchorMapping .at n ?= anchorId
            tell "`&__`(#token(\""
            tell $ show anchorId
            tell "\",\"Int\"),"
            incrementClosingParens
            return $ ConsumedTokens 2

        -- BeginTag
        'G' -> collectNodeTag tt

        -- BeginAlias
        'R' -> do
            -- Token sequence: RItr
            let name = yeastContent (tt !! 2)
            v <- nsAnchorMapping #. at name
            case v of
                Nothing -> fail $ "Undefined anchor reference *" ++ name
                Just refNum -> do
                    tell "`*_`(#token(\""
                    tell $ show refNum
                    tell "\",\"Int\"))"

            return $ ConsumedTokens 4

        -- BeginScalar
        'S' -> do
            nsInScalar .= True
            nsAccumulatedText .= ""
            -- Don't tellNodeStart here because an 'I' token might change the tag
            return $ ConsumedTokens 1

        -- Indicator (only important during scalar, indicates non-plain)
        'I' -> do
            inScalar <- use nsInScalar
            nodeTag <- use nsNodeTag
            when (inScalar && nodeTag == "?") $ nsNodeTag .= "!"
            return $ ConsumedTokens 1

        -- Text content
        'T' -> do
            captureText t
            return $ ConsumedTokens 1

        -- Escape sequence
        'E' -> do
            captureEscapedText tt

        -- Linebreak in content (represents linebreak)
        'L' -> do
            nsAccumulatedText <>= "\n"
            return $ ConsumedTokens 1

        -- Linebreak in content (represents space)
        'l' -> do
            nsAccumulatedText <>= " "
            return $ ConsumedTokens 1

        -- EndScalar
        's' -> do
            -- Tell node start here because we can't know the tag until we see
            -- the text, but it could start with escaped text or have multiple
            -- texts, but there will be only on EndScalar
            tellNodeStart '.'
            incrementClosingParens
            tellScalarValue
            return $ ConsumedTokens 1

        -- BeginSequence
        'Q' -> do
            openSequence
            return $ ConsumedTokens 1

        -- EndSequence
        'q' -> do
            closeContainer ']'
            return $ ConsumedTokens 1

        -- BeginMapping
        'M' -> do
            openMapping
            return $ ConsumedTokens 1

        -- EndMapping
        'm' -> do
            closeContainer '}'
            return $ ConsumedTokens 1

        -- Error
        '!' -> fail $ yeastContent t

        -- EndNode
        'n' -> do
            closeNode
            joiningOp <- popL1 nsJoiningOps
            joiningOp
            return $ ConsumedTokens 1

        -- Otherwise
        _ -> return $ ConsumedTokens 1

processDirective :: (Show a) => NormalizerFun a TokenResult
processDirective  = do
    (tt, name, pp) <- directiveParts
    case name of
        "YAML" -> processYamlDirective pp
        "TAG" -> processTagDirective pp
        otherwise -> fail $ "Unknown directive %" ++ name

    case L.findIndex (hasYeastCode 'd') tt of
        Just i -> return $ ConsumedTokens i
        Nothing -> fail $ "Unterminated directive"

directiveParts :: (Show a) => NormalizerFun a ([a], String, [a])
directiveParts  = do
    tt <- use nsTokensLeft
    case tt of
        (_:nt:_:pp) -> return $ (tt, yeastContent nt, pp)

processYamlDirective :: (Show a) => [a] -> NormalizerOp a
processYamlDirective (vt:et:_) = case yeastCode et of
    'd' -> do
        let verString = yeastContent vt
        let (major:minor:[]) = map read $ splitOn "." verString
        if major > 1
            then fail $ "Cannot process YAML " ++ verString ++ " content"
            else return ()
    otherwise -> fail $ "Too many parameters to %YAML directive"

processTagDirective :: (Show a) => [a] -> NormalizerOp a
processTagDirective (ht:_:vt:et:_) = case yeastCode et of
    'd' -> do
        nsTagShortcuts . at (yeastContent ht) ?= (yeastContent vt)
    otherwise -> fail $ "Wrong number of parameters to %TAG directive"

collectNodeTag :: (Show a) => [a] -> NormalizerFun a TokenResult
collectNodeTag tt
    -- If it is tagged "!"
    | isTokenSeq "GIg" tt = do
        -- Safe match: ! tag is defaulted
        tvm <- nsTagShortcuts #. at "!"
        captureTag $ fromJust tvm
        dropTagTokens

    -- If it is tagged "!!"
    | isTokenSeq "GHIIhtg" tt = do
        -- Safe match: !! tag is defaulted
        tvm <- nsTagShortcuts #. at "!!"
        captureTag $ fromJust tvm
        dropTagTokens

    -- If it is taggeed "!something"
    | isTokenSeq "GHIhtg" tt = do
        captureTag ('!':yeastContent (tt !! 4))
        dropTagTokens

    -- If it is tagged "!<tag:example.com,2019:something-else>" of "!<!foo>"
    | isTokenSeq "GIItIg" tt = do
        captureTag (yeastContent (tt !! 3))
        dropTagTokens

    -- If it is tagged "!l!other"
    | isTokenSeq "GHItIhtg" tt = do
        let th = tagHandleFromToken (tt !! 3)
        mpv <- nsTagShortcuts #. at th
        case mpv of
            Just pv -> do
                captureTag $ pv ++ (yeastContent (tt !! 6))
                dropTagTokens
            Nothing -> fail $ "Undefined tag handle " ++ th

    where
        dropTagTokens :: (Show a) => NormalizerFun a TokenResult
        dropTagTokens  = do
            tt <- use nsTokensLeft
            case L.findIndex (hasYeastCode 'g') tt of
                Just i -> return $ ConsumedTokens i
                Nothing -> fail $ "Unterminated tag"

        tagHandleFromToken t = ('!':yeastContent t ++ "!")

        testTvm :: Maybe String -> NormalizerOp a
        testTvm _ = return ()

captureTag :: String -> NormalizerOp a
captureTag s = nsNodeTag .= s

captureText :: (Show a) => a -> NormalizerOp a
captureText t = do
    let chunk = unescapeText $ yeastContent t
    nsAccumulatedText <>= chunk

specificCharacterEscapes = M.fromList
    [ ('0', '\0')
    , ('a', '\x07')
    , ('b', '\x08')
    , ('t', '\x09')
    , ('\x09', '\x09')
    , ('n', '\x0A')
    , ('v', '\x0B')
    , ('f', '\x0C')
    , ('r', '\x0D')
    , ('e', '\x1B')
    , (' ', ' ')
    , ('"', '"')
    , ('/', '/')
    , ('\\', '\\')
    , ('N', '\x85')
    , ('_', '\xA0')
    , ('L', '\x2028')
    , ('P', '\x2029')
    ]

captureEscapedText :: (Show a) => [a] -> NormalizerFun a TokenResult
captureEscapedText tt
    -- For a specific character escape
    | isTokenSeq "EIte" tt = do
        let (Just ch) = M.lookup (head $ unescapeText $ yeastContent (tt !!2)) specificCharacterEscapes
        nsAccumulatedText <>= [ch]
        return $ ConsumedTokens 4

    -- For a coded character escape
    | isTokenSeq "EIIte" tt = do
        let [(codePoint, "")] = readHex $ yeastContent (tt !! 3)
        nsAccumulatedText <>= [chr codePoint]
        return $ ConsumedTokens 5

noJoining :: (Show a) => NormalizerOp a
noJoining  = return ()

incrementClosingParens :: (Show a) => NormalizerOp a
incrementClosingParens  = do
    nsClosingParens %= \(p:ps) -> (p + 1:ps)

tellNodeStart :: (Show a) => Char -> NormalizerOp a
tellNodeStart nodeIndicator = do
    case nodeIndicator of
        '.' -> tell "`.__`("
        '[' -> tell "`[__]`("
        '{' -> tell "`{__}`("
    tag <- use nsNodeTag
    tell "#token("
    tell $ stringLiteral ("\"" ++ tag ++ "\"")
    tell ",\"String\"),"

tellScalarValue :: (Show a) => NormalizerOp a
tellScalarValue  = do
    value <- use nsAccumulatedText
    tell "#token("
    tell $ stringLiteral $ stringLiteral value
    tell ",\"String\")"

closeNode :: (Show a) => NormalizerOp a
closeNode  = do
        p <- popL1 nsClosingParens
        tell $ replicate p ')'

openSequence :: (Show a) => NormalizerOp a
openSequence  = do
    tellNodeStart '['
    prepareForSequenceItem
    pushL (joinItems) nsJoiningOps

joinItems :: (Show a) => NormalizerOp a
joinItems  = do
    tell ","
    prepareForSequenceItem
    -- Put this joiner back on the stack to handle the next sequence item
    pushL (joinItems) nsJoiningOps

prepareForSequenceItem :: (Show a) => NormalizerOp a
prepareForSequenceItem  = do
    nsBeforeNestedNode .= (tell "`_,_`(")
    incrementClosingParens

openMapping :: (Show a) => NormalizerOp a
openMapping  = do
    tellNodeStart '{'
    prepareForMappingEntry
    pushL (joinKeyToValue) nsJoiningOps

joinKeyValuePairs :: (Show a) => NormalizerOp a
joinKeyValuePairs = do
    tell "),"
    prepareForMappingEntry
    pushL (joinKeyToValue) nsJoiningOps

prepareForMappingEntry :: (Show a) => NormalizerOp a
prepareForMappingEntry  = do
    nsBeforeNestedNode .= (tell "`_,_`(`_:_`(")
    incrementClosingParens

joinKeyToValue :: (Show a) => NormalizerOp a
joinKeyToValue  = do
    tell ","
    nsBeforeNestedNode .= (return ())
    pushL (joinKeyValuePairs) nsJoiningOps

closeContainer :: (Show a) => Char -> NormalizerOp a
closeContainer _ = do
    tell "`.List{\"_,_\"}`(.KList)"
    dropL 1 nsJoiningOps

stringLiteral :: String -> String
stringLiteral s = "\"" ++ (escapedContent s) ++ "\""
    where
        escapedContent :: String -> String
        escapedContent [] = ""
        escapedContent (c:cs)
            | c `elem` "\"\\"               =
                ('\\':c:escapedContent cs)
            | ordC >= 32 && ordC <= 127     =
                (c:escapedContent cs)
            | ordC >= 128 && ordC <= 0xffff =
                (printf "\\u%04X" ordC) ++ (escapedContent cs)
            | otherwise                     =
                (printf "\\U%08X" ordC) ++ (escapedContent cs)
            where
                ordC :: Int
                ordC = ord c


--- General Lens Operations ---

pushL :: (S.MonadState s m) => a -> ASetter' s [a] -> m ()
pushL v l = l %= (v:)

popL1 :: (S.MonadState s m) => Over' (->) ((,) a) s [a] -> m a
popL1 l = l %%= \(x:xs) -> (x,xs)

dropL :: (S.MonadState s m) => Int -> ALens s s [a] [a] -> m ()
dropL n l = l #%= drop n

(#.) :: S.MonadState s m => (b -> s -> Const a s) -> ((a -> Const a a ) -> b) -> m a
l #. f = use (l . f)
infixl 8 #.

--- Monad Operations ---

whenDoing :: (Show b) => (NormalizerConfig -> Bool) -> NormalizerOp b -> NormalizerOp b
whenDoing getInMode op = do
    inMode <- asks getInMode
    when inMode op

--- Yeast Functions ---

yeastData :: (Show a) => a -> String
yeastData t = (lines $ show t) !! 1

yeastCode :: (Show a) => a -> Char
yeastCode t = let
    (c:_) = yeastData t
    in c

hasYeastCode :: (Show a) => Char -> a -> Bool
hasYeastCode c t = yeastCode t == c

yeastContent :: (Show a) => a -> String
yeastContent t = tail $ yeastData t

isTokenSeq :: (Show a) => [Char] -> [a] -> Bool
isTokenSeq [] _ = True
isTokenSeq (c:cs) (t:ts)
    | yeastCode t == c  = isTokenSeq cs ts
    | otherwise         = False

unescapeText :: String -> String
unescapeText "" = ""
unescapeText ('\\':'x':h1:h2:xs) = let
    [(codePoint,"")] = readHex (h1:h2:"")
    in (chr codePoint:unescapeText xs)
unescapeText ('\\':'u':xs) = let
    (cc,yy) = splitAt 4 xs
    [(codePoint,"")] = readHex cc
    in (chr codePoint:unescapeText yy)
unescapeText ('\\':'U':xs) = let
    (cc,yy) = splitAt 8 xs
    [(codePoint,"")] = readHex cc
    in (chr codePoint:unescapeText yy)
unescapeText ('\\':c:xs) =
    (c:unescapeText xs)
unescapeText (c:xs) =
    (c:unescapeText xs)
