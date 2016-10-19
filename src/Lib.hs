{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Codec.Compression.Zlib
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  as T
import           Text.Megaparsec            as P


type Hash = Text

data Object = Commit
    { tree    :: Hash
    , parent  :: [Hash]
    , message :: LBS.ByteString
    } deriving (Show, Eq)

hashParser :: Parsec ByteString Hash
hashParser = T.pack <$> P.count 40 hexDigitChar

taggedHashParser :: String -> Parsec ByteString Hash
taggedHashParser tag = (do
    _ <- string tag
    _ <- string " "
    hash <- hashParser
    _ <- string "\n"
    return hash) <?> tag

objectParser :: Parsec ByteString Object
objectParser = do
    _ <- string "commit "
    _ <- some digitChar
    _ <- string "\000"
    treeHash <- taggedHashParser "tree"
    parents <- many (taggedHashParser "parent")
    rest <- LBS.pack <$> (many anyChar <* eof)
    return (Commit treeHash parents rest) -- TODO

master :: Hash
master = "9107febd65c3eb17eee16628fbd748d545b463a1"

readHash :: Hash -> IO LBS.ByteString
readHash hash = decompress <$> (LBS.readFile (pathForHash hash))

parseHash :: Hash -> IO (Either ParseError Object)
parseHash hash = do
    contents <- readHash hash
    return (parse objectParser (T.unpack hash) contents)

pathForHash :: Hash -> FilePath
pathForHash hash =
  T.unpack $
        mconcat
            [ "/Users/kris/Work/WLHN/gitparse/.git/objects/"
            , T.take 2 hash
            , "/"
            , T.drop 2 hash]

someFunc :: IO ()
someFunc = parseHash master >>= print
