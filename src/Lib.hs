{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib where

import           Codec.Compression.Zlib
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  as T
import           Text.Megaparsec            as P


type Hash = Text

data Object = Commit
    { tree    :: Hash
    , parents :: [Hash]
    , headers :: LBS.ByteString
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
    tree <- taggedHashParser "tree"
    parents <- many (taggedHashParser "parent")
    headers <- LBS.pack <$> (manyTill anyChar (string "\n\n"))
    message <- LBS.pack <$> (many anyChar <* eof)
    return
        (Commit
         { ..
         }) -- TODO

master :: Hash
--master = "9107febd65c3eb17eee16628fbd748d545b463a1"
master = "7d10b9ae7f44a57b286ec2a69a33b0ba0d244cbc"

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
