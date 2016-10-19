{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib where

import           Codec.Compression.Zlib
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import           Data.Text                  as T
import           Data.Text.IO               as T
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
master = "3ae4528c3039e366bc2577f28187f6fb128374fa"

readHash :: Hash -> IO LBS.ByteString
readHash hash = decompress <$> (LBS.readFile (pathForHash hash))

parseHash :: Hash -> IO ()
parseHash hash = do
    contents <- readHash hash
    let maybeCommit = parse objectParser (T.unpack hash) contents
    case maybeCommit of
        Left err -> print err
        Right commit -> do
            T.putStrLn hash
            LBS.putStrLn (message commit)
            case listToMaybe (parents commit) of
                Nothing -> Prelude.putStrLn "Done."
                Just parent -> parseHash parent

pathForHash :: Hash -> FilePath
pathForHash hash =
  T.unpack $
        mconcat
            [ "/Users/kris/Work/WLHN/gitparse/.git/objects/"
            , T.take 2 hash
            , "/"
            , T.drop 2 hash]

someFunc :: IO ()
someFunc = parseHash master
