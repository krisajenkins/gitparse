{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib where

import           Codec.Compression.Zlib
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.IO               as T
import           System.Console.ANSI
import           Text.Megaparsec            as P

projectPath :: Text
projectPath = "/Users/kris/Work/WLHN/gitparse/"

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
         })

readHash :: Hash -> IO LBS.ByteString
readHash hash = decompress <$> (LBS.readFile (pathForHash hash))

gitLog :: Hash -> IO ()
gitLog hash = do
    contents <- readHash hash
    let maybeCommit = parse objectParser (T.unpack hash) contents
    case maybeCommit of
        Left err -> print err
        Right commit -> do
            setSGR [SetColor Foreground Vivid Blue]
            T.putStrLn hash
            setSGR [Reset]
            LBS.putStrLn (message commit)
            case listToMaybe (parents commit) of
                Nothing -> do
                    setSGR [SetColor Foreground Vivid Red]
                    Prelude.putStrLn "Done."
                    setSGR [Reset]
                Just parent -> gitLog parent

pathForHash :: Hash -> FilePath
pathForHash hash =
    T.unpack $
    mconcat [projectPath, ".git/objects/", T.take 2 hash, "/", T.drop 2 hash]

pathForBranch :: Text -> FilePath
pathForBranch branchName =
    T.unpack $
    mconcat [projectPath, ".git/refs/heads/", branchName]

hashForBranch :: Text -> IO Hash
hashForBranch branchName =
    decodeUtf8 . LBS.toStrict . LBS.take 40 <$>
    LBS.readFile (pathForBranch branchName)

someFunc :: IO ()
someFunc = hashForBranch "master" >>= gitLog
