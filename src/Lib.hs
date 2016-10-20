{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( logMaster
  ) where

import           Codec.Compression.Zlib
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.IO               as T
import           System.Console.ANSI
import           System.Directory
import           Text.Megaparsec            as P

newtype Hash = Hash
    { unHash :: Text
    } deriving (Show, Eq)

data Object = Commit
    { objectId :: Hash
    , tree     :: Hash
    , parents  :: [Hash]
    , headers  :: LBS.ByteString
    , message  :: LBS.ByteString
    } deriving (Show, Eq)

------------------------------------------------------------
hashParser
    :: Monad m
    => ParsecT ByteString m Hash
hashParser = Hash . T.pack <$> P.count 40 hexDigitChar

taggedHashParser
    :: Monad m
    => String -> ParsecT ByteString m Hash
taggedHashParser tag =
    (do _ <- string tag
        _ <- string " "
        hash <- hashParser
        _ <- string "\n"
        return hash) <?>
    tag

objectParser
    :: Monad m
    => Hash -> ParsecT ByteString m Object
objectParser objectId = do
    _ <- string "commit "
    _ <- some digitChar
    _ <- string "\000"
    tree <- taggedHashParser "tree"
    parents <- many (taggedHashParser "parent")
    headers <- LBS.pack <$> manyTill anyChar (string "\n\n")
    message <- LBS.pack <$> many anyChar <* eof
    return
        Commit
        { ..
        }

getObject :: Hash -> IO (Either ParseError Object)
getObject hash = readHash hash >>= runParserT (objectParser hash) (show hash)

------------------------------------------------------------
readHash :: Hash -> IO LBS.ByteString
readHash hash = do
    currentDirectory <- getCurrentDirectory
    decompress <$> LBS.readFile (pathForHash hash currentDirectory)

pathForHash :: Hash -> FilePath -> FilePath
pathForHash hash currentDirectory =
    T.unpack $
    mconcat
        [ T.pack currentDirectory
        , "/.git/objects/"
        , T.take 2 (unHash hash)
        , "/"
        , T.drop 2 (unHash hash)]

pathForBranch :: Text -> FilePath -> FilePath
pathForBranch branchName currentDirectory =
    T.unpack $
    mconcat [T.pack currentDirectory, "/.git/refs/heads/", branchName]

hashForBranch :: Text -> IO Hash
hashForBranch branchName = do
    currentDirectory <- getCurrentDirectory
    let branchPath = pathForBranch branchName currentDirectory
    Hash . decodeUtf8 . LBS.toStrict . LBS.take 40 <$> LBS.readFile branchPath

------------------------------------------------------------
red :: Text -> IO ()
red msg = do
    setSGR [SetColor Foreground Vivid Blue]
    T.putStrLn msg
    setSGR [Reset]

blue :: Text -> IO ()
blue msg = do
    setSGR [SetColor Foreground Vivid Red]
    T.putStrLn msg
    setSGR [Reset]

printObject :: Object -> IO ()
printObject object = do
    red . unHash $ objectId object
    LBS.putStrLn $ message object

gitLog :: Hash -> IO ()
gitLog hash = do
    result <- getObject hash
    case result of
        Left err -> print err
        Right object -> do
            printObject object
            case listToMaybe (parents object) of
                Just parent -> gitLog parent
                Nothing -> blue "Done."

------------------------------------------------------------
logMaster :: IO ()
logMaster = hashForBranch "master" >>= gitLog
