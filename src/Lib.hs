{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib (logMaster) where

import           Codec.Compression.Zlib
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import           Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.IO               as T
import           System.Console.ANSI
import           System.Directory
import           Text.Megaparsec            as P

type Hash = Text

data Object = Commit
    { objectId :: Hash
    , tree     :: Hash
    , parents  :: [Hash]
    , headers  :: LBS.ByteString
    , message  :: LBS.ByteString
    } deriving (Show, Eq)

------------------------------------------------------------

hashParser :: Parsec ByteString Hash
hashParser = T.pack <$> P.count 40 hexDigitChar

taggedHashParser :: String -> Parsec ByteString Hash
taggedHashParser tag =
    (do _ <- string tag
        _ <- string " "
        hash <- hashParser
        _ <- string "\n"
        return hash) <?>
    tag

objectParser :: Hash -> Parsec ByteString Object
objectParser objectId = do
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

getObject :: Hash -> IO (Either ParseError Object)
getObject hash = parse (objectParser hash) (T.unpack hash) <$> readHash hash

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
        , T.take 2 hash
        , "/"
        , T.drop 2 hash]

pathForBranch :: Text -> FilePath -> FilePath
pathForBranch branchName currentDirectory =
    T.unpack $ mconcat [T.pack currentDirectory, "/.git/refs/heads/", branchName]

hashForBranch :: Text -> IO Hash
hashForBranch branchName = do
    currentDirectory <- getCurrentDirectory
    let branchPath = pathForBranch branchName currentDirectory
    print branchPath
    decodeUtf8 . LBS.toStrict . LBS.take 40 <$> LBS.readFile branchPath

------------------------------------------------------------

printObject :: Object -> IO ()
printObject object = do
    setSGR [SetColor Foreground Vivid Blue]
    T.putStrLn (objectId object)
    setSGR [Reset]
    LBS.putStrLn (message object)

gitLog :: Hash -> IO ()
gitLog hash = do
    result <- getObject hash
    case result of
        Left err -> print err
        Right object -> do
            printObject object
            case listToMaybe (parents object) of
                Just parent -> gitLog parent
                Nothing -> do
                    setSGR [SetColor Foreground Vivid Red]
                    Prelude.putStrLn "Done."
                    setSGR [Reset]

------------------------------------------------------------

logMaster :: IO ()
logMaster = hashForBranch "master" >>= gitLog
