{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
import Control.Monad (zipWithM_)
import Data.ByteString.Char8 (ByteString, pack)
import Database.PostgreSQL.LibPQ

testStrings :: [ByteString]
testStrings =
    [ ""
    , "j"
    , "\t"
    , "\n"
    , "\\N"
    , "\\\\N"
    , "\r\n"
    , "\25"
    , "\31\31\127"
    , pack ['\1'..'\127']
    ]

binaryTestStrings :: [ByteString]
binaryTestStrings =
    testStrings ++
    [ "\0"
    , "\0\0"
    , pack ['\0'..'\255']
    ]

withCopyFrom :: Connection
             -> ByteString
             -> (([Maybe (ByteString, Format)] -> IO ()) -> IO a)
             -> IO a
withCopyFrom conn query inner = do
    Just result <- exec conn query
    CopyIn <- resultStatus result

    r <- inner $ \row -> do
        CopyOk <- putCopyRow conn row
        return ()

    CopyOk <- putCopyEnd conn Nothing

    Just result <- getResult conn
    CommandOk <- resultStatus result

    return r

showInt :: Int -> ByteString
showInt = pack . show

main :: IO ()
main = do
    conn <- connectdb ""

    -- CREATE TABLE test_text (id INT, content TEXT);
    withCopyFrom conn "COPY test_text (id, content) FROM stdin"
        $ \putRow -> do
            putRow [Nothing, Nothing]
            putRow [Just ("0", Text), Nothing]
            let f n s = putRow [Just (showInt n, Text), Just (s, Text)]
             in zipWithM_ f [1..] testStrings

    -- CREATE TABLE test_binary (id INT, content BYTEA);
    withCopyFrom conn "COPY test_binary (id, content) FROM stdin"
        $ \putRow -> do
            putRow [Nothing, Nothing]
            putRow [Just ("0", Text), Nothing]
            let f n s = putRow [Just (showInt n, Text), Just (s, Binary)]
             in zipWithM_ f [1..] binaryTestStrings

    finish conn
