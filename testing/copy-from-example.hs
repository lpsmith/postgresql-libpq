{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
import Control.Monad (forM_)
import Database.PostgreSQL.LibPQ

main :: IO ()
main = do
    conn <- connectdb ""

    -- Create a temporary table for testing
    Just result <- exec conn "CREATE TEMPORARY TABLE foo (a INT, b TEXT)"
    CommandOk <- resultStatus result

    -- Put the connection in the COPY_IN state
    -- by executing a COPY ... FROM query.
    Just result <- exec conn "COPY foo (a, b) FROM stdin"
    CopyIn <- resultStatus result

    -- Send a couple lines of COPY data
    CopyOk <- putCopyRow conn [Just ("1", Text), Just ("one", Text)]
    CopyOk <- putCopyRow conn [Just ("2", Text), Nothing]

    -- Send end-of-data indication
    CopyOk <- putCopyEnd conn Nothing

    -- Get the final result status of the copy command.
    Just result <- getResult conn
    CommandOk <- resultStatus result

    -- Retrieve the rows and print them
    Just result <- exec conn "SELECT * FROM foo"
    TuplesOk <- resultStatus result
    n <- ntuples result
    forM_ [0..n-1] $ \i -> do
        c0 <- getvalue result i 0
        c1 <- getvalue result i 1
        putStrLn $ show c0 ++ "\t" ++ show c1

    finish conn
