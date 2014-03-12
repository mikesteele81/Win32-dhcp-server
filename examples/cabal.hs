-- This is the example used in the main .cabal file.

{-# LANGUAGE OverloadedStrings #-}

-- Print all MAC addresses with an active client lease
module Main where

import Data.Ip
import Data.Mac
import qualified Data.Text.IO as T
import System.Win32.DHCP

main :: IO ()
main = do
    api <- loadDHCP
    clients <- enumClients api context
    let macs = map (showMac ":" . clientHardwareAddress) clients
    mapM_ T.putStrLn macs
 where
    Right subnet = readIp "192.168.1.0"
    context = Context "192.168.1.5" subnet

