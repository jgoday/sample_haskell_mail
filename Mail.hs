module Mail (
    mailConnect, mailSend,
    ServerConfig(Server, host, port)
) where

import Data.IORef

import Network.SMTP.ClientSession
import Network.SMTP.Client
import Network.Socket

import System.Time
import System.IO


data ServerConfig = Server {
    host :: String,
    port :: Int
} deriving (Show)


mailConnect host =
    do let (Server hostValue portValue) = host
       addrs <- getAddrInfo Nothing (Just hostValue) Nothing
       let SockAddrInet _ hostAddr = addrAddress (addrs !! 0)
       let sockAddr = SockAddrInet (fromIntegral portValue) hostAddr
       return sockAddr

mailSend myDomain from to subject body server = do
    now <- getClockTime
    nowCT <- toCalendarTime now
    let to_list = map (\t -> (NameAddr (Just "") t)) (filter (/= "") to)
    let message = Message [
                From [NameAddr (Just "") from],
                To   to_list,
                Subject subject,
                Date nowCT
            ] (body)
    sentRef <- newIORef []
    sendSMTP' (hPutStrLn stderr) (Just sentRef) myDomain
        server [message]
    statuses <- readIORef sentRef
    let res = case head statuses of
                Nothing     -> True
                Just status -> False
    return res


