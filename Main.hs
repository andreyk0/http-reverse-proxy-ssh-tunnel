{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.List
import Data.Monoid
import Data.Streaming.Network
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Socket (setSocketOption, sClose, SocketOption(ReuseAddr))
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.CmdArgs as CA
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.IORef as I
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HC
import qualified System.Posix.Process as SPP
import qualified System.Process as SP


data Opts =
  Opts { listen :: Int
       , gateway:: String
       } deriving(Show, Data, Typeable)

cmdLineOpts :: Opts
cmdLineOpts =
  Opts { listen = 8080 &= help "local port to bind to, defaults to 8080"
       , gateway = "" &= help "SSH gateway host"
       } &= program "http-reverse-proxy-ssh-tunnel" &=
                summary "A quick SSH tunnel hack for proxying HTTP service calls to another environment." &=
                help "Usage: http-reverse-proxy-ssh-tunnel [--listen 8080] --gateway my.gateway.host" &=
                details [ "Source: https://github.com/andreyk0/http-reverse-proxy-ssh-tunnel.git"
                        , ""
                        , "Proxies calls like 'http://svc-something:LISTEN_PORT/' to another environment via SSH gateway."
                        , ""
                        , "This allows use of canonical service URLs in development environment."
                        , "E.g. settings listed below should make 'http://svc-something/' work on local host."
                        , ""
                        , "Parses /etc/hosts, looking for entries below this comment"
                        , ""
                        , "# http-reverse-proxy-ssh-tunnel"
                        , ""
                        , "127.0.0.1 svc-something"
                        , "127.0.0.1 svc-something-else"
                        , ""
                        , "On a Mac you can run it on a non-privileged port and forward port 80 connections to it with"
                        , "$ sudo ipfw add 100 fwd '127.0.0.1,8080' tcp from 127.0.0.1 to any 80 in"
                        , "$ sudo ipfw show"
                        , ""
                        ]


nextPort :: I.IORef Int
nextPort = unsafePerformIO $ I.newIORef 15452
{-# NOINLINE nextPort #-}

getFreePort :: IO Int
getFreePort = do
    port <- I.atomicModifyIORef nextPort $ \p -> (p + 1, p)
    esocket <- try $ bindPortTCP port "*4"
    case esocket of
        Left (_ :: IOException) -> getFreePort
        Right socket -> do
            setSocketOption socket ReuseAddr 1
            sClose socket
            return port


{-
 Parses /etc/hosts, looking for entries below this comment

# http-reverse-proxy-ssh-tunnel

127.0.0.1 svc-something
127.0.0.1 svc-something-else

-}
getServiceNamesToProxy :: IO [T.Text]
getServiceNamesToProxy = do
  etcHosts <- TIO.readFile "/etc/hosts"
  let needle = "# http-reverse-proxy-ssh-tunnel"
  let (_, svcLines) = T.breakOn needle etcHosts
  let nonEmptySvcLines = filter (not . T.null) $ filter (not . T.isPrefixOf "#") $ map (T.strip) $ T.lines svcLines
  let svcNames = map ( last . (T.splitOn " ") . T.strip  ) nonEmptySvcLines
  if (null svcNames)
    then error $ "Expected to find " <> (T.unpack needle) <> " in /etc/hosts, followed by\n\
                                                  \127.0.0.1 svc-something\n\
                                                  \127.0.0.1 svc-or-other\n"
    else do TIO.putStrLn $ "Extracted from /etc/hosts: " <> (mconcat . intersperse ",") svcNames
            return svcNames

assignRandomLocalPorts :: [T.Text] -> IO (M.Map T.Text Int)
assignRandomLocalPorts svcNames = do
  ports <- mapM (\_ -> getFreePort) svcNames
  let kvs = zip svcNames ports
  putStrLn $ "Local port mappings: " <> show kvs
  return $ M.fromList kvs


startSSH :: String -> M.Map T.Text Int -> IO ()
startSSH gwHost host2port = do
  _ <- SPP.forkProcess $ do
    let portForwardOpts = map hp2opt $ M.toList host2port
    let sshOpts =  [ "-a", "-C", "-N", "-n", "-v"] <> (mconcat portForwardOpts) <> [gwHost]
    putStrLn $ "Starting SSH with options " <> (mconcat . intersperse " ") sshOpts
    _ <- SP.rawSystem  "ssh" sshOpts
    return ()
  return ()

  where hp2opt (h,p) = [ "-L", show p <> ":" <> T.unpack h <> ":" <> "80"]


handler :: M.Map T.Text Int -> Request -> IO WaiProxyResponse
handler hostToLocalPort req = do
  case requestHeaderHost req
    of Nothing    -> return $ WPRResponse $ responseLBS status500 [] "ERROR: proxy requires Host header to work."
       Just hostH -> case M.lookup (TE.decodeUtf8 hostH) hostToLocalPort
                       of Nothing   -> return $ WPRResponse $ responseLBS status404 [] $ "No mapping for " <> (L8.fromChunks [hostH]) <> ", have you listed it in /etc/hosts?"
                          Just port -> return $ WPRProxyDest $ ProxyDest "localhost" port


main :: IO ()
main = do
  Opts{..} <- cmdArgs cmdLineOpts
  svcNames <- getServiceNamesToProxy
  hostToLocalPort <- assignRandomLocalPorts svcNames

  if (null gateway)
    then error $ "\n\nGateway host is required!\nPlease re-run with --help to see all options.\n\n"
    else return ()

  startSSH gateway hostToLocalPort

  putStrLn "Starting proxy ..."

  man <- HC.newManager HC.defaultManagerSettings
  let app = waiProxyTo (handler hostToLocalPort) (defaultOnExc) man
  run listen app
  HC.closeManager man


