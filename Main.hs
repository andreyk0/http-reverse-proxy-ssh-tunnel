{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.List
import Data.Monoid
import Data.Streaming.Network
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Socket               (setSocketOption, sClose, SocketOption(ReuseAddr))
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO.Unsafe             (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.IORef as I
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HC
import qualified System.Environment as SE
import qualified System.Exit as SE
import qualified System.IO as SIO
import qualified System.Posix.Process as SPP
import qualified System.Process as SP


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

# PROXY TO PROD

127.0.0.1 svc-something
127.0.0.1 svc-something-else

-}
getServiceNamesToProxy :: IO [T.Text]
getServiceNamesToProxy = do
  etcHosts <- TIO.readFile "/etc/hosts"
  let needle = "# PROXY TO PROD"
  let (_, svcLines) = T.breakOnEnd needle etcHosts
  let nonEmptySvcLines = filter (not . T.null) $ T.lines svcLines
  let svcNames = map ( last . (T.splitOn " ") . T.strip  ) nonEmptySvcLines
  if (null svcNames)
    then error $ "Expected to find " <> (T.unpack needle) <> " in /etc/hosts, followed by \
                                                  \127.0.0.1 svc-something \
                                                  \127.0.0.1 svc-or-other"
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

-- only care about gateway host for now
parseArgs :: IO String
parseArgs = do
  args <- SE.getArgs
  if (null args)
    then do
      pName <- SE.getProgName
      SIO.hPutStrLn SIO.stderr $ "Usage: " <> pName <> " ssh.gateway.host.name"
      SE.exitFailure
    else
      return $ head args


main :: IO ()
main = do
  gwHost <- parseArgs
  svcNames <- getServiceNamesToProxy
  hostToLocalPort <- assignRandomLocalPorts svcNames
  startSSH gwHost hostToLocalPort

  putStrLn "Starting proxy ..."

  man <- HC.newManager HC.defaultManagerSettings
  let app = waiProxyTo (handler hostToLocalPort) (defaultOnExc) man
  run 8080 app
  HC.closeManager man


