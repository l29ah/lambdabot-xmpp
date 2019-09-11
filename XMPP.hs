{-# LANGUAGE OverloadedStrings #-}

module XMPP (
    xmppPlugins,
    xmppPlugin
) where

-- base
import Data.Char (isControl)
import Control.Monad
import Data.List.Split
import Data.List

-- Hackage
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Lifted (fork)
import Control.Exception.Lifted as E (SomeException(..), throwIO, catch)
import Control.Monad.Trans (lift)
import Network (PortID(..))
import qualified Data.Text as T
import Network.TLS (
  ClientParams(..), ClientHooks(..), defaultParamsClient, Supported(..)
  )
import qualified Network.TLS.Extra as CI
import Data.Default (def)
import Network.Xmpp
import Data.XML.Types (
  nameLocalName, elementName, elementText
  , Element(Element), Name(Name), Content(ContentText), Node(..)
  )
import System.Timeout.Lifted (timeout)
import qualified Data.X509.Validation as XV

-- Lambdabot
import Lambdabot.IRC
import Lambdabot.Logging
import Lambdabot.Monad
import Lambdabot.Plugin
import Lambdabot.Util

------
type XMPP = ModuleT () LB

data XMPPConfig = XMPPConfig {
    xmppHost :: String,
    xmppPort :: PortID,
    xmppUser :: String,
    xmppNick :: String,
    xmppPass :: String,
    xmppRoom :: String
    }

----

xmppPlugins :: [String]
xmppPlugins = ["xmpp"]

xmppPlugin :: Module ()
xmppPlugin = newModule
    { moduleCmds = return
        [ (command "xmpp-connect")
            { aliases = []
            , help = say "xmpp-connect <tag> <host> <portnum> <xmpp_user> <xmpp_nick> <xmpp_pass> <xmpp_room>.  connect to an xmpp server"
            , process = \rest ->
                         case splitOn " " rest of
                          tag:hostn:portn:usern:nick:passw:room -> do
                              pn <- (PortNumber . fromInteger) `fmap` readM portn
                              let xmppconf = XMPPConfig hostn pn usern nick passw (intercalate " " room)
                              lift (online tag xmppconf)
                          _ -> say "XMPP: Not enough parameters!"
            }
        ]
    }

enqueue :: Chan T.Text -> IrcMessage -> XMPP ()
enqueue chan ircmsg = do
    let msg = Data.List.last (ircMsgParams ircmsg)
    let msg' = filtermsg $ T.filter (not . isControl) (T.drop 1 (T.pack msg))
    io $ writeChan chan msg'
    where
      filtermsg :: T.Text -> T.Text
      filtermsg m = case (T.isPrefixOf "ACTION " m) of
        True -> T.replace "ACTION" "/me" m
        False -> T.stripStart m

joinMessages = T.intercalate (T.singleton '\n') . filter (not . T.null)

readTimeout :: Chan T.Text -> Int -> IO T.Text
readTimeout chan time = do
    result <- timeout time $ readChan chan
    case result of
        Nothing -> return T.empty
        Just text -> do
            next <- readTimeout chan time
            return $ joinMessages [text, next]

consolidate :: Session -> XMPPConfig -> Chan T.Text -> XMPP ()
consolidate sess xmppconf chan = io $ forever $ do
    first <- readChan chan
    others <- readTimeout chan 50000
    let name = Name "body" (Just "jabber:client") Nothing
        node = NodeContent (ContentText $ joinMessages [first, others])
    let payload = Element name [] [node]

    let m = Message{ messageFrom    = Nothing
            , messageID      = Nothing
            , messageTo      = Just (parseJid (xmppRoom xmppconf))
           , messageType = GroupChat
            , messagePayload = [payload]
            , messageLangTag = Nothing
            , messageAttributes = []
            }
    io $ sendMessage m sess >> return ()

online :: String -> XMPPConfig -> XMPP ()
online tag xmppconf = do
    chan <- io $ newChan
    sess <- io $ xmppListen xmppconf
    void . fork $ consolidate sess xmppconf chan
    E.catch
        (registerServer tag (enqueue chan))
        (\err@SomeException{} -> E.throwIO err)
        
    void . fork $ E.catch
        (lb (readerLoop tag sess xmppconf))
        (\e@SomeException{} -> do
            errorM (show e)
            unregisterServer tag)

readerLoop :: String -> Session -> XMPPConfig -> LB ()
readerLoop tag sess xmppconf = forever $ do
    mes <- io $ getMessage sess

    let from = maybe "(anybody)" T.unpack (resourcepart =<< messageFrom mes)
    let to = maybe "(anybody)" T.unpack (resourcepart =<< messageTo mes)
    let bodyElems = elems "body" mes
    let delayElems = elems "delay" mes

    when ((/=) from (xmppNick xmppconf) &&
          null delayElems &&
          (not . null) bodyElems &&
          messageType mes == GroupChat) $ do
        
        let body = head $ elementText (head bodyElems)
            
        void . fork . void . timeout 15000000 . received $ IrcMessage {
          ircMsgServer = tag
          , ircMsgLBName = xmppNick xmppconf
          , ircMsgPrefix = from
          , ircMsgCommand = "PRIVMSG"
          , ircMsgParams = [to, ':' : T.unpack body]
          }
        return ()
    (readerLoop tag sess xmppconf)

    where
      elems tagname mes = filter ((== tagname) . nameLocalName . elementName) $
                          (messagePayload mes)

xmppListen :: XMPPConfig -> IO Session
xmppListen xmppconf = do
    result <- session
              (xmppHost xmppconf)
              (Just (\_ -> [plain (T.pack . xmppUser $ xmppconf) Nothing (T.pack . xmppPass $ xmppconf)], Nothing))
              def { onConnectionClosed = \sess _ -> do
                      reconnect' sess
                      sendMUCPresence xmppconf sess
                  }
    sess <- case result of
                Right s -> return s
                Left e -> error $ "XmppFailure: " ++ (show e)
    sendMUCPresence xmppconf sess
    return sess

sendMUCPresence :: XMPPConfig -> Session -> IO ()
sendMUCPresence xmppconf sess = do
    jid <- getJid $ sess
    _ <- sendPresence (def {
           presenceFrom = jid
           , presenceTo = Just . parseJid $ (xmppRoom xmppconf) ++ '/' : (xmppNick xmppconf)
           , presencePayload = [Element "x" [(Name "xmlns" Nothing Nothing, [ContentText "http://jabber.org/protocol/muc"])] []]
           }) sess
    return ()
