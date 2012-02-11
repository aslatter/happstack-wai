{-# LANGUAGE OverloadedStrings, CPP #-}

module Happstack.Server.Wai
    ( toApplication
    , run
    , Warp.Port
      -- ** Low-level functions
    , convertRequest
    , convertResponse
    , standardHeaders
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Happstack.Server as H
import qualified Happstack.Server.Internal.Clock as H
import qualified Happstack.Server.Internal.Cookie as H
import qualified Happstack.Server.Internal.MessageWrap as H

import Control.Monad.Trans.Resource
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Lazy as C
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W

import qualified Network.Wai.Handler.Warp as Warp

-- | Convert a Happstack 'H.ServerPart' to a WAI 'W.Application'.
toApplication :: H.ServerPart H.Response -> W.Application
toApplication sp wReq = do
  hReq <- convertRequest wReq
  hResp <- liftIO $ H.simpleHTTP'' sp hReq
  additionalHeaders <- liftIO standardHeaders
  return $ convertResponse additionalHeaders hResp

-- | Run a 'H.ServerPart' on warp at the specified port.
run :: Warp.Port -> H.ServerPart H.Response -> IO ()
run port = Warp.run port . toApplication

-- TODO - return '400 bad request' if we can't convert it
-- | Convert a WAI 'W.Request' to a Happstack 'H.Request'.
convertRequest :: W.Request -- ^ WAI request
               -> ResourceT IO H.Request
convertRequest wReq = do
  bodyInputRef <- liftIO newEmptyMVar
  bodyLbs <- BL.fromChunks <$> C.lazyConsume (W.requestBody wReq)
  bodyRef <- liftIO $ newMVar $ H.Body bodyLbs

  return $
    H.Request
     (W.isSecure wReq)
     (convertMethod $ W.requestMethod wReq)
     (convertPath $ W.pathInfo wReq)
     rawPath -- includes leading slash, does not include query
     rawQuery -- includes leading questionmark
     parsedQuery
     bodyInputRef
     cookies
     httpVersion
     headers
     bodyRef
     (B8.unpack (W.serverName wReq), W.serverPort wReq)

  where
    headers :: H.Headers -- Map ByteString HeaderPair
    headers =
        let rawAssocs = flip map (W.requestHeaders wReq) $ \(ciName, val) ->
                        (CI.original ciName, val)
            -- TODO: skip round-trip through string and back
            assocs = map (\(x,y) -> (B8.unpack x, B8.unpack y)) rawAssocs
        in H.mkHeaders assocs

    httpVersion :: H.HttpVersion
    httpVersion =
        case W.httpVersion wReq of
          W.HttpVersion major minor ->
              H.HttpVersion major minor

    cookies :: [(String, H.Cookie)]
    cookies =
        let cookieHeaders =
                filter (\x -> fst x == "Cookie") $ W.requestHeaders wReq
            rawCookies =
                map snd cookieHeaders
            foundCookies =
                concat $ mapMaybe H.getCookies rawCookies
        in map (\c -> (H.cookieName c, c)) foundCookies

    parsedQuery :: [(String,H.Input)]
    parsedQuery =
        case rawQuery of
          '?':xs -> H.formDecode xs
          xs     -> H.formDecode xs

    rawQuery :: String
    rawQuery = B8.unpack $ W.rawQueryString wReq

    rawPath :: String
    rawPath =
        B8.unpack . fst $ B.breakByte 63 (W.rawPathInfo wReq) -- 63 == '?'

convertPath :: [Text] -> [String]
convertPath [] = []
convertPath xs =
    -- the WAI paths include a blank for the trailing slash
    case reverse xs of
      ("":rest) -> map T.unpack (reverse rest)
      _ -> map T.unpack xs

convertMethod :: W.Method -> H.Method
convertMethod m =
    -- TODO: somehow return 'Bad Request' response
    -- instead of expecting the application host to
    -- catch errors.
    case W.parseMethod m of
      Left{} -> error $ "Unknown method " ++ (show . B8.unpack) m
      Right stdM ->
          case stdM of
            W.GET -> H.GET
            W.POST -> H.POST
            W.HEAD -> H.HEAD
            W.PUT -> H.PUT
            W.DELETE -> H.DELETE
            W.TRACE -> H.TRACE
            W.CONNECT -> H.CONNECT
            W.OPTIONS -> H.OPTIONS

-- | 'Date' header and server identification.
standardHeaders :: IO W.ResponseHeaders
standardHeaders = do
  dtStr <- H.getApproximateTime
  return
    [ ("Date", dtStr)
    , serverIdent
    , waiIdent
    ]

-- | Convert a Happstack 'H.Response' to a WAI 'W.Response'
convertResponse :: W.ResponseHeaders
                -- ^ Headers not in the response to send to the client (see 'standardHeaders')
                -> H.Response -- ^ Happstack response
                -> W.Response
convertResponse additionalHeaders hResp =
    case hResp of
      H.SendFile{H.sfOffset=off,H.sfCount=count,H.sfFilePath=filePath}
          ->
            let fp = W.FilePart off count
            in W.ResponseFile status headersNoCl filePath (Just fp)
      H.Response{H.rsBody=body}
          -> W.responseLBS status headers body
  where
  -- TODO description
    status = W.Status (H.rsCode hResp) ""
    headersNoCl =
        (additionalHeaders ++) $
        concatMap (\(H.HeaderPair k vs) -> map (\v -> (CI.mk k, v)) vs) $
        Map.elems (H.rsHeaders hResp)
    headers =
        case H.rsfLength (H.rsFlags hResp) of
          H.ContentLength ->
              ("Content-Length", B8.pack $ show $ BL.length $ H.rsBody hResp)
              : headersNoCl
          _ -> headersNoCl

serverIdent :: W.Header
serverIdent =
    ( "Server"
    , B8.pack $ "Happstack/" ++ VERSION_happstack_server
    )

waiIdent :: W.Header
waiIdent =
    ( "X-Wai-Version"
    , VERSION_wai
    )
