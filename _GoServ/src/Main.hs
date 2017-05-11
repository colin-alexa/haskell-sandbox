module Main where

import qualified Text.Blaze.Html.Renderer.Pretty as RenderPretty

import qualified Web.SimpleServer as S
import qualified Web.SimpleServer.HTTPResponse as HTTP
import qualified Web.SimpleServer.HTTPRequest as Request

import qualified GoServ.Persistence as P
import qualified GoServ.Templates as T

import Debug.Trace

goRoute :: S.HTTPRequest -> IO S.HTTPResponse
goRoute req = case Request.reqMethod req of
                Request.Get  -> redirect req
                Request.Post -> do 
                  traceIO "received POST"
                  return . HTTP.okMessage . show $ req

redirect :: S.HTTPRequest -> IO S.HTTPResponse
redirect req = do
  let (alias_part, suffix_part) = break (/= '/') (Request.reqURL req)
  alias <- P.lookupAlias alias_part
  case alias of
    Just route -> return $ HTTP.redirect (route ++ suffix_part)
    Nothing    -> return newAliasResponse
  where
    setHeader'       = fmap flip . flip $ HTTP.setHeader
    newAliasResponse = setHeader' "content-type" "text/html" $ HTTP.okMessage $ RenderPretty.renderHtml T.newAliasPage

main = S.simpleServe 8080 goRoute
