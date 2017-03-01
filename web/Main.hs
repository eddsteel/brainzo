{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Brainzo
import Control.Applicative((<$>), optional)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Happstack.Server (uriRest)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Turtle(sh, echo, liftIO)

main :: IO ()
main = sh $ do
  brainzo <- birth
  liftIO $ serve (Just $ defaultServerConfig {port = 4242}) (api brainzo)

api :: Brainzo -> ServerPart Response
api b = msum [
  dir "js" $ path (\f -> serveFile
                         (asContentType "application/javascript")
                         (concat ["js/", f, ".js"]))
  , dir "key" $ nullDir >> keyboard
  , uriRest (bzHandler b . T.pack)
  , usage ]

emptyBody :: Response
emptyBody = toResponse ()

template :: Text -> H.Html -> Response
template title body =
  toResponse $ H.docTypeHtml $ do
    H.head $ do
      H.title . toHtml $ title
    H.body $ do
      body

bzHandler :: Brainzo -> Text -> ServerPart Response
bzHandler b rest =
  let ts = tail $ "/" `T.splitOn` rest
  in
    sh (getToWork b ts) >> ok emptyBody

usage :: ServerPart Response
usage = ok . template "Bleep Bloop" $ H.pre . H.code . toHtml $ brainzoUsage

keyLink :: Text -> Text -> H.Html
keyLink key word = a ! href keyPath $ (toHtml word)
  where
    keyPath = H.textValue . T.concat $ ["/key/", key]

keyboard :: ServerPart Response
keyboard = serveJs "keyboard"

serveJs :: Text -> ServerPart Response
serveJs s = ok . template s $ do
  H.script ! A.src "foo" $ ""
  H.div ! A.class_ "canvas" $ ""
