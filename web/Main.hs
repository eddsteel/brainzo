{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Brainzo
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import Happstack.Lite
import Happstack.Server (uriRest)
import Text.Blaze.Html5 ((!), a, toHtml)
import Text.Blaze.Html5.Attributes (href)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Turtle(sh, liftIO, lineToText, Line, strict)

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
      out = getToWork b ts
  in
   strict out >>= ok . toResponse

usage :: ServerPart Response
usage = ok . template "Bleep Bloop" $ H.pre . H.code . toHtml . linesToText . NEL.toList $ brainzoUsage

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

linesToText :: [Line] -> Text
linesToText = T.concat . fmap lineToText
