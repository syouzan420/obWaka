module Frontend where

import Common.Route (FrontendRoute (..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route (R)
import Obelisk.Generated.Static (static)


import Reflex.Dom.Core 
  ( text, el, elAttr, blank , (=:) 
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  )

import CWidget (elSpace)
import Waka (loadGame)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead 
  , _frontend_body = frontendBody 
  }

frontendHead :: DomBuilder t m => m ()
frontendHead = do
  el "title" $ text "waka"
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1.0"
    )
    blank

  elAttr
    "link"
    ("href" =: $(static "main.css")
      <> "type" =: "text/css"
      <> "rel" =: "stylesheet")
    blank

frontendBody :: 
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , Prerender t m
  ) => m ()
frontendBody = do 
  elSpace
  loadGame 

