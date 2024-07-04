module Frontend where

import Common.Route (FrontendRoute (..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route (R)
import Obelisk.Generated.Static (static)


import Reflex.Dom.Core 
  ( text, el, elAttr, blank , (=:) 
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  )

import CWidget (elSpace)
import Define
import Initialize (newGame)
import Converter (getSections)
import TextData (textData)
import Waka (wakaMain)

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
        <> "contents" =: "width=device-width, initial-scale=1.0"
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

  let sections = getSections $ T.lines textData 
  let (TS _ tx) = head sections
  wakaMain newGame{_txs=sections,_txw=tx}

testText :: T.Text
testText = "情報空間にも 重さみたいなものがある\nと思ふんだよね\n重い情報は 次元の低い\n抽象度の低い方へ向かひ\n軽い情報は 次元の高い\n抽象度の高い方へ向かふ\n重い情報が 長時間意識されると\n物理空間において 何かが「つく」ことになるし\n軽い情報の意識が續くと\n物理空間の「滞り」が取れたりするのだと思ふ"
