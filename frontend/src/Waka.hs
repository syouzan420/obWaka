module Waka (wakaMain) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate   
  , accumDyn, divClass, leftmost 
  , tickLossyFromPostBuildTime
  , DomBuilder, MonadHold, PostBuild
  , Performable, PerformEvent, TriggerEvent
  )

import CWidget (evElButton)

import Define
import Converter (getInfoFromChar)
import Code (exeCode)

wakaMain ::
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  ) => Game -> m ()
wakaMain gs = mdo
  evTime <- tickLossyFromPostBuildTime 0.1
  let beIsText = current dyIsText
  let evNTime = gate beIsText evTime
  let evWTick = WTick <$ evNTime
  let evWk = leftmost [evWTick, evWOk, evWCancel]
  dyGs <- accumDyn wakaUpdate gs evWk
  let dyVText = _txv <$> dyGs
  let dyIsText = _itx <$> dyGs
  divClass "tbox" $ divClass "tate" $ dynText dyVText
  evButtonOk <- evElButton "pad" "●"
  evButtonCancel <- evElButton "pad" "■"
  let evWOk = WOk <$ evButtonOk
  let evWCancel = WCancel <$ evButtonCancel
  pure ()

wakaUpdate :: Game -> WkEvent -> Game
wakaUpdate gs wev =
  case wev of
    WTick -> textUpdate gs
    WOk -> okButton gs
    WCancel -> cancelButton gs

okButton :: Game -> Game
okButton gs = gs{_itx=True} 

cancelButton :: Game -> Game
cancelButton gs = gs

textUpdate :: Game -> Game
textUpdate gs =
  let isText = _itx gs
      wholeText = _txw gs
      textCount = _tct gs
      textLength = T.length wholeText 
      isTextShowing = textCount < textLength
   in if isText && isTextShowing then 
        let textView = _txv gs
            (isStop,isTyping,isCode,targetChar,codeText,scanLength)
              = getInfoFromChar wholeText textCount
            nitx = not isStop && isText
            ntxv = if isTyping then textView <> T.singleton targetChar
                               else textView
            ntct = textCount + scanLength 
            ngs0 = gs{_itx=nitx, _txv=ntxv, _tct=ntct}
            ngs1 = if isCode then exeCode ngs0 codeText else ngs0
         in ngs1
                                else gs


