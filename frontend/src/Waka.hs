module Waka (wakaMain) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate, blank, elAttr 
  , accumDyn, divClass, leftmost, (=:), zipDynWith 
  , tickLossyFromPostBuildTime, widgetHold_
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  )

import CWidget (elChara,elSpace,evElButton,elTextScroll)

import Define
import Converter (getInfoFromChar,showMap,putMapInFrame)
import Action (movePlayer)
import Code (exeCode)

wakaMain ::
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , Prerender t m
  ) => Game -> m ()
wakaMain gs = do
  elAttr "div" ("id" =: "map") $ mdo
    evTime <- tickLossyFromPostBuildTime 0.1
    let beTxtOn = current dyTxtOn
    let beScrOn = current dyScrOn
    let evNTime = gate beTxtOn evTime
    let evScrOn = gate beScrOn evNTime
    let evWTick = WTick <$ evNTime
    let evWk = leftmost [evWOk, evWLeft, evWUp, evWDown, evWRight, evWTick]
    dyGs <- accumDyn wakaUpdate gs evWk
    let dyVText = _txv <$> dyGs
    let dyIsText = _itx <$> dyGs
    let dyIMode = _imd <$> dyGs
    let dyTexCountSub = _tcs <$> dyGs
    let dyTxtOn = zipDynWith (\a b -> a && b==Txt) dyIsText dyIMode
    let dyScrOn = zipDynWith (\a b -> a && b `mod` 3 == 2) dyTxtOn dyTexCountSub
    divClass "flexbox" $ do
      elChara
      divClass "kai" $ dynText (showMapRect <$> dyGs)
    elSpace
    divClass "tbox" $ 
      elAttr "div" ("id" =: "wkText" <> "class" =: "tate") (dynText dyVText)
    elSpace  
    evButtonOk <- evElButton "pad" "●"
    evButtonLeft <- evElButton "pad" "←"
    evButtonUp <- evElButton "pad" "↑"
    evButtonDown <- evElButton "pad" "↓"
    evButtonRight <- evElButton "pad" "→"
    widgetHold_ blank (elTextScroll <$ evScrOn)
    let evWOk = WOk <$ evButtonOk
    let evWLeft = WLeft <$ evButtonLeft
    let evWUp = WUp <$ evButtonUp
    let evWDown = WDown <$ evButtonDown
    let evWRight = WRight <$ evButtonRight
    pure ()

showMapRect :: Game -> T.Text
showMapRect gs =
  let obMap = _omp gs
      mapSize = _msz gs
      mapPos = _mps gs
   in putMapInFrame mapWinSize mapPos $ showMap mapSize obMap

wakaUpdate :: Game -> WkEvent -> Game
wakaUpdate gs wev =
  let imode = _imd gs
   in if imode==Txt then
        case wev of
          WTick -> textUpdate gs
          WOk -> okButton gs
          _ -> gs
                    else
        case wev of
          WOk -> gs
          dirEv -> 
            let obMap = _omp gs
                mapSize = _msz gs
                mapPos = _mps gs
                evActs = _evas gs
                (nomp,nmps,npevs) = 
                      movePlayer dirEv mapSize mapWinSize mapPos obMap
                ngs = exeEvActs gs npevs evActs
             in ngs{_omp=nomp,_mps=nmps} 

exeEvActs :: Game -> [PEvent] -> [EvAct] -> Game
exeEvActs gs [] nevas = gs{_evas = nevas} 
exeEvActs gs (pe:pes) evas = 
  let (nevas,ncodes) = getCodes pe ([],[]) evas
      ngs = if null ncodes then gs else foldl exeCode gs ncodes
   in exeEvActs ngs pes nevas

getCodes :: PEvent -> ([EvAct],[Code]) -> [EvAct] -> ([EvAct],[Code])
getCodes _ necs [] = necs
getCodes pe (nevas,ncodes) (ea@(EA te cd n i):eas) =
  let ast = checkAct pe ea
   in case ast of
      NAct -> getCodes pe (nevas<>[ea],ncodes) eas
      TAct -> getCodes pe (nevas<>[EA te cd n (i+1)],ncodes) eas
      EAct -> getCodes pe (nevas,ncodes<>[cd]) eas

checkAct :: PEvent -> EvAct -> Ast
checkAct pe (EA te _ n co) = 
  let isAct = pe==te
   in if isAct then if co+1==n then EAct else TAct else NAct

okButton :: Game -> Game
okButton gs = 
  let imd = _imd gs
   in gs{_itx=imd==Txt,_tcs=0} 

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
            nitx = not isStop && isText && isTextShowing
            ntxv = if isTyping then textView <> T.singleton targetChar
                               else textView
            ntct = textCount + scanLength 
            ngs0 = gs{_itx=nitx, _txv=ntxv, _tct=ntct, _tcs=_tcs gs + 1}
            ngs1 = if isCode then exeCode ngs0 codeText else ngs0
         in ngs1
                                else gs

