module Waka (wakaMain) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate, blank, elAttr, constDyn, el 
  , accumDyn, divClass, leftmost, (=:), zipDynWith 
  , tickLossyFromPostBuildTime, widgetHold_
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  )

import CWidget (dyChara,imgsrc,elSpace,evElButton,elTextScroll)

import Define
import Converter (getInfoFromChar,showMap,putMapInFrame,inpToDir)
import Object (getDirByName,updateDirByName,getObjName)
import Action (movePlayer,hitAction,putAction)
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
    let evTxTime = gate beTxtOn evTime
    let evWTick = WTick <$ evTime
    let evWk = leftmost (evWDir<>[evWTick])
    dyGs <- accumDyn wakaUpdate gs evWk
    let dyVText = _txv <$> dyGs
    let dyIsText = _itx <$> dyGs
    let dyIMode = _imd <$> dyGs
    let dyTxtOn = zipDynWith (\a b -> a && b==Txt) dyIsText dyIMode
    let dyImg = dyGs >>= (\n -> constDyn (imgsrc!!n)) . _chn
    let dyHave = _hav <$> dyGs
    divClass "flexbox" $ do
      el "div" $ dyChara dyImg
      divClass "kai" $ dynText (showMapRect <$> dyGs)
      el "div" $ dynText $ dyHave <&> 
          \hav -> case hav of Just hv -> " >:"<>getObjName hv; Nothing -> T.empty 
    elSpace
--    let dyObjectMap = _omp <$> dyGs
--    let dyEvas = _evas <$> dyGs
--    dynText (T.pack . show <$> dyObjectMap)
    divClass "tbox" $ 
      elAttr "div" ("id" =: "wkText" <> "class" =: "tate") (dynText dyVText)
    elSpace  
    evWDir <- mapM (evElButton "pad") ["●","←","↑","↓","→"] <&> zipWith (<$) [WOk,WLeft,WUp,WDown,WRight]
    widgetHold_ blank (elTextScroll <$ evTxTime)

showMapRect :: Game -> T.Text
showMapRect gs =
  let obMap = _omp gs
      tMap = _tmp gs
      mapSize = _msz gs
      mapPos = _mps gs
   in putMapInFrame mapWinSize mapPos $ showMap mapSize obMap tMap

wakaUpdate :: Game -> WkEvent -> Game
wakaUpdate gs wev =
  let imode = _imd gs
   in if imode==Txt then
        case wev of
          WTick -> textUpdate gs
          WOk -> okButton gs
          _ -> gs
                    else
        let obMap = _omp gs
            mapSize = _msz gs
            pHave = _hav gs    
            pDir = getDirByName "player" obMap
         in case wev of
           WTick -> effectUpdate gs
           WOk -> 
             let tmpMap = _tmp gs
                 ntmp = if isNothing pHave 
                             then hitAction "player" mapSize obMap tmpMap
                             else tmpMap
                 (nomp,nphv) = case pHave of
                          Nothing -> (obMap,Nothing) 
                          Just tob -> putAction tob pDir mapSize obMap  
              in gs{_tmp=ntmp, _omp=nomp, _hav=nphv}
           dirEv -> 
             let mapPos = _mps gs
                 evActs = _evas gs
                 keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir dirEv
                 isSameDir = pDir == keyDir
                 (nomp,nmps,npevs,nphv) = if isSameDir 
                      then movePlayer dirEv pHave mapSize mapWinSize mapPos obMap
                      else (updateDirByName "player" keyDir obMap,mapPos,[],pHave)
                 ngs = exeEvActs gs npevs evActs
              in ngs{_omp=nomp,_mps=nmps,_hav=nphv} 

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
      EAct -> if n==0 then getCodes pe (nevas<>[ea],ncodes<>[cd]) eas
                      else getCodes pe (nevas,ncodes<>[cd]) eas

checkAct :: PEvent -> EvAct -> Ast
checkAct pe (EA te _ n co) = 
  let isAct = pe==te
   in if isAct then if co+1==n || n==0 then EAct else TAct else NAct

okButton :: Game -> Game
okButton gs = 
  let imd = _imd gs
   in gs{_itx=imd==Txt,_tcs=0} 


scanEffect :: ObMap -> ObMap
scanEffect [] = []
scanEffect (ob@(Ob ch nm tp df oc dr ps):xs)
  | ch=='/' = Ob '\\' nm tp df oc dr ps:scanEffect xs
  | ch=='\\' = scanEffect xs
  | otherwise = ob:scanEffect xs

effectUpdate :: Game -> Game
effectUpdate gs = gs{_tmp = scanEffect (_tmp gs)}

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
                                else 
         if isTextShowing then gs else exeCode gs "ch_0 stpl"

