module Waka (wakaMain) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate, blank, elAttr, constDyn, el, text 
  , accumDyn, divClass, leftmost, (=:), zipDynWith 
  , tickLossyFromPostBuildTime, widgetHold_
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  )

import CWidget (dyChara,imgsrc,elSpace,evElButton,elTextScroll)

import Define
import Converter (getInfoFromChar,showMap,putMapInFrame,inpToDir
                 ,setMapStartPos,dirToText)
import Object (getDirByName,updateDirByName,updatePosByName,getObjName
              ,getObjDef)
import Action (movePlayer,hitAction,putAction,attackAction,moveObject)
import Code (exeCode,setMap,moveDialog)

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
    let evWk = leftmost (evWDir1<>evWDir2<>[evWTick])
    dyGs <- accumDyn wakaUpdate gs evWk
    let dyVText = _txv <$> dyGs
    let dyIsText = _itx <$> dyGs
    let dyIMode = _imd <$> dyGs
    let dyTxtOn = zipDynWith (\a b -> a && b==Txt) dyIsText dyIMode
    let dyImg = dyGs >>= (\n -> constDyn (imgsrc!!n)) . _chn
    let dyDir = (dirToText <$> getDirByName "player") . _omp <$> dyGs
    let dyHave = _hav <$> dyGs
    divClass "flexbox" $ do
      el "div" $ dyChara dyImg
      divClass "kai" $ dynText (showMapRect <$> dyGs)
      divClass "kai" $ do
         dynText $ fmap (<>"\n") dyDir 
         dynText $ dyHave <&> 
            \case Just hv -> ">"<>getObjName hv; Nothing -> T.empty 
    elSpace
--    let dyObjectMap = _omp <$> dyGs
--    let dyEvas = _evas <$> dyGs
--    let dyTxs = _txs <$> dyGs
--    dynText (T.pack . show <$> dyTxs)
    divClass "tbox" $ 
      elAttr "div" ("id" =: "wkText" <> "class" =: "tate") (dynText dyVText)
    elSpace  
    evWDir1 <- mapM (evElButton "pad") ["●","→","↑"] <&>
                                     zipWith (<$) [WOk,WRight,WUp]
    _ <- el "div" $ text " " 
    evWDir2 <- mapM (evElButton "pad") ["□","←","↓"] <&>
                                     zipWith (<$) [WSub,WLeft,WDown]
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
   in case imode of
        Txt -> case wev of
          WTick -> let iths = _iths gs
                    in if iths then repeatTexUpdate gs else textUpdate gs
          WOk -> okButton gs
          _ -> gs
        Cho -> 
          let titles = _cho gs
              tln = length titles
              title = case wev of
                WOk -> if tln>0 then head titles else T.empty 
                WLeft -> if tln>1 then titles!!1 else T.empty
                WUp -> if tln>2 then titles!!2 else T.empty
                WDown -> if tln>3 then titles!!3 else T.empty
                WRight -> if tln>4 then titles!!4 else T.empty
                _ -> T.empty 
           in if title==T.empty then gs else moveDialog gs{_imd=Txt} title
        Ply -> 
          let obMap = _omp gs
              mapSize = _msz gs
              pHave = _hav gs    
              pDir = getDirByName "player" obMap
              evActs = _evas gs
              mnm = _mnm gs
           in case wev of
             WTick -> objectUpdate $ effectUpdate gs
             WOk -> 
               let tmpMap = _tmp gs
                   txSec = _txs gs
                   ntmp = if isNothing pHave 
                             then hitAction "player" mapSize obMap tmpMap
                             else tmpMap
                   (npevs,nomp,nphv) = case pHave of
                          Nothing -> attackAction txSec pDir mnm obMap 
                          Just tob -> putAction tob pDir mapSize obMap  
                   ngs = gs{_tmp=ntmp, _omp=nomp, _hav=nphv}
                in exeEvActs ngs npevs evActs
             dirEv -> 
               let mapPos = _mps gs
                   keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir dirEv
                   isSameDir = pDir == keyDir
                   (npevs,nomp,nmps,nphv) = if isSameDir 
                      then movePlayer dirEv pHave mapSize mapWinSize mapPos obMap
                      else ([],updateDirByName "player" keyDir obMap,mapPos,pHave)
                   ngs = exeEvActs gs{_omp=nomp,_mps=nmps,_hav=nphv} npevs evActs
                   ngs2 = enterNewMap ngs npevs
                in ngs2 

objectUpdate :: Game -> Game
objectUpdate gs = let omp = _omp gs
                      msz = _msz gs
                      stg = _stg gs
                      (nomp,nstg) = moveObject stg msz omp omp 
                   in gs{_omp=nomp,_stg=nstg}

enterNewMap :: Game -> [PEvent] -> Game 
enterNewMap gs [] = gs 
enterNewMap gs (PEnter pps obj:_) = 
  let mnm = _mnm gs
      omp = _omp gs
      tdf = getObjDef obj
   in setMap gs{_pmp = (mnm,pps,omp)} (if tdf==T.empty then "0" else T.drop 3 tdf) 
enterNewMap gs (PLeave:_) =
  let (tmnm,tps,omp) = _pmp gs 
      ngs = setMap gs tmnm
      msz = _msz ngs
      nomp = updatePosByName "player" tps omp
      mpos = setMapStartPos tps mapWinSize msz
   in ngs{_mps=mpos, _omp=nomp}
enterNewMap gs (_:xs) = enterNewMap gs xs


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
                      else (nevas<>eas,ncodes<>[cd])

checkAct :: PEvent -> EvAct -> Ast
checkAct pe (EA te _ n co) = 
  let isAct = pe==te
   in if isAct then if co+1==n || n==0 then EAct else TAct else NAct

okButton :: Game -> Game
okButton gs = 
  let imd = _imd gs
      itx = _itx gs
      iths = _iths gs
      niths = itx && not iths
   in gs{_itx=imd==Txt,_tcs=0,_iths=niths} 


scanEffect :: ObMap -> ObMap
scanEffect [] = []
scanEffect (ob@(Ob ch nm tp df oc dr ps):xs)
  | ch=='/' = Ob '\\' nm tp df oc dr ps:scanEffect xs
  | ch=='\\' = scanEffect xs
  | otherwise = ob:scanEffect xs

effectUpdate :: Game -> Game
effectUpdate gs = gs{_tmp = scanEffect (_tmp gs)}

repeatTexUpdate :: Game -> Game
repeatTexUpdate gs =
  let isText = _itx gs
   in if isText then repeatTexUpdate (textUpdate gs) else gs{_iths=False}

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
         if isTextShowing then gs else exeCode gs "sc_0 sp"

