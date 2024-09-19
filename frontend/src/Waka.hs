module Waka (loadGame) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Maybe (isNothing,fromMaybe)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate, blank, elAttr, constDyn, el, text 
  , accumDyn, divClass, leftmost, (=:), zipDynWith , sample, elDynAttr
  , tickLossyFromPostBuildTime, widgetHold_, toggle, holdDyn 
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  , Dynamic
  )

import CWidget (dyChara,imgsrc,elSpace,evElButton,evElButtonH,elTextScroll
               ,saveState,loadState,mkHidden,elImage0)

import Define
import Initialize (newGame)
import TextData (textData)
import Converter (getInfoFromChar,showMap,putMapInFrame,inpToDir,getSections
                 ,setMapStartPos,dirToText,lookupFromSections,updateTextSection
                 ,updateMapData,makeObjectDatas,makeGameStateText,toGameState)
import Object (getDirByName,updateDirByName,updatePosByName,getObjName
              ,getObjDef,deleteObjByName,getObjByName)
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
    let beETR = current dyETR
    let beIsSave = fmap (==Save) beETR
    let evTxTime = gate beTxtOn evTime
    let evWTick = WTick <$ evTime
    let evWk = leftmost (evWDir1<>evWDir2<>[evWTick])
    let evSave = gate beIsSave evWTick 
    dyGs <- accumDyn wakaUpdate gs evWk
    let dyVText = _txv <$> dyGs
    let dyIsText = _itx <$> dyGs
    let dyIMode = _imd <$> dyGs
    let dyETR = _etr <$> dyGs 
    let dyTxtOn = zipDynWith (\a b -> a && (b==Txt || b==Cho)) dyIsText dyIMode
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
--    dynText (T.pack . show <$> dyObjectMap)
    divClass "tbox" $ 
      elAttr "div" ("id" =: "wkText" <> "class" =: "tate") (dynText dyVText)
    elSpace  
    evWDir1 <- mapM (evElButton "pad") ["●","→","↑"] <&>
                                     zipWith (<$) [WOk,WRight,WUp]
    _ <- el "div" $ text " " 
    evWDir2 <- mapM (evElButton "pad") ["□","←","↓"] <&>
                                     zipWith (<$) [WSub,WLeft,WDown]
    widgetHold_ blank (elTextScroll <$ evTxTime)
    widgetHold_ blank (saveGame dyGs <$ evSave)

saveGame :: (DomBuilder t m, Prerender t m, MonadHold t m) => Dynamic t Game -> m ()
saveGame dyGs = do 
  gs <- sample (current dyGs)
  (saveState . makeGameStateText) gs

loadGame ::
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , Prerender t m
  ) => m () 
loadGame = mdo
  dyStateMb <- loadState
  let dyState = fmap (fromMaybe T.empty) dyStateMb
  elDynAttr "div" dyHide $ do 
        elImage0
        divClass "kai" $ text "變〜へん〜現實の向かう側"
  evs <- mapM (evElButtonH dyBool "pad") ["はじめから","つづき"] 
                                            <&> zipWith (<$) [1,2]
  let evStart = leftmost evs
  dyNum <- holdDyn 0 evStart
  dyBool <- toggle True evStart
  let dyHide = mkHidden <$> dyBool
  widgetHold_ blank (gameStart dyState dyNum <$ evStart)

gameStart ::
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , Prerender t m
  ) => Dynamic t T.Text -> Dynamic t Int -> m () 
gameStart dst di = do
  st <- (sample . current) dst
  i <- (sample . current) di 
  if st==T.empty || i==1 then let txs = getSections $ T.lines textData
                                  (TS _ tx) = head txs
                               in wakaMain newGame{_txs=txs, _txw=tx}
                         else wakaMain (toGameState st) 

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
                WRight -> if tln>1 then titles!!1 else T.empty
                WUp -> if tln>2 then titles!!2 else T.empty
                WLeft -> if tln>3 then titles!!3 else T.empty
                WDown -> if tln>4 then titles!!4 else T.empty
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
             WSub -> 
               let txw = ";cho_セーブ_textSave_もどる_textBack"
                in gs{_imd=Txt, _itx=True, _txw=txw, _txv=T.empty, _tct=0}
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
enterNewMap gs (PEnter pps oname:_) = 
  let mnm = _mnm gs
      omp = _omp gs
      obj = getObjByName oname omp
      tdf = maybe T.empty getObjDef obj
   in setMap gs{_pmp = (mnm,pps,omp)} (if tdf==T.empty then "0" else T.drop 3 tdf) 
enterNewMap gs (PLeave:_) =
  let txs = _txs gs
      mnm = _mnm gs
      lomp = deleteObjByName "player" (_omp gs)
      titleM = "map"<>mnm
      titleO = "obj"<>mnm
      mapData = lookupFromSections txs titleM 
      newMapData = updateMapData mapData lomp
      newObjData = T.unlines $ makeObjectDatas lomp
      ntxs = updateTextSection (TS titleO newObjData) $ 
                          updateTextSection (TS titleM newMapData) txs
      (tmnm,tps,omp) = _pmp gs 
      ngs = setMap gs tmnm
      msz = _msz ngs
      nomp = updatePosByName "player" tps omp
      mpos = setMapStartPos tps mapWinSize msz
   in ngs{_txs=ntxs, _mps=mpos, _omp=nomp}
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
   in gs{_itx=imd==Txt,_tcs=0,_iths=niths,_etr=NoEvent} 


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
      eventTrigger = _etr gs
      wholeText = _txw gs
      textCount = _tct gs
      textLength = T.length wholeText 
      isTextShowing = textCount < textLength
      pgs = if eventTrigger==NoEvent then gs else gs{_etr=NoEvent}
   in if isText && isTextShowing then 
        let textView = _txv pgs
            (isStop,isTyping,isCode,targetChar,codeText,scanLength)
              = getInfoFromChar wholeText textCount
            nitx = not isStop && isText && isTextShowing
            ntxv = if isTyping then textView <> T.singleton targetChar
                               else textView
            ntct = textCount + scanLength 
            ngs0 = pgs{_itx=nitx, _txv=ntxv, _tct=ntct, _tcs=_tcs pgs + 1}
            ngs1 = if isCode then exeCode ngs0 codeText else ngs0
         in ngs1
                                else 
         if isTextShowing then pgs else exeCode pgs "sc_0 sp"

