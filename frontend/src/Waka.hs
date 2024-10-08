module Waka (loadGame) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))
import qualified Data.Map as M 
import Data.List (nub)
import Data.Functor ((<&>),void)
import Data.Maybe (isNothing,isJust,fromMaybe)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate, blank, elAttr, constDyn, el, text
  , accumDyn, divClass, leftmost, (=:), zipDynWith , sample, elDynAttr
  , tickLossyFromPostBuildTime, widgetHold_, toggle, holdDyn 
  , prerender_, elDynHtmlAttr' 
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  , Dynamic, Event
  )

import CWidget (dyChara,imgsrc,elSpace,evElButton,evElButtonH,elTextScroll
               ,saveState,loadState,mkHidden,elImage0,elVibration)

import Define
import Initialize (newGame)
import TextData (textData)
import Converter (getInfoFromChar,showMap,putMapInFrame,inpToDir,getSections
                 ,setMapStartPos,dirToText,lookupFromSections,updateTextSection
                 ,updateMapData,makeObjectDatas,makeGameStateText,toGameState
                 ,makeRubiHtml)
import Object (getDirByName,updateDirByName,updatePosByName,getObjName
              ,getObjDef,deleteObjByName,getObjByName,getPosByName)
import Action (movePlayer,hitAction,putAction,attackAction,moveObject,shootBullet)
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
    let beIsLSave = fmap (==LSave) beETR
    let evTxTime = gate beTxtOn evTime
    let evWTick = WTick <$ evTime
    let evWk = leftmost (evBtList<>[evWTick])
    let evBt = leftmost evBtList 
    let evSave = gate beIsSave evWTick 
    let evLSave = gate beIsLSave evWTick
    dyGs <- accumDyn wakaUpdate gs evWk
    let dyVText = _txv <$> dyGs
    let dyIsText = _itx <$> dyGs
    let dyIMode = _imd <$> dyGs
    let dyETR = _etr <$> dyGs 
    let dyLife = _lif <$> dyGs
    let dyLnu = _lnu <$> dyGs
    let dyAtr = fmap (\t -> ("href" =: t)::M.Map T.Text T.Text) dyLnu
    let dyLnt = _lnt <$> dyGs
    let dyIsShowLife = isJust <$> dyLife
    let dyHide = mkHidden <$> dyIsShowLife
    let dyTxtOn = zipDynWith (\a b -> a && (b==Txt || b==Cho)) dyIsText dyIMode
    let dyImg = dyGs >>= (\n -> constDyn (imgsrc!!n)) . _chn
    let dyDir = (dirToText <$> getDirByName "player") . _omp <$> dyGs
    let dyHave = _hav <$> dyGs
    divClass "flexbox" $ do
      el "div" $ dyChara dyImg
      divClass "kai" $ do
         elDynAttr "div" dyHide $ divClass "cen" $ 
                  dynText (fmap (fromMaybe T.empty) dyLife) 
         dynText (showMapRect <$> dyGs)
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
--      elAttr "div" ("id" =: "wkText" <> "class" =: "tate") $ 
      prerender_ blank $ void $ 
            elDynHtmlAttr' "div" ("id"=: "wkText" <> "class" =: "tate") dyVText
        -- (dynText dyVText)
    elSpace  
    evBtList <- evWkButtons
    divClass "lnk" $ elDynAttr "a" dyAtr $ dynText dyLnt
    widgetHold_ blank (elVibration <$ evBt)
    widgetHold_ blank (elTextScroll <$ evTxTime)
    widgetHold_ blank (saveGame dyGs <$ evSave)
    widgetHold_ blank (lastSave dyGs <$ evLSave)

evWkButtons :: (DomBuilder t m) => m [Event t WkEvent]
evWkButtons = do
  evWUp <- evElButton "pad3" "↑" <&> (<$) WUp
  _ <- el "div" $ text "" 
  evWDir <- mapM (evElButton "pad") ["←","●","→"] <&>
                                   zipWith (<$) [WLeft,WOk,WRight]
  _ <- el "div" $ text "" 
  evWDown <- evElButton "pad3" "↓" <&> (<$) WDown
  evWSub <- evElButton "pad2" "□" <&> (<$) WSub
  return $ [evWUp,evWSub]<>evWDir<>[evWDown]
  

lastSave :: (DomBuilder t m, Prerender t m, MonadHold t m) => Dynamic t Game -> m ()
lastSave dyGs = do 
  gs <- sample (current dyGs)
  let gmc = _gmc gs
  let txs = getSections $ T.lines textData
  (saveState . makeGameStateText) newGame{_txs=txs, _gmc=gmc+1}

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
  dyStTextMb <- loadState
  let dyStText = fmap (fromMaybe T.empty) dyStTextMb
  let dySt = toGameState <$> dyStText
  let dyIsSaveData = (/=T.empty) <$> dyStText
  let dyIsClearGame = fmap (\st -> _gmc st>0) dySt
  elDynAttr "div" dyHide $ do 
        elImage0
        divClass "kai" $ text "變〜へん〜現實の向かう側"
  evFromBegin <- evElButtonH dyBool "pad4" "はじめから" <&> (<$) 1
  evContinue <- evElButtonH dyBool2 "pad4" "つづき" <&> (<$) 2
  evExtra <- evElButtonH dyBool3 "pad4" "おまけ" <&> (<$) 3
  let evStart = leftmost [evFromBegin,evContinue,evExtra] 
  dyNum <- holdDyn 0 evStart
  dyBool <- toggle True evStart
  let dyBool2 = zipDynWith (&&) dyBool dyIsSaveData
  let dyBool3 = zipDynWith (&&) dyBool dyIsClearGame 
  let dyHide = mkHidden <$> dyBool
  widgetHold_ blank (gameStart dySt dyNum <$ evStart)

gameStart ::
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , Prerender t m
  ) => Dynamic t Game -> Dynamic t Int -> m () 
gameStart dst di = do
  st <- (sample . current) dst
  i <- (sample . current) di 
  if st==newGame || i==1 then let txs = getSections $ T.lines textData
                                  (TS _ tx) = head txs
                               in wakaMain newGame{_txs=txs, _txw=tx}
                         else if i==2 then 
                           let omp = _omp st
                               txs = _txs st
                               (TS _ tx) = head txs
                               nst = if null omp then st{_txw=tx,_itx=True} else st
                            in wakaMain nst 
    else let nomp =  
              [Ob '@' "player" (TLive LStand) T.empty CBlock North (V2 6 3)
              ,Ob 'V' "vaccine1" (TLive (LShoot 4 0)) T.empty CBlock South (V2 1 0)]
          in wakaMain st{_imd=Ext,_msz=V2 12 6,_omp=nomp,_cnn=1,_lif=Just "★★★★★"}

showMapRect :: Game -> T.Text
showMapRect gs =
  let obMap = _omp gs
      tMap = _tmp gs
      mapSize = _msz gs
      mapPos = _mps gs
      isMapShow = _ims gs
   in if isMapShow then putMapInFrame mapWinSize mapPos $ showMap mapSize obMap tMap
                   else T.empty

wakaUpdate :: Game -> WkEvent -> Game
wakaUpdate gs wev =
  let imode = _imd gs
   in case imode of
        End -> gs
        Wai -> let cnn = _cnn gs
                   ncnn = if ncnn>10 then 0 else cnn + 1
                in gs{_imd=if ncnn>10 then Txt else Wai, _cnn=ncnn}
        Txt -> case wev of
          WTick -> let ngs = effectUpdate gs 
                       iths = _iths ngs
                    in if iths then repeatTexUpdate ngs else textUpdate ngs
          WOk -> okButton gs
          _ -> gs
        Cho -> 
          let titles = _cho gs
              tln = length titles
              title = case wev of
                WOk -> if tln>0 then head titles else T.empty 
                WUp -> if tln>1 then titles!!1 else T.empty
                WSub -> if tln>2 then titles!!2 else T.empty
                WLeft -> if tln>3 then titles!!3 else T.empty
                WDown -> if tln>4 then titles!!4 else T.empty
                WRight -> if tln>5 then titles!!5 else T.empty
                _ -> T.empty 
           in if title==T.empty then gs else moveDialog gs{_imd=Txt} title
        Mov -> case wev of 
          WTick -> 
            let ngs = objectUpdate gs 
                cnn = _cnn gs
                omp = _omp gs
                pps = getPosByName "player" omp
                apos = getPosByName "Anna" omp
                ypos = getPosByName "Yoko" omp
                tpos = getPosByName "Tana" omp
                fIsCome (V2 x y) = x^(2::Int) + y^(2::Int) < 10 
                adf = apos - pps
                ydf = ypos - pps
                tdf = tpos - pps
                isCome = fIsCome adf && fIsCome ydf && fIsCome tdf 
             in if isCome || cnn>100 then ngs{_imd=Txt} else ngs{_cnn=cnn+1}
          _     -> gs
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
                          Just tob -> if getObjName tob=="ZBuster"
                                 then shootBullet tob pDir mapSize obMap
                                 else putAction tob pDir mapSize obMap  
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
        Ext -> 
          let obMap = _omp gs
              mapSize = _msz gs
              pDir = getDirByName "player" obMap
              evActs = _evas gs
           in case wev of
             WTick -> 
              let omp = _omp gs
                  msz@(V2 sx _) = _msz gs
                  stg = _stg gs
                  lif = _lif gs
                  cnn = _cnn gs
                  (nomp,(nhs,nstg)) = moveObject stg [] msz omp omp 
                  nlif = if HBullet `elem` nhs then 
                    (\lf -> if lf==T.empty then lf else T.drop 1 lf) <$> lif
                                               else lif
                  nnomp = foldl (\acc ev-> case ev of
                                     HBulletTo nm -> deleteObjByName nm acc 
                                     _ -> acc) nomp nhs
                  isOver = nlif==Just T.empty
                  nimd = if isOver then End else Ext
                  txt = if isOver then "Game Over On Stage "<>(T.pack . show) cnn
                                  else "Stage: "<>(T.pack . show) cnn
                  isNoEnemy = length nnomp == 1
                  ncnn = if isNoEnemy then cnn+1 else cnn
                  nmsz = if isNoEnemy then msz+1 else msz
                  nnnomp = if isNoEnemy then nnomp<>map (\i ->
                    Ob 'V' ("vaccine"<>(T.pack . show) i) (TLive (LShoot 4 0))
                            T.empty CBlock South (V2 (mod ncnn sx) (div ncnn sx)))
                                                                      [1..ncnn]
                                        else nnomp
               in gs{_imd=nimd,_txv=txt,_msz=nmsz,_omp=nnnomp,_stg=nstg
                    ,_lif=nlif,_cnn=ncnn}
             WSub -> gs
             WOk -> 
               let tob = Ob 'b' "ZBuster" TTool T.empty CBlock NoDir (V2 0 0) 
                   (npevs,nomp,_) = shootBullet tob pDir mapSize obMap
                   ngs = gs{ _omp=nomp}
                in exeEvActs ngs npevs evActs
             dirEv -> 
               let mapPos = _mps gs
                   keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir dirEv
                   isSameDir = pDir == keyDir
                   (_,nomp,nmps,_) = if isSameDir 
                      then movePlayer dirEv Nothing mapSize mapWinSize mapPos obMap
                      else ([],updateDirByName "player" keyDir obMap,mapPos,Nothing)
                in gs{_omp=nomp,_mps=nmps} 

objectUpdate :: Game -> Game
objectUpdate gs = let omp = _omp gs
                      msz = _msz gs
                      stg = _stg gs
                      lif = _lif gs
                      evas = _evas gs
                      (nomp,(nhs,nstg)) = moveObject stg [] msz omp omp 
                      nlif = if HBullet `elem` nhs then 
                        (\lf -> if lf==T.empty then lf else T.drop 1 lf) <$> lif
                                                   else lif
                      isShootZ = HBulletTo "ZVaccine" `elem` nhs
                      pev = [PNoLife | nlif==Just T.empty] <>
                            [PShoot "ZVaccine" | isShootZ]
                      ngs = exeEvActs gs{_omp=nomp,_stg=nstg,_lif=nlif} pev evas 
                   in ngs

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
      objData = lookupFromSections txs titleO
      newMapData = updateMapData mapData lomp
      newObjData = T.unlines $ nub $ makeObjectDatas lomp <> T.lines objData
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
            (isStop,isTyping,isCode,isRubi,targetChar,codeText,rubiText,scanLength)
              = getInfoFromChar wholeText textCount
            nitx = not isStop && isText && isTextShowing
            ntxv = if isTyping then if isRubi 
                                        then textView <> makeRubiHtml rubiText
                                        else textView <> T.singleton targetChar
                               else textView
            ntct = textCount + scanLength 
            ngs0 = pgs{_itx=nitx, _txv=ntxv, _tct=ntct, _tcs=_tcs pgs + 1}
            ngs1 = if isCode then exeCode ngs0 codeText else ngs0
         in ngs1
                                else 
         if isTextShowing then pgs else exeCode pgs "sc_0 sp"

