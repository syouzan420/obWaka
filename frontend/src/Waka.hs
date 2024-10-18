module Waka (loadGame) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2(..))
import qualified Data.Map as M 
import Data.List (nub)
import Data.Functor (void)
import Data.Maybe (isNothing,isJust,fromMaybe)
import qualified Data.Text as T
import Reflex.Dom.Core 
  ( dynText, current, gate, blank, elAttr, el, text
  , accumDyn, divClass, leftmost, (=:), zipDynWith , sample, elDynAttr
  , tickLossyFromPostBuildTime, widgetHold_, toggle, holdDyn 
  , prerender_, elDynHtmlAttr', elDynHtml' 
  , DomBuilder, MonadHold, PostBuild, Prerender
  , Performable, PerformEvent, TriggerEvent
  , Dynamic, Event
  )

import CWidget (elChara,elSpace,evElButton,evElButtonH,elTextScroll
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

makeLenses ''Game

(*.) :: Functor f => f s -> Getting b s b -> f b
(*.) a b = a <&> (^. b)

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
    let beIsSave = fmap (\et -> et==Save || et==LSave) (current (dyGs*.etr)) 
    let evWTick = WTick <$ evTime
    let evWk = leftmost (evBtList<>[evWTick])
    let evBt = leftmost evBtList 
    let evTxTime = gate (current dyTxtOn) evTime
    let evSave = gate beIsSave evWTick 
    dyGs <- accumDyn wakaUpdate gs evWk
    let dyLife = dyGs*.lif  
    let dyAtr = fmap (\t -> ("href" =: t)::M.Map T.Text T.Text) (dyGs*.lnu) 
    let dyIsShowLife = isJust <$> dyLife
    let dyHide = mkHidden <$> dyIsShowLife
    let dyTxtOn = 
          zipDynWith (\a b -> a && (b==Txt || b==Cho)) (dyGs*.itx) (dyGs*.imd)
    let dyDir = dirToText . getDirByName "player" <$> (dyGs*.omp)
    divClass "flexbox" $ do
      el "div" $ elChara (dyGs*.chn) 
      divClass "kaimap" $ do
         elDynAttr "div" dyHide $ divClass "cen" $ 
                  dynText (fmap (fromMaybe T.empty) dyLife) 
         prerender_ blank $ void $ 
            elDynHtml' "div" $ showMapRect <$> dyGs 
      divClass "kai" $ do
         dynText $ fmap (<>"\n") dyDir 
         dynText $ (dyGs*.hav) <&> 
            \case Just hv -> ">"<>getObjName hv; Nothing -> T.empty 
    elSpace

--    let dyOmp = _omp <$> dyGs
--    dynText $ T.pack . show <$> dyOmp

    divClass "tbox" $  
      prerender_ blank $ void $ 
        elDynHtmlAttr' "div" ("id"=: "wkText" <> "class" =: "tate") (dyGs*.txv) 
    elSpace  
    evBtList <- evWkButtons
    divClass "lnk" $ elDynAttr "a" dyAtr $ dynText (dyGs*.lnt) 
    widgetHold_ blank (elVibration <$ evBt)
    widgetHold_ blank (elTextScroll <$ evTxTime)
    widgetHold_ blank (saveGame dyGs <$ evSave)

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

saveGame :: (DomBuilder t m, Prerender t m, MonadHold t m) => Dynamic t Game -> m ()
saveGame dyGs = do 
  gs <- sample (current dyGs)
  let ntxs = getSections $ T.lines textData
  let ngs = if gs^.etr==LSave then newGame{_txs= ntxs, _gmc=gs^.gmc+1} else gs
  (saveState . makeGameStateText) ngs

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
        divClass "kai" $ text gameTitle
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
  if st==newGame || i==1 
    then let textSections = getSections $ T.lines textData
             (TS _ tx) = head textSections 
          in wakaMain newGame{_txs=textSections, _txw=tx}
    else if i==2 then 
            let (TS _ tx) = head (st^.txs)
                nst = if null (st^.omp) then st{_txw=tx,_itx=True} else st
             in wakaMain nst 
    else let nomp =  
              [Ob pChar "player" (TLive LStand) T.empty CBlock North Orange (V2 6 3)
              ,Ob 'V' "vaccine1" (TLive (LShoot 4 0)) T.empty CBlock South Black (V2 1 0)]
          in wakaMain st{_imd=Ext,_msz=V2 12 6,_omp=nomp,_cnn=1,_lif=Just "★★★★★"}

showMapRect :: Game -> T.Text
showMapRect gs = if gs^.ims then putMapInFrame mapWinSize (gs^.mps) 
                                $ showMap (gs^.msz) (gs^.omp) (gs^.tmp) 
                            else T.empty

wakaUpdate :: Game -> WkEvent -> Game
wakaUpdate gs wev =
   case gs^.imd of
        End -> gs
        Wai -> let ncnn = if ncnn>10 then 0 else gs^.cnn + 1
                in gs{_imd=if ncnn>10 then Txt else Wai, _cnn=ncnn}
        Txt -> case wev of
          WTick -> let ngs = effectUpdate gs 
                    in if ngs^.iths then repeatTexUpdate ngs else textUpdate ngs
          WOk -> okButton gs
          _ -> gs
        Cho -> 
          let titles = gs^.cho
              tln = length titles
              cNum = lookup wev 
                        (zip [WRight,WLeft,WUp,WDown,WOk,WSub] [(0::Int)..]) 
              title = case cNum of
                Just cn -> if tln>cn then titles!!cn else T.empty 
                _ -> T.empty
           in if title==T.empty then gs else moveDialog gs{_imd=Txt} title
        Mov -> case wev of 
          WTick -> 
            let ngs = objectUpdate gs 
                count = gs^.cnn
                obMap = gs^.omp
                pps = getPosByName "player" obMap
                apos = getPosByName "Anna" obMap
                ypos = getPosByName "Yoko" obMap
                tpos = getPosByName "Tana" obMap
                fIsCome (V2 x y) = x^(2::Int) + y^(2::Int) < 10 
                adf = apos - pps
                ydf = ypos - pps
                tdf = tpos - pps
                isCome = fIsCome adf && fIsCome ydf && fIsCome tdf 
             in if isCome || count>100 then ngs{_imd=Txt} else ngs{_cnn=count+1}
          _     -> gs
        Ply -> 
          let obMap = gs^.omp
              mapSize = gs^.msz
              pHave = gs^.hav    
              pDir = getDirByName "player" obMap
              evActs = gs^.evas
           in case wev of
             WTick -> objectUpdate $ effectUpdate gs
             WSub -> 
               let ntxw = ";cho_セーブ_textSave_もどる_textBack"
                in gs{_imd=Txt, _itx=True, _txw=ntxw, _txv=T.empty, _tct=0}
             WOk -> 
               let tmpMap = gs^.tmp
                   ntmp = if isNothing pHave 
                             then hitAction "player" mapSize obMap tmpMap
                             else tmpMap
                   (npevs,nomp,nphv) = case pHave of
                          Nothing -> attackAction (gs^.txs) pDir (gs^.mnm) obMap 
                          Just tob -> if getObjName tob=="ZBuster"
                                 then shootBullet tob pDir mapSize obMap
                                 else putAction tob pDir mapSize obMap  
                   ngs = gs{_tmp=ntmp, _omp=nomp, _hav=nphv}
                in exeEvActs ngs npevs evActs
             dirEv -> 
               let mapPos = gs^.mps
                   keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir dirEv
                   isSameDir = pDir == keyDir
                   (npevs,nomp,nmps,nphv) = if isSameDir 
                      then movePlayer dirEv pHave mapSize mapWinSize mapPos obMap
                      else ([],updateDirByName "player" keyDir obMap,mapPos,pHave)
                   ngs = exeEvActs gs{_omp=nomp,_mps=nmps,_hav=nphv} npevs evActs
                   ngs2 = enterNewMap ngs npevs
                in ngs2 
        Ext -> 
          let obMap = gs^.omp
              mapSize = _msz gs
              pDir = getDirByName "player" obMap
              evActs = _evas gs
           in case wev of
             WTick -> 
              let mapSz@(V2 sx _) = gs^.msz
                  life = gs^.lif
                  count = gs^.cnn
                  (nomp,(nhs,nstg)) = moveObject (gs^.stg) [] mapSz obMap obMap 
                  nlif = if HBullet `elem` nhs then 
                    (\lf -> if lf==T.empty then lf else T.drop 1 lf) <$> life
                                               else life
                  nnomp = foldl (\acc ev-> case ev of
                                     HBulletTo nm -> deleteObjByName nm acc 
                                     _ -> acc) nomp nhs
                  isOver = nlif==Just T.empty
                  nimd = if isOver then End else Ext
                  txt = if isOver then "Game Over On Stage "<>(T.pack . show) count
                                  else "Stage: "<>(T.pack . show) count
                  isNoEnemy = length nnomp == 1
                  ncnn = if isNoEnemy then count+1 else count
                  nmsz = if isNoEnemy then mapSz+1 else mapSz
                  nnnomp = if isNoEnemy then nnomp<>map (\i ->
                    Ob 'V' ("vaccine"<>(T.pack . show) i) (TLive (LShoot 4 0))
                      T.empty CBlock South Black (V2 (mod ncnn sx) (div ncnn sx)))
                                                                      [1..ncnn]
                                        else nnomp
               in gs{_imd=nimd,_txv=txt,_msz=nmsz,_omp=nnnomp,_stg=nstg
                    ,_lif=nlif,_cnn=ncnn}
             WSub -> gs
             WOk -> 
               let tob = Ob 'b' "ZBuster" TTool T.empty CBlock NoDir Black (V2 0 0) 
                   (npevs,nomp,_) = shootBullet tob pDir mapSize obMap
                   ngs = gs{ _omp=nomp}
                in exeEvActs ngs npevs evActs
             dirEv -> 
               let mapPos = gs^.mps
                   keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir dirEv
                   isSameDir = pDir == keyDir
                   (_,nomp,nmps,_) = if isSameDir 
                      then movePlayer dirEv Nothing mapSize mapWinSize mapPos obMap
                      else ([],updateDirByName "player" keyDir obMap,mapPos,Nothing)
                in gs{_omp=nomp,_mps=nmps} 

objectUpdate :: Game -> Game
objectUpdate gs = 
  let obMap = gs^.omp
      life = gs^.lif
      (nomp,(nhs,nstg)) = moveObject (gs^.stg) [] (gs^.msz) obMap obMap 
      nlif = if HBullet `elem` nhs then 
                (\lf -> if lf==T.empty then lf else T.drop 1 lf) <$> life
                                   else life
      isShootZ = HBulletTo "ZVaccine" `elem` nhs
      pev = [PNoLife | nlif==Just T.empty] <>
                            [PShoot "ZVaccine" | isShootZ]
      ngs = exeEvActs gs{_omp=nomp,_stg=nstg,_lif=nlif} pev (gs^.evas) 
   in ngs

enterNewMap :: Game -> [PEvent] -> Game 
enterNewMap gs [] = gs 
enterNewMap gs (PEnter pps oname:_) = 
  let obMap = gs^.omp
      obj = getObjByName oname obMap
      tdf = maybe T.empty getObjDef obj
   in setMap gs{_pmp = (gs^.mnm,pps,obMap)} 
                        (if tdf==T.empty then "0" else T.drop 3 tdf) 
enterNewMap gs (PLeave:_) =
  let textSections = gs^.txs
      mapName = gs^.mnm
      lomp = deleteObjByName "player" (gs^.omp)
      titleM = "map"<>mapName
      titleO = "obj"<>mapName
      mapData = lookupFromSections textSections titleM 
      objData = lookupFromSections textSections titleO
      newMapData = updateMapData mapData lomp
      newObjData = T.unlines $ nub $ makeObjectDatas lomp <> T.lines objData
      ntxs = updateTextSection (TS titleO newObjData) $ 
                          updateTextSection (TS titleM newMapData) textSections 
      (tmnm,tps,obMap) = gs^.pmp 
      ngs = setMap gs tmnm
      nomp = updatePosByName "player" tps obMap
      mpos = setMapStartPos tps mapWinSize (ngs^.msz) 
   in ngs{_txs=ntxs, _mps=mpos, _omp=nomp}
enterNewMap gs (_:xs) = enterNewMap gs xs


exeEvActs :: Game -> [PEvent] -> [EvAct] -> Game
exeEvActs gs [] nevas = gs{_evas = nevas} 
exeEvActs gs (pe:pes) evActs = 
  let (nevas,ncodes) = getCodes pe ([],[]) evActs
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
  let niths = gs^.itx && not (gs^.iths) 
   in gs{_itx=gs^.imd==Txt,_tcs=0,_iths=niths,_etr=NoEvent} 


scanEffect :: ObMap -> ObMap
scanEffect [] = []
scanEffect (ob@(Ob ch nm tp df oc dr co ps):xs)
  | ch==eAt0 = Ob eAt1 nm tp df oc dr co ps:scanEffect xs
  | ch==eAt1 = scanEffect xs
  | otherwise = ob:scanEffect xs

effectUpdate :: Game -> Game
effectUpdate gs = gs{_tmp = scanEffect (_tmp gs)}

repeatTexUpdate :: Game -> Game
repeatTexUpdate gs = 
      if gs^.itx then repeatTexUpdate (textUpdate gs) else gs{_iths=False}

textUpdate :: Game -> Game
textUpdate gs =
  let isText = gs^.itx
      eventTrigger = gs^.etr
      wholeText = gs^.txw
      textCount = gs^.tct
      textLength = T.length wholeText 
      isTextShowing = textCount < textLength
      pgs = if eventTrigger==NoEvent then gs else gs{_etr=NoEvent}
   in if isText && isTextShowing then 
        let textView = _txv pgs
            (isTyping,dtype,targetChar,doc,scanLength)
              = getInfoFromChar wholeText textCount
            isStop = dtype == DStop
            isCode = dtype == DCode
            isRubi = dtype == DRubi
            nitx = not isStop && isText && isTextShowing
            ntxv = if isTyping then if isRubi 
                                        then textView <> makeRubiHtml doc 
                                        else textView <> T.singleton targetChar
                               else textView
            ntct = textCount + scanLength 
            ngs0 = pgs{_itx=nitx, _txv=ntxv, _tct=ntct, _tcs=pgs^.tcs + 1}
            ngs1 = if isCode then exeCode ngs0 doc else ngs0
         in ngs1
                                else 
         if isTextShowing then pgs else exeCode pgs "sc_0 sp"
