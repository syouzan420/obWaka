module Code(exeCode,setMap,moveDialog) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe,isJust)
import Data.List (uncons,deleteBy,find)
import Linear.V2 (V2(..))
import Converter (makeObjectMap,setObjectData,setMapStartPos
                 ,lookupFromSections,makeObjectByName,updateTextSection
                 ,updateObjectData,txToObject)
import Object (getPosByName,getObjName,putObjOnPos,putablePos,updateDirByName
              ,deleteObjByPos,updateDefByName,updateObjByName,getObjByName
              ,updatePosByName,deleteObjByName,isInMap,isObjOnPos,getObjDef)
import Define

exeCode :: Game -> T.Text -> Game 
exeCode gs evt = do 
  let etxs = T.split (==' ') evt
   in foldl exeOneCode gs etxs

exeOneCode :: Game -> T.Text -> Game 
exeOneCode gs evt = do
  let en_ags = T.split (=='_') evt
      (en,ags) = fromMaybe ("null",[]) (uncons en_ags)
   in if null ags then case en of
    "sp" -> setPlayer gs
    "cn" -> consumeItem gs
    "sl" -> showLife gs
    "dm" -> deleteMap gs
    "hm" -> hideMap gs
    "rm" -> revealMap gs
    "save" -> gs{_etr=Save}
    "end" -> endGame gs
    _ -> gs 
                    else case en of
    "a" -> setEventAction gs (head ags) (T.intercalate "_" (tail ags)) 
    "d" -> delEventActions gs ags
    "if" -> exeCondition gs ags
    "md" -> moveDialog gs (head ags)
    "sm" -> setMap gs (head ags)
    "sc" -> showChara gs (head ags)
    "cho" -> choiceDialog gs ags
    "cd" -> changeDir gs (head ags)
    "p" -> putObject gs ags
    "co" -> changeObject gs (head ags)
    "ac" -> addCounter gs (head ags)
    "ud" -> updateDef gs (head ags)
    "uo" -> updateObject gs (head ags)
    "gt" -> getItem gs (head ags)
    "sp" -> setPosition gs (head ags)
    "cm" -> changeMode gs (head ags)
    "hl" -> hyperLink gs (head ags)
    "em" -> enterMap gs (head ags)
    _ -> gs 

revealMap :: Game -> Game
revealMap gs = gs{_ims=True}

hideMap :: Game -> Game
hideMap gs = gs{_ims=False}

enterMap :: Game -> T.Text -> Game
enterMap gs oname =
  let omp = _omp gs
      mnm = _mnm gs
      msz = _msz gs
      ops = getPosByName oname omp
      ppsList d = [ops+V2 d 0,ops+V2 0 (-d),ops+V2 (-d) 0,ops+V2 0 d]
      canPutPS 3 [] = V2 0 0
      canPutPS d [] = canPutPS (d+1) (ppsList (d+1)) 
      canPutPS d (p:xs) = if isInMap p msz && not (isObjOnPos p omp) then p else
                              canPutPS d xs
      resPos = canPutPS 1 (ppsList 1)
      obj = getObjByName oname omp
      tdf = maybe T.empty getObjDef obj
   in setMap gs{_pmp = (mnm,resPos,omp)} (if tdf==T.empty then "0" else T.drop 3 tdf) 

hyperLink :: Game -> T.Text -> Game
hyperLink gs tx =
  let lnTx = T.splitOn "-" tx
      (lnu,lnt) = case lnTx of
                    [ln,txt] -> (T.replace "+" ":" (T.replace "|" "_" ln),txt)
                    _ -> (T.empty,T.empty)
   in gs{_lnu=lnu, _lnt=lnt}

endGame :: Game -> Game
endGame gs =
  let omp = _omp gs
      mps = _mps gs
      nomp = deleteObjByName "player" omp
      V2 ex ey = mps + V2 5 2
      showT = T.pack . show
      putList = ["E"<>"."<>showT ex<>"."<>showT ey
                ,"N"<>"."<>showT (ex+1)<>"."<>showT ey 
                ,"D"<>"."<>showT (ex+2)<>"."<>showT ey]
      ngs = putObject gs{_omp=nomp} putList
   in moveDialog ngs{_etr=LSave} "textEnd"

textMode :: [(T.Text,IMode)]
textMode = [("txt",Txt),("cho",Cho),("mov",Mov),("ply",Ply),("end",End)]

changeMode :: Game -> T.Text -> Game
changeMode gs tx =
  let md = fromMaybe Txt $ lookup tx textMode
   in gs {_imd = md}

setPosition :: Game -> T.Text -> Game
setPosition gs tx =
  let nmpos = T.splitOn "." tx
      omp = _omp gs
      nomp = case nmpos of
        [oname,px,py] -> updatePosByName oname 
                          (V2 ((read . T.unpack) px) ((read . T.unpack) py)) omp
        _ -> omp
   in gs {_omp = nomp} 

getItem :: Game -> T.Text -> Game
getItem gs nm =
  if isJust (_hav gs) then gs else
        let mnm = _mnm gs
            txs = _txs gs
            obDatas = T.lines $ lookupFromSections txs ("obj"<>mnm)
            obj = makeObjectByName nm obDatas
         in gs{_hav=obj} 

updateObject :: Game -> T.Text -> Game
updateObject gs tx =
  let mnObData = T.splitOn "." tx 
   in case mnObData of
        [mnm,oname,odt] ->
          let mnmNow = _mnm gs 
              omp = _omp gs
              pmp@(pmnm,pmps,pomp) = _pmp gs
              txs = _txs gs
              title = "obj"<>mnm
              obList = T.lines $ lookupFromSections txs title 
              getName name od = T.take (T.length name) $ T.drop 2 od 
              tgCh = maybe ' ' T.head (find (\dt -> oname==getName oname dt) obList)
              newObList = deleteBy (\nm1 nm2 -> nm1==getName oname nm2) 
                                                                  oname obList
              newObData = T.singleton tgCh<>","<>oname<>","<>T.replace "-" " " odt
              newOb = txToObject newObData 
              nomp = if mnmNow==mnm then updateObjByName oname newOb omp else omp
              npmp = if pmnm==mnm 
                      then (pmnm,pmps,updateObjByName oname newOb pomp) else pmp  
              newObText = T.unlines $ newObData :newObList 
              newTxs = updateTextSection (TS title newObText) txs
           in gs{_txs=newTxs,_omp=nomp,_pmp=npmp}
        _ -> gs


updateDef :: Game -> T.Text -> Game
updateDef gs tx =
  let omp = _omp gs
      mnm = _mnm gs
      txs = _txs gs
      title = "obj"<>mnm
      objTxt = lookupFromSections txs title 
      nmDef = T.splitOn "." tx
      (nomp,nob) = case nmDef of 
          [nm,df] -> (updateDefByName nm df omp, getObjByName nm omp)
          _ -> (omp,Nothing)
      nObjTxt = case nob of
        Just ob -> updateObjectData objTxt ob
        Nothing -> objTxt
      ntxs = case nob of
        Just _ -> updateTextSection (TS title nObjTxt) txs 
        Nothing -> txs
   in gs{_txs=ntxs, _omp=nomp} 

addCounter :: Game -> T.Text -> Game
addCounter gs tx = 
  let cnts = _cnts gs
      c = fromMaybe 0 $ lookup tx cnts
      ncnts  = if c==0 then (tx,1):cnts 
                       else (tx,c+1):
                         deleteBy (\(snm,_) (sn,_) -> snm==sn) (tx,0) cnts
   in gs{_cnts = ncnts}

changeObject :: Game -> T.Text -> Game
changeObject gs tx =
  let omp = _omp gs
      names = T.splitOn "-" tx
   in case names of 
        [nm,tnm] -> 
          let ps@(V2 px py) = getPosByName nm omp
              nomp = deleteObjByPos ps omp
              namePos = tnm<>"."<>(T.pack . show) px <>"."<>(T.pack . show) py
           in putObject gs{_omp=nomp} [namePos]
        _ -> gs

changeDir :: Game -> T.Text -> Game
changeDir gs tx =
  let dir = case tx of "x" -> NoDir; "e" -> East; "n" -> North
                       "w" -> West; "s" -> South; _ -> NoDir
      nomp = updateDirByName "player" dir (_omp gs)
   in gs{_omp = nomp}

putObject :: Game -> [T.Text] -> Game
putObject gs [] = gs
putObject gs (objNamePos:xs) =
  let npData = T.splitOn "." objNamePos
   in case npData of
        [oname,opx,opy] -> 
          let textSections = _txs gs
              mnm = _mnm gs
              omp = _omp gs
              obDatas = T.lines $ lookupFromSections textSections ("obj"<>mnm)
              obj = makeObjectByName oname obDatas
              nomp = case obj of
                      Just ob -> 
                        let msz = _msz gs
                            tps = V2 ((read. T.unpack) opx) ((read . T.unpack) opy)
                            nps = putablePos tps msz omp
                         in putObjOnPos ob nps omp 
                      Nothing -> omp
           in putObject gs{_omp=nomp} xs
        _ -> putObject gs xs

consumeItem :: Game -> Game
consumeItem gs = gs{_hav = Nothing}

setPlayer :: Game -> Game 
setPlayer gs = gs{_itx=False,_imd = Ply} 

showLife :: Game -> Game
showLife gs = gs{_lif = Just "★★★★★"}

setEventAction :: Game -> T.Text -> Code -> Game 
setEventAction gs ead pcd = 
  let eaData = T.splitOn "." ead
   in case eaData of
        ["leave"] -> gs{_evas = EA PLeave pcd 1 0:_evas gs} 
        ["nolife"] -> gs{_evas = EA PNoLife pcd 2 0:_evas gs}
        [act,dt,num] -> 
          let pev 
               | elem act txsByName = 
                    fromMaybe PNon $ lookup act (txPevByName dt)
               | act=="pushto" = 
                    let (nm,aonm) = T.breakOn "-" dt
                     in PPushTo nm (T.tail aonm)
               | otherwise = PNon
              ea = if pev==PNon then EA PNon T.empty 0 0
                                else EA pev pcd ((read . T.unpack) num) 0
          in gs{_evas = ea:_evas gs} 
        _ -> gs

txsByName :: [T.Text]
txsByName = ["block","push","get","on","consume","attack","shoot"]

txPevByName :: ObName -> [(T.Text,PEvent)]
txPevByName nm = [("block",PBlock nm),("push",PPush nm),("get",PGet nm)
        ,("on",POn nm),("consume",PConsume nm),("attack",PAttack nm)
        ,("shoot",PShoot nm)]

delEventActions :: Game -> [T.Text] -> Game
delEventActions gs [] = gs
delEventActions gs (ead:xs) =
  let evas = _evas gs
      eaData = T.splitOn "." ead
   in case eaData of 
        [act,dt] ->
          let pev 
               | elem act txsByName = 
                    fromMaybe PNon $ lookup act (txPevByName dt)
               | otherwise = PNon
              nevas = filter (\(EA pe _ _ _) -> pe/=pev) evas 
           in delEventActions gs{_evas=nevas} xs 
        _ -> gs

exeCondition :: Game -> [T.Text] -> Game
exeCondition gs [] = gs
exeCondition gs (ag:xs)
  | ag=="if" || ag=="el" = exeCondition gs xs
  | hag =='?' = let cnData = T.splitOn "." tag
                    bool = case cnData of
                      [cnd,tgt] -> conditions cnd tgt gs 
                      _ -> False
                 in case xs of
                      (_:xxs) -> if bool then exeCondition gs xs
                                          else exeCondition gs xxs
                      _ -> gs
  | otherwise = let cd = T.replace "." "_" ag 
                 in exeCode gs cd
  where (hag,tag) = fromMaybe (' ',T.empty) $ T.uncons ag

conditions :: T.Text -> T.Text -> Game -> Bool
conditions "pHave" tgt gs =
  let ob = _hav gs
      obnm = maybe T.empty getObjName ob
   in obnm == tgt || (tgt=="any" && isJust ob)
conditions "counter" tgt gs =
  let scs = T.splitOn "," tgt
      cnts = _cnts gs
      scps = makeScPair scs
   in isMatchCount cnts scps
conditions _ _ _ = False

isMatchCount :: [Counter] -> [Counter] -> Bool
isMatchCount _ [] = True
isMatchCount cnts ((s,c):xs) =
  let co = fromMaybe 0 $ lookup s cnts 
   in (co >= c) && isMatchCount cnts xs

makeScPair :: [T.Text] -> [(T.Text,Int)]
makeScPair [] = []
makeScPair [_] = []
makeScPair (s:c:xs) = (s,(read . T.unpack) c):makeScPair xs


setMap :: Game -> T.Text -> Game 
setMap gs mnm = 
  let textSections = _txs gs 
      obMapText = lookupFromSections textSections ("map" <> mnm)
      nmnm = if obMapText==T.empty then T.empty else mnm
      (obMapPre,mapSize) = makeObjectMap obMapText
      objData = lookupFromSections textSections ("obj" <> mnm)
      obMap = if objData==T.empty then obMapPre 
                                  else setObjectData (T.lines objData) obMapPre
      pps = getPosByName "player" obMap 
      mpos = setMapStartPos pps mapWinSize mapSize
   in gs{_mnm = nmnm, _msz = mapSize, _mps = mpos, _omp = obMap, _ims=True}
   
deleteMap :: Game -> Game
deleteMap gs = gs{_omp=[], _tmp=[], _mnm=T.empty, _msz=V2 0 0, _mps=V2 0 0}

choiceDialog :: Game -> [T.Text] -> Game
choiceDialog gs args = 
  choiceDialog' gs{_imd=Cho,_cho=[],_txv=_txv gs<>"\n",_tct=_tct gs + 1}
                                          args ["●","←","□","↑","→","↓"] 

choiceDialog' :: Game -> [T.Text] -> [T.Text] -> Game
choiceDialog' gs [] _ = gs
choiceDialog' gs [_] _ = gs
choiceDialog' gs _ [] = gs
choiceDialog' gs (tx:title:xs) (hd:ys) = let tlen = T.length tx + 3
  in choiceDialog' gs{_cho=_cho gs<>[title],_txv=_txv gs<>hd<>" "<>tx<>"\n"
                     ,_tct=_tct gs + tlen} xs ys

moveDialog :: Game -> T.Text -> Game 
moveDialog gs title = 
  let textSections = _txs gs 
      newText = lookupFromSections textSections title
   in if newText==T.empty then gs else 
    gs{_imd=Txt, _itx=True, _tct=0, _txw=newText, _txv=T.empty
      ,_lnu=T.empty, _lnt=T.empty}

showChara :: Game -> T.Text -> Game
showChara gs chn = gs{_chn = (read . T.unpack) chn}
