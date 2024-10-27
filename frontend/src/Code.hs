module Code(exeCode,setMap,moveDialog) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe,isJust)
import Data.List (uncons,deleteBy,find)
import Linear.V2 (V2(..))
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Converter (makeObjectMap,setObjectData,setMapStartPos
                 ,lookupFromSections,makeObjectByName,updateTextSection
                 ,updateObjectData,txToObject)
import Object (getPosByName,getObjName,putObjOnPos,putablePos,updateDirByName
              ,deleteObjByPos,updateDefByName,updateObjByName,getObjByName
              ,updatePosByName,deleteObjByName,isInMap,isObjOnPos,getObjDef)
import Define

makeLenses ''Game

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
    "ip" -> inputText gs ags
    _ -> gs 

revealMap :: Game -> Game
revealMap gs = gs&ims.~ True

hideMap :: Game -> Game
hideMap gs = gs&ims.~ False

enterMap :: Game -> T.Text -> Game
enterMap gs oname =
  let obMap = gs^.omp
      ops = getPosByName oname obMap
      ppsList d = [ops+V2 d 0,ops+V2 0 (-d),ops+V2 (-d) 0,ops+V2 0 d]
      canPutPS 3 [] = V2 0 0
      canPutPS d [] = canPutPS (d+1) (ppsList (d+1)) 
      canPutPS d (p:xs) = if isInMap p (gs^.msz) && not (isObjOnPos p obMap) then p else
                              canPutPS d xs
      resPos = canPutPS 1 (ppsList 1)
      obj = getObjByName oname obMap
      tdf = maybe T.empty getObjDef obj
   in setMap (gs&pmp.~ (gs^.mnm,resPos,obMap)) (if tdf==T.empty then "0" else T.drop 3 tdf) 

hyperLink :: Game -> T.Text -> Game
hyperLink gs tx =
  let lnTx = T.splitOn "-" tx
      (lnUrl,lnTxt) = case lnTx of
                    [ln,txt] -> (T.replace "+" ":" (T.replace "|" "_" ln),txt)
                    _ -> (T.empty,T.empty)
   in gs &lnu.~lnUrl &lnt.~lnTxt

endGame :: Game -> Game
endGame gs =
  let nomp = deleteObjByName "player" (gs^.omp) 
      V2 ex ey = (gs^.mps) + V2 5 2
      showT = T.pack . show
      putList = ["E"<>"."<>showT ex<>"."<>showT ey
                ,"N"<>"."<>showT (ex+1)<>"."<>showT ey 
                ,"D"<>"."<>showT (ex+2)<>"."<>showT ey]
      ngs = putObject gs{_omp=nomp} putList
   in moveDialog (ngs&etr.~ LSave) "textEnd"

textMode :: [(T.Text,IMode)]
textMode = [("txt",Txt),("cho",Cho),("mov",Mov),("ply",Ply),("end",End)]

changeMode :: Game -> T.Text -> Game
changeMode gs tx =
  let md = fromMaybe Txt $ lookup tx textMode
   in gs&imd.~ md

setPosition :: Game -> T.Text -> Game
setPosition gs tx =
  let obMap = gs^.omp
      nmpos = T.splitOn "." tx
      nomp = case nmpos of
        [oname,px,py] 
          -> updatePosByName oname 
                   (V2 ((read . T.unpack) px) ((read . T.unpack) py)) obMap 
        _ -> obMap
   in gs&omp.~ nomp 

getItem :: Game -> T.Text -> Game
getItem gs nm =
  if isJust (_hav gs) then gs else
        let obDatas = T.lines $ lookupFromSections (gs^.txs) ("obj"<>(gs^.mnm))
            obj = makeObjectByName nm obDatas
         in gs&hav.~ obj 

updateObject :: Game -> T.Text -> Game
updateObject gs tx =
  let mnObData = T.splitOn "." tx 
   in case mnObData of
        [mapNm,oname,odt] ->
          let mnmNow = gs^.mnm 
              obMap = gs^.omp
              preMap@(pmnm,pmps,pomp) = gs^.pmp
              textSections = gs^.txs
              title = "obj"<>mapNm
              obList = T.lines $ lookupFromSections textSections title 
              getName name od = T.take (T.length name) $ T.drop 2 od 
              tgCh = maybe ' ' T.head (find (\dt -> oname==getName oname dt) obList)
              newObList = deleteBy (\nm1 nm2 -> nm1==getName oname nm2) 
                                                                  oname obList
              newObData = T.singleton tgCh<>","<>oname<>","<>T.replace "-" " " odt
              newOb = txToObject newObData 
              nomp = if mnmNow==mapNm then updateObjByName oname newOb obMap 
                                       else obMap
              npmp = if pmnm==mapNm 
                      then (pmnm,pmps,updateObjByName oname newOb pomp) 
                      else preMap 
              newObText = T.unlines $ newObData :newObList 
              newTxs = updateTextSection (TS title newObText) textSections
           in gs&txs.~ newTxs &omp.~ nomp &pmp.~npmp
        _ -> gs

updateDef :: Game -> T.Text -> Game
updateDef gs tx =
  let obMap = gs^.omp
      textSections = gs^.txs
      title = "obj"<>(gs^.mnm)
      objTxt = lookupFromSections textSections title 
      nmDef = T.splitOn "." tx
      (nomp,nob) = case nmDef of 
          [nm,df] -> (updateDefByName nm df obMap, getObjByName nm obMap)
          _ -> (obMap,Nothing)
      nObjTxt = case nob of
        Just ob -> updateObjectData objTxt ob
        Nothing -> objTxt
      ntxs = case nob of
        Just _ -> updateTextSection (TS title nObjTxt) textSections 
        Nothing -> textSections 
   in gs &txs.~ ntxs &omp.~ nomp 

addCounter :: Game -> T.Text -> Game
addCounter gs tx = 
  let counters = gs^.cnts
      c = fromMaybe 0 $ lookup tx counters 
      ncnts  = if c==0 then (tx,1):counters 
                       else (tx,c+1):
                         deleteBy (\(snm,_) (sn,_) -> snm==sn) (tx,0) counters
   in gs&cnts.~ ncnts

changeObject :: Game -> T.Text -> Game
changeObject gs tx =
  let obMap = gs^.omp
      names = T.splitOn "-" tx
   in case names of 
        [nm,tnm] -> 
          let ps@(V2 px py) = getPosByName nm obMap
              nomp = deleteObjByPos ps obMap
              namePos = tnm<>"."<>(T.pack . show) px <>"."<>(T.pack . show) py
           in putObject (gs&omp.~ nomp) [namePos]
        _ -> gs

changeDir :: Game -> T.Text -> Game
changeDir gs tx =
  let dir = case tx of "x" -> NoDir; "e" -> East; "n" -> North
                       "w" -> West; "s" -> South; _ -> NoDir
      nomp = updateDirByName "player" dir (_omp gs)
   in gs&omp.~nomp

putObject :: Game -> [T.Text] -> Game
putObject gs [] = gs
putObject gs (objNamePos:xs) =
  let npData = T.splitOn "." objNamePos
   in case npData of
        [oname,opx,opy] -> 
          let obMap = gs^.omp
              obDatas = T.lines $ lookupFromSections (gs^.txs) ("obj"<>(gs^.mnm))
              obj = makeObjectByName oname obDatas
              nomp = case obj of
                      Just ob -> 
                        let msz = _msz gs
                            tps = V2 ((read. T.unpack) opx) ((read . T.unpack) opy)
                            nps = putablePos tps msz obMap
                         in putObjOnPos ob nps obMap 
                      Nothing -> obMap
           in putObject (gs&omp.~ nomp) xs
        _ -> putObject gs xs

consumeItem :: Game -> Game
consumeItem gs = gs&hav.~Nothing

setPlayer :: Game -> Game 
setPlayer gs = gs& itx.~ False &imd.~ Ply 

showLife :: Game -> Game
showLife gs = gs&lif?~ "★★★★★"

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
          in gs&evas.~ ea:_evas gs 
        _ -> gs

txsByName :: [T.Text]
txsByName = map fst (txPevByName T.empty) 

txPevByName :: ObName -> [(T.Text,PEvent)]
txPevByName nm = [("block",PBlock nm),("push",PPush nm),("get",PGet nm)
        ,("on",POn nm),("consume",PConsume nm),("attack",PAttack nm)
        ,("shoot",PShoot nm)]

delEventActions :: Game -> [T.Text] -> Game
delEventActions gs [] = gs
delEventActions gs (ead:xs) =
  let eaData = T.splitOn "." ead
   in case eaData of 
        [act,dt] ->
          let pev 
               | elem act txsByName = 
                    fromMaybe PNon $ lookup act (txPevByName dt)
               | otherwise = PNon
              nevas = filter (\(EA pe _ _ _) -> pe/=pev) (gs^.evas) 
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
  let ob = gs^.hav
      obnm = maybe T.empty getObjName ob
   in obnm == tgt || (tgt=="any" && isJust ob)
conditions "counter" tgt gs =
  let scs = T.splitOn "," tgt
      scps = makeScPair scs
   in isMatchCount (gs^.cnts) scps
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
setMap gs mapNm = 
  let textSections = gs^.txs 
      obMapText = lookupFromSections textSections ("map" <> mapNm)
      nmnm = if obMapText==T.empty then T.empty else mapNm 
      (obMapPre,mapSize) = makeObjectMap obMapText
      objData = lookupFromSections textSections ("obj" <> mapNm)
      mimData = lookupFromSections textSections ("mim" <> mapNm)
      nmim = case mimData of "" -> 0::Int; mm -> (read . T.unpack ) mm
      obMap = if objData==T.empty then obMapPre 
                                  else setObjectData (T.lines objData) obMapPre
      pps = getPosByName "player" obMap 
      mpos = setMapStartPos pps mapWinSize mapSize
   in gs&mnm.~ nmnm &msz.~ mapSize &mim.~ nmim &mps.~ mpos &omp.~ obMap &ims.~ True
   
deleteMap :: Game -> Game
deleteMap gs = gs&omp.~ [] &tmp.~ [] &mnm.~ T.empty &msz.~ V2 0 0 &mps.~V2 0 0

inputText :: Game -> [T.Text] -> Game
inputText gs args = gs&imd.~ Inp &tip.~ args

choiceDialog :: Game -> [T.Text] -> Game
choiceDialog gs args = 
  choiceDialog' (gs&imd.~ Cho &cho.~ [] &txv<>~ "\n" &tct+~ 1)
                                          args ["↑","↓","←","→","□"] 

choiceDialog' :: Game -> [T.Text] -> [T.Text] -> Game
choiceDialog' gs [] _ = gs
choiceDialog' gs [_] _ = gs
choiceDialog' gs _ [] = gs
choiceDialog' gs (tx:title:xs) (hd:ys) = let tlen = T.length tx + 3
  in choiceDialog' (gs&cho<>~ [title] &txv<>~ (hd<>" "<>tx<>"\n") &tct+~ tlen) xs ys

moveDialog :: Game -> T.Text -> Game 
moveDialog gs title = 
  let newText = lookupFromSections (gs^.txs) title
   in if newText==T.empty then gs else 
    gs&imd.~ Txt &itx.~ True &tct.~ 0 &txw.~ newText &txv.~T.empty 
      &lnu.~T.empty &lnt.~ T.empty

showChara :: Game -> T.Text -> Game
showChara gs chnum = gs&chn.~ (read . T.unpack) chnum
