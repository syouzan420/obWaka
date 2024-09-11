module Code(exeCode,setMap,moveDialog) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Linear.V2 (V2(..))
import Converter (makeObjectMap,setObjectData,setMapStartPos
                 ,lookupFromSections,makeObjectByName)
import Object (getPosByName,getObjName,putObjOnPos)
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
    "stpl" -> setPlayer gs
    _ -> gs 
                    else case en of
    "a" -> setEventAction gs (head ags) (T.intercalate "_" (tail ags)) 
    "if" -> exeCondition gs ags
    "mvdi" -> moveDialog gs (head ags)
    "stmp" -> setMap gs (head ags)
    "ch" -> changeChara gs (head ags)
    "cho" -> choiceDialog gs ags
    "p" -> putObject gs ags
    _ -> gs 

putObject :: Game -> [T.Text] -> Game
putObject gs [] = gs
putObject gs (objNamePos:xs) =
  let npData = T.splitOn "," objNamePos
   in case npData of
        [oname,opx,opy] -> 
          let textSections = _txs gs
              mnm = _mnm gs
              omp = _omp gs
              obDatas = T.lines $ lookupFromSections textSections ("obj"<>mnm)
              obj = makeObjectByName oname obDatas
              nomp = case obj of
                      Just ob -> putObjOnPos ob (V2 ((read . T.unpack) opx)
                                                     ((read . T.unpack) opy)) omp 
                      Nothing -> omp
           in putObject gs{_omp=nomp} xs
        _ -> putObject gs xs


setPlayer :: Game -> Game 
setPlayer gs = gs{_itx=False,_imd = Ply} 

setEventAction :: Game -> T.Text -> Code -> Game 
setEventAction gs ead pcd = 
  let eaData = T.splitOn "." ead
  in case eaData of
      [act,dt,num] -> 
        let ea = case act of
              "block" -> EA (PBlock dt) pcd ((read . T.unpack) num) 0
              _ -> EA PNon pcd 0 0
         in gs{_evas = ea:_evas gs} 
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
   in obnm == tgt
conditions _ _ _ = False


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
   in gs{_mnm = nmnm, _msz = mapSize, _mps = mpos, _omp = obMap}
   

choiceDialog :: Game -> [T.Text] -> Game
choiceDialog gs args = 
  choiceDialog' gs{_imd=Cho,_cho=[],_txv=_txv gs<>"\n",_tct=_tct gs + 1}
                                          args ["●","↑","←","□","↓","→"] 

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
    gs{_imd=Txt, _itx=True, _tct=0, _txw=newText, _txv=T.empty}

changeChara :: Game -> T.Text -> Game
changeChara gs chn = gs{_chn = (read . T.unpack) chn}
