module Code(exeCode) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Converter (makeObjectMap,setObjectData,setMapStartPos)
import Object (getPosByName)
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
    "a" -> if length ags==2 then setEventAction gs (head ags) (last ags) else gs 
    "mvdi" -> moveDialog gs (head ags)
    "stmp" -> setMap gs (head ags)
    _ -> gs 

lookupFromSections :: Game -> T.Text -> T.Text
lookupFromSections gs tx = 
  let textSections = _txs gs
      tsKeyValues = map (\(TS ti t) -> (ti,t)) textSections
   in fromMaybe T.empty (lookup tx tsKeyValues)  

setPlayer :: Game -> Game 
setPlayer gs = gs{_itx=False,_imd = Ply} 

setEventAction :: Game -> T.Text -> Code -> Game 
setEventAction gs ead pcd = 
  let eaData = T.splitOn "." ead
      cd = T.replace "." "_" pcd
  in case eaData of
      [act,dt,num] -> 
        let ea = case act of
              "block" -> EA (PBlock dt) cd ((read . T.unpack) num) 0
              _ -> EA PNon cd 0 0
                       in gs{_evas = _evas gs<>[ea]} 
      _ -> gs

setMap :: Game -> T.Text -> Game 
setMap gs i = 
  let obMapText = lookupFromSections gs ("map" <> i)
      (obMapPre,mapSize) = makeObjectMap obMapText
  --    obMap = map (\(Ob ch nme tp df oc dr ps) -> let nm = lookupFromSections gs ("name" <> T.singleton ch) in Ob ch (if nm==T.empty then nme else T.init nm) tp df oc dr ps)  obMapPre
      objData = lookupFromSections gs ("obj" <> i)
      obMap = setObjectData (T.lines objData) obMapPre
      pps = getPosByName "player" obMap 
      mpos = setMapStartPos pps mapWinSize mapSize
   in gs{_msz = mapSize, _mps = mpos, _omp = obMap}

moveDialog :: Game -> T.Text -> Game 
moveDialog gs title = 
  let newText = lookupFromSections gs title
   in if newText==T.empty then gs else 
    gs{_imd=Txt, _itx=True, _tct=0, _txw=newText, _txv=_txv gs <> "\n \n"}

