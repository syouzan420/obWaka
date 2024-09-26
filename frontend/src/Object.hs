module Object where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Define 

isInMap :: Pos -> MapSize -> Bool
isInMap (V2 px py) (V2 mw mh) = px>=0 && px<mw && py>=0 && py<mh

getPosByName :: ObName -> ObMap -> Pos
getPosByName _ [] = V2 (-1) (-1)
getPosByName tn ((Ob _ nm _ _ _ _ ps):xs) =
  if tn==nm then ps else getPosByName tn xs

getDirByName :: ObName -> ObMap -> Dir 
getDirByName _ [] = NoDir 
getDirByName tn ((Ob _ nm _ _ _ dr _):xs) =
  if tn==nm then dr else getDirByName tn xs

getObjByPos :: Pos -> ObMap -> Maybe Object
getObjByPos _ [] = Nothing 
getObjByPos pos (ob@(Ob _ _ _ _ _ _ ps):xs) = 
  if pos==ps then Just ob else getObjByPos pos xs 

getObjByName :: ObName -> ObMap -> Maybe Object
getObjByName _ [] = Nothing 
getObjByName name (ob@(Ob _ nm _ _ _ _ _):xs) = 
  if name==nm then Just ob else getObjByName name xs 

getNameByPos :: Pos -> ObMap -> ObName 
getNameByPos _ [] = T.empty 
getNameByPos pos ((Ob _ nm _ _ _ _ ps):xs) =
  if pos==ps then nm else getNameByPos pos xs

updatePosByName :: ObName -> Pos -> ObMap -> ObMap 
updatePosByName _ _ [] = []
updatePosByName tnm pos (ob@(Ob ch nm tp df oc dr _):xs) =
  if tnm==nm then Ob ch nm tp df oc dr pos :xs
             else ob:updatePosByName tnm pos xs

updateDirByName :: ObName -> Dir -> ObMap -> ObMap 
updateDirByName _ _ [] = []
updateDirByName tnm dir (ob@(Ob ch nm tp df oc _ ps):xs) =
  if tnm==nm then Ob ch nm tp df oc dir ps :xs
             else ob:updateDirByName tnm dir xs

updateDefByName :: ObName -> ObDef -> ObMap -> ObMap 
updateDefByName _ _ [] = []
updateDefByName tnm def (ob@(Ob ch nm tp _ oc dr ps):xs) =
  if tnm==nm then Ob ch nm tp def oc dr ps :xs
             else ob:updateDefByName tnm def xs

updateObjByName :: ObName -> Object -> ObMap -> ObMap
updateObjByName _ _ [] = []
updateObjByName tnm tob@(Ob _ _ tp df cn dr tps) (ob@(Ob ch nm _ _ _ _ ps):xs) = 
  if tnm==nm then Ob ch nm tp df cn dr (if tps==V2 0 0 then ps else tps) :xs  
             else ob:updateObjByName tnm tob xs 


deleteObjByPos :: Pos -> ObMap -> ObMap
deleteObjByPos _ [] = []
deleteObjByPos pos (ob@(Ob _ nm _ _ _ _ ps):xs) =  
  if pos==ps && nm/="player" then deleteObjByPos pos xs 
                        else ob:deleteObjByPos pos xs

deleteObjByName :: ObName -> ObMap -> ObMap
deleteObjByName _ [] = []
deleteObjByName oname (ob@(Ob _ nm _ _ _ _ _):xs) =
  if oname==nm then xs else ob:deleteObjByName oname xs 

isObjOnPos :: Pos -> ObMap -> Bool
isObjOnPos _ [] = False
isObjOnPos pos ((Ob _ _ _ _ _ _ ps):xs) = pos==ps || isObjOnPos pos xs 

putObjOnPos :: Object -> Pos -> ObMap -> ObMap
putObjOnPos (Ob ch nm tp df oc dr _) pos om = Ob ch nm tp df oc dr pos:om 

getObjName :: Object -> ObName
getObjName (Ob _ nm _ _ _ _ _) = nm

getObjType :: Object -> ObType
getObjType (Ob _ _ tp _ _ _ _) = tp

getObjCon :: Object -> ObCon
getObjCon (Ob _ _ _ _ oc _ _) = oc

getObjCh :: Object -> ObChar
getObjCh (Ob ch _ _ _ _ _ _) = ch

getObjDef :: Object -> ObDef
getObjDef (Ob _ _ _ df _ _ _) = df 

getObjDir :: Object -> Dir 
getObjDir (Ob _ _ _ _ _ dr _) = dr 

getObjPos :: Object -> Pos
getObjPos (Ob _ _ _ _ _ _ ps) = ps

setObjType :: ObType -> Object -> Object
setObjType tp (Ob ch nm _ df oc dr ps) =  Ob ch nm tp df oc dr ps

setObjPos :: Pos -> Object -> Object
setObjPos ps (Ob ch nm tp df oc dr _) =  Ob ch nm tp df oc dr ps

changeObjCh :: ObChar -> Object -> Object
changeObjCh ch (Ob _ nm tp df oc dr ps) = Ob ch nm tp df oc dr ps 

blankObj :: Object
blankObj = Ob ' ' T.empty TBlock T.empty CBlock NoDir (V2 0 0)

putablePos :: Pos -> MapSize -> ObMap -> Pos
putablePos pos@(V2 px py) msz omp = 
  let imp = isInMap pos msz
      iob = imp && isObjOnPos pos omp  
      nps 
        | iob = V2 (px+1) py
        | imp = pos
        | otherwise = V2 (px-2) py
   in if iob || not imp then putablePos nps msz omp else nps
