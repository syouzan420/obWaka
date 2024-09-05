module Object where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Define 

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

deleteObjByPos :: ObName -> Pos -> ObMap -> ObMap
deleteObjByPos _ _ [] = []
deleteObjByPos tnm pos (ob@(Ob _ nm _ _ _ _ ps):xs) =  
  if pos==ps && tnm==nm then deleteObjByPos tnm pos xs 
                        else ob:deleteObjByPos tnm pos xs

isObjOnPos :: Pos -> ObMap -> Bool
isObjOnPos _ [] = False
isObjOnPos pos ((Ob _ _ _ _ _ _ ps):xs) = pos==ps || isObjOnPos pos xs 

putObjOnPos :: Object -> Pos -> ObMap -> ObMap
putObjOnPos (Ob ch nm tp df oc dr _) pos om = Ob ch nm tp df oc dr pos:om 

getObjName :: Object -> ObName
getObjName (Ob _ nm _ _ _ _ _) = nm
--
