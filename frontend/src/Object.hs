module Object where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Define 

getPosByName :: ObName -> ObMap -> Pos
getPosByName _ [] = V2 (-1) (-1)
getPosByName tn ((Ob _ nm _ _ ps):xs) =
  if tn==nm then ps else getPosByName tn xs

getObjByPos :: Pos -> ObMap -> Maybe Object
getObjByPos _ [] = Nothing 
getObjByPos pos (ob@(Ob _ _ _ _ ps):xs) = 
  if pos==ps then Just ob else getObjByPos pos xs 

getNameByPos :: Pos -> ObMap -> ObName 
getNameByPos _ [] = T.empty 
getNameByPos pos ((Ob _ nm _ _ ps):xs) =
  if pos==ps then nm else getNameByPos pos xs

updatePosByName :: ObName -> Pos -> ObMap -> ObMap 
updatePosByName _ _ [] = []
updatePosByName tnm pos (ob@(Ob ch nm tp df _):xs) =
  if tnm==nm then Ob ch nm tp df pos :xs
             else ob:updatePosByName tnm pos xs
--
