module Object where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Define 

getPosByName :: ObName -> ObMap -> Pos
getPosByName _ [] = V2 (-1) (-1)
getPosByName tn ((Ob _ nm _ _ ps):xs) =
  if tn==nm then ps else getPosByName tn xs

--getLayerByName :: ObName -> ObMap -> ObLayer 
--getLayerByName _ [] = 0 
--getLayerByName tn ((Ob _ nm ly _ _):xs) =
--  if tn==nm then ly else getLayerByName tn xs

--getObjByPos :: Pos -> ObLayer -> ObMap -> Object
--getObjByPos _ _ [] = Ob ' ' T.empty 0 (V2 0 0) No
--getObjByPos pos lay (ob@(Ob _ _ ly ps _):xs) = 
--  if pos==ps && lay==ly then ob else getObjByPos pos lay xs 

getNameByPos :: Pos -> ObMap -> ObName 
getNameByPos _ [] = T.empty 
getNameByPos pos ((Ob _ nm _ _ ps):xs) =
  if pos==ps then nm else getNameByPos pos xs
--
--getOprByPos :: Pos -> ObLayer -> ObMap -> ObProperty
--getOprByPos _ _ [] = No
--getOprByPos pos lay ((Ob _ _ ly ps op):xs) =
--  if pos==ps && lay==ly then op else getOprByPos pos lay xs  
--
--getLayerByPos :: Pos -> ObMap -> ObLayer 
--getLayerByPos _ [] = 0 
--getLayerByPos pos ((Ob _ _ ly ps _):xs) =
--  if pos==ps then ly else getLayerByPos pos xs  
--
updatePosByName :: ObName -> Pos -> ObMap -> ObMap 
updatePosByName _ _ [] = []
updatePosByName tnm pos (ob@(Ob ch nm tp df _):xs) =
  if tnm==nm then Ob ch nm tp df pos :xs
             else ob:updatePosByName tnm pos xs
--
