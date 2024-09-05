module Action (movePlayer,hitAction) where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Object (getPosByName,getDirByName,getObjByPos
              ,updatePosByName,deleteObjByPos)
import Converter (inpToDir,dirToDelta)
import Data.Maybe (isNothing)
import Define 

type MapPos = Pos
type MapWinPos = Pos

movePlayer :: WkEvent -> Maybe Object -> MapSize -> MapWinPos -> MapPos  
                             -> ObMap -> (ObMap,MapPos,[PEvent],Maybe Object) 
movePlayer ev hv (V2 mw mh) (V2 w h) (V2 mx my) omp =  
  let pps = getPosByName "player" omp
      dps = dirToDelta $ inpToDir ev 
      tps@(V2 tx ty) = pps + dps
      isInMap = tx>=0 && tx<mw && ty>=0 && ty<mh 
      obj = getObjByPos tps omp  -- map opr on target
      obc = case obj of
              Just (Ob _ _ _ _ oc _ _) -> Just oc
              Nothing -> Nothing
      isBlock = case obc of Just oc-> oc==CBlock; Nothing -> False
      isPush = case obc of Just oc -> oc==CMove; Nothing -> False
      isGet = case obc of Just oc -> oc==CGet; Nothing -> False
      oname = case obj of
                  Just (Ob _ nm _ _ _ _ _) -> nm
                  Nothing -> T.empty
      tops@(V2 tox toy) = if isPush then tps + dps else tps
      isObInMap = tox>=0 && tox<mw && toy>=0 && toy<mh 
      nops = if isObInMap then tops else tps
      npps@(V2 nx ny) = if isInMap && not isBlock then tps else pps
      nmx 
        | nx-mx < 1 && mx > 0 = mx - 1 
        | nx-mx > w-2 && mx < mw-w = mx + 1
        | otherwise = mx
      nmy 
        | ny-my < 1 && my > 0 = my - 1
        | ny-my > h-2 && my < mh-h = my + 1
        | otherwise = my
      nomp = if npps/=pps then updatePosByName "player" npps omp else omp
      nomp2 = if isPush then updatePosByName oname nops nomp else nomp 
      nomp3 = if isGet && isNothing hv then deleteObjByPos oname nops nomp2 else nomp2  
      npevs = [PMove npps]<>[PBlock oname | isBlock]<>[PPush oname | isPush]
      nphv = if isGet && isNothing hv then obj else hv
   in (nomp3, V2 nmx nmy, npevs, nphv)

hitAction :: ObName -> MapSize -> ObMap -> ObMap -> ObMap 
hitAction onm (V2 mw mh) om tm = 
  let pPos = getPosByName onm om 
      pDir = getDirByName onm om
      eps@(V2 ex ey) = pPos + dirToDelta pDir 
      isShow = ex>=0 && ex<mw && ey>=0 && ey<mh
   in if isShow then Ob '/' "hit" TLive T.empty COn pDir eps:tm else tm 
