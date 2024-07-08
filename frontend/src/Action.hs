module Action (movePlayer,hitAction) where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Object (getPosByName,getDirByName,getObjByPos,updatePosByName)
import Converter (inpToDir,dirToDelta)
import Define 

type MapPos = Pos
type MapWinPos = Pos

movePlayer :: WkEvent -> MapSize -> MapWinPos -> MapPos  
                                   -> ObMap -> (ObMap,MapPos,[PEvent]) 
movePlayer ev (V2 mw mh) (V2 w h) (V2 mx my) omp =  
  let pps = getPosByName "player" omp
      dps = dirToDelta $ inpToDir ev 
      tps@(V2 tx ty) = pps + dps
      isInMap = tx>=0 && tx<mw && ty>=0 && ty<mh 
      obj = getObjByPos tps omp  -- map opr on target
      isBlock = case obj of
                  Just (Ob _ _ _ _ oc _ _) -> oc==CBlock
                  Nothing -> False
      oname = case obj of
                  Just (Ob _ nm _ _ _ _ _) -> nm
                  Nothing -> T.empty
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
      npevs = [PMove npps]<>[PBlock oname | isBlock]
   in (nomp, V2 nmx nmy, npevs)

hitAction :: ObName -> MapSize -> ObMap -> ObMap -> ObMap 
hitAction onm (V2 mw mh) om tm = 
  let pPos = getPosByName onm om 
      pDir = getDirByName onm om
      eps@(V2 ex ey) = pPos + dirToDelta pDir 
      isShow = ex>=0 && ex<mw && ey>=0 && ey<mh
   in if isShow then Ob '/' "hit" TLive T.empty COn pDir eps:tm else tm 
