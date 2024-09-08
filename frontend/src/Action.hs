module Action (movePlayer,hitAction,putAction,triggerFunc) where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Object (getPosByName,getDirByName,getObjByPos,blankObj,changeObjCh
              ,getObjType,getObjName,getObjCon,getObjCh,getObjDef
              ,updatePosByName,deleteObjByPos,isObjOnPos,putObjOnPos)
import Converter (inpToDir,dirToDelta,lookupFromSections,setObjectData)
import Data.Maybe (isNothing,fromMaybe)
import Data.Functor ((<&>))
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
      obc = obj <&> getObjCon
      isBlock = case obc of Just oc-> oc==CBlock; Nothing -> False
      isPush = case obc of Just oc -> oc==CMove; Nothing -> False
      isGet = case obc of Just oc -> oc==CGet; Nothing -> False
      oname = maybe T.empty getObjName obj
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
      nomp3 = if isGet && isNothing hv then deleteObjByPos nomp2 nops else nomp2  
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

putAction :: Object -> Dir -> MapSize -> ObMap -> (ObMap,Maybe Object)
putAction tob pDir (V2 mw mh) om =
  let pPos = getPosByName "player" om
      tps@(V2 tx ty) = pPos + dirToDelta pDir   
      canPut = tx>=0 && tx<mw && ty>=0 && ty<mh && not (isObjOnPos tps om)
   in if canPut then (putObjOnPos tob tps om,Nothing) else (om,Just tob)    

triggerFunc :: Game -> Dir -> ObMap -> ObMap
triggerFunc gs pDir om =
  let pPos = getPosByName "player" om
      tps = pPos + dirToDelta pDir   
      tob = getObjByPos tps om
      otp = maybe TBlock getObjType tob
      och = maybe ' ' getObjCh tob
      odf = maybe T.empty getObjDef tob
      isFunc = case otp of TFunc _ -> True; _ -> False
      argTps = case otp of TFunc args -> args; _ -> []
   in if isFunc then exeFunc gs pDir och tps odf om $ getArgs pDir tps om argTps  
                else om

getArgs :: Dir -> Pos -> ObMap -> [ObType] -> [ObChar]  
getArgs _ _ _ [] = [] 
getArgs pDir tps om (atp:xs) =   
  let agPos = tps + dirToDelta pDir 
      agObj = getObjByPos agPos om
      agTp = maybe TBlock getObjType agObj
      agCh = maybe ' ' getObjCh agObj
   in if atp==agTp then agCh:getArgs pDir agPos om xs else [] 

exeFunc :: Game -> Dir -> ObChar -> Pos -> ObDef -> ObMap -> [ObChar] -> ObMap
exeFunc gs pDir och tps df om chs = 
  let defList = makeDef gs och df
      objList = makeObj gs df
      resExp = patternMatch chs defList
      isRes = resExp /= T.empty
   in if isRes then let agPosList = zipWith (\ i _y -> 
                          tps + V2 i i*dirToDelta pDir) [1..] chs  
                        nomp = foldl deleteObjByPos om agPosList 
                        resCh = getResult resExp
                        resObj = fromMaybe blankObj $ lookup resCh objList
                        resPos = tps + dirToDelta pDir
                     in putObjOnPos resObj resPos nomp 
               else om 

makeDef :: Game -> ObChar -> ObDef -> [[T.Text]]
makeDef gs ch df =
  let obMapText = lookupFromSections gs df
      textLines = T.lines (T.replace "*" "" obMapText)
   in makeDefLines ch textLines 

makeDefLines :: ObChar -> [T.Text] -> [[T.Text]]
makeDefLines _ [] = []
makeDefLines ch (ln:xs) = 
  let str = T.unpack ln
   in if elem '=' str && elem ch str then defLine ch ln:makeDefLines ch xs  
                                     else makeDefLines ch xs

defLine :: ObChar -> T.Text -> [T.Text]
defLine ch ln = 
  let leftEqual = maybe T.empty (fst . T.breakOn "=" <$> snd) $ T.uncons (snd $ T.breakOn (T.singleton ch) ln)
      rightEqual = maybe T.empty snd (T.uncons $ snd $ T.breakOn "=" ln)
      leftChs = map T.singleton $ T.unpack leftEqual
   in leftChs <> [rightEqual] 

makeObj :: Game -> ObDef -> [(ObChar,Object)]
makeObj gs df =
  let mpn = if T.take 3 df == "map" then T.drop 3 df else T.empty 
      objTxts = T.lines $ lookupFromSections gs ("obj"<>mpn)
      preObjs = map (\txt -> changeObjCh (T.head txt) blankObj) objTxts
      objList = setObjectData objTxts preObjs 
      chList = map getObjCh objList 
   in zip chList objList

patternMatch :: [ObChar] -> [[T.Text]] -> T.Text
patternMatch _ [] = T.empty
patternMatch chs (dfs:xs) =
  let isMatch = foldl (\acc (ch,df) -> [ch]==T.unpack df && acc) True (zip chs dfs) 
   in if isMatch && (length chs + 1)==length dfs then last dfs
                                                 else patternMatch chs xs 

getResult :: T.Text -> ObChar
getResult res
  | T.length res == 1 = T.head res 
  | otherwise = ' ' -- not complete
