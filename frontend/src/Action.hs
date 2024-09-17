module Action (movePlayer,hitAction,putAction,attackAction,moveObject) where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Object (getPosByName,getDirByName,getObjByPos,blankObj,changeObjCh
              ,getObjType,getObjName,getObjCon,getObjCh,getObjDef
              ,updatePosByName,deleteObjByPos,isObjOnPos,putObjOnPos
              ,getObjPos,setObjPos,setObjType)
import Converter (inpToDir,dirToDelta,lookupFromSections,setObjectData,isInMap)
import Data.Maybe (isNothing,fromMaybe)
import Data.Functor ((<&>))
import Data.Bifunctor (first)
import System.Random (StdGen,uniformR)
import Define 

import Debug.Trace (trace)

type MapPos = Pos
type MapWinPos = Pos

movePlayer :: WkEvent -> Maybe Object -> MapSize -> MapWinPos -> MapPos  
                             -> ObMap -> ([PEvent],ObMap,MapPos,Maybe Object) 
movePlayer ev hv msz@(V2 mw mh) (V2 w h) (V2 mx my) omp =  
  let pps = getPosByName "player" omp
      dps = dirToDelta $ inpToDir ev 
      tps = pps + dps
      imp = isInMap tps msz 
      obj = getObjByPos tps omp  -- map opr on target
      oname = maybe T.empty getObjName obj
      obc = obj <&> getObjCon
      isBlock = case obc of Just oc-> oc==CBlock; Nothing -> False
      isPush = case obc of Just oc -> oc==CMove; Nothing -> False
      isGet = case obc of Just oc -> oc==CGet; Nothing -> False
      isEnter = case obc of Just oc -> oc==CEnter; Nothing -> False
      isOn = case obc of Just oc -> oc==COn; Nothing -> False
      isLeave = isOn && oname=="leave"
      tops = if isPush then tps + dps else tps
      isObInMap = isInMap tops msz 
      isAnotherObj = isObjOnPos tops omp
      isPushTo = isPush && isAnotherObj
      aoName = maybe T.empty getObjName (getObjByPos tops omp)
      nops = if isObInMap && not isAnotherObj then tops else tps
      npps@(V2 nx ny) = if imp && not isBlock && not isPushTo then tps else pps
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
            <>[PPushTo oname aoName | isPushTo]<>[POn oname | isOn] 
            <>[PGet oname | isGet]<>[PLeave | isLeave]
            <>case obj of Just ob -> [PEnter pps ob | isEnter]; Nothing -> []
      nphv = if isGet && isNothing hv then obj else hv
   in (npevs, nomp3, V2 nmx nmy, nphv)

data MoveType = MV | AT | NN deriving stock Eq

moveObject :: StdGen -> MapSize -> ObMap -> ObMap -> (ObMap,StdGen)
moveObject g _ _ [] = ([],g)
moveObject g msz omp (obj:xs) =
  let obType = getObjType obj
      (mvType,mct,ct) = case obType of
        TLive (LMove maxCount count) -> (MV,maxCount,count) 
        TLive (LAttack maxCount count) -> (AT,maxCount,count)
        _ -> (NN,0,0)
   in if mvType==NN then let mvo = moveObject g msz omp xs 
                          in first (obj :) mvo else
          let isExec = mct==ct
              newCount = if isExec then 0 else ct+1
              pos = getObjPos obj
              (npos,ng) = if isExec then case mvType of
                       MV -> confirmPos pos msz omp $ nextMVPos g pos
                       AT -> let pps = getPosByName "player" omp
                              in confirmPos pos msz omp $ nextATPos g pps pos
                       _  -> (pos,g)
                                    else (pos,g)
              ntp = case mvType of
                      MV -> TLive (LMove mct newCount)
                      AT -> TLive (LAttack mct newCount)
                      _ -> obType
              nobj = setObjPos npos $ setObjType ntp obj
              mvo = moveObject ng msz omp xs
           in first (nobj :) mvo

confirmPos :: Pos -> MapSize -> ObMap -> (Pos,StdGen) -> (Pos,StdGen)
confirmPos pos msz omp (tps,g) = 
  let imp = isInMap tps msz
      isObj = isObjOnPos tps omp
   in if imp && not isObj then (tps,g) else (pos,g) 

nextMVPos :: StdGen -> Pos -> (Pos,StdGen)
nextMVPos g pos = let (dirNum,ng) = uniformR (0,4) g
                      dir = toEnum dirNum :: Dir
                   in (pos + dirToDelta dir, ng)

nextATPos :: StdGen -> Pos -> Pos -> (Pos,StdGen)
nextATPos g pps pos = let dff@(V2 dx dy) = pps - pos  
                          isRange = dx*dx + dy*dy < 25
                          (dirNum,ng) = if isRange then approach g dff
                                                   else uniformR (0,4) g
                          dir = toEnum dirNum :: Dir
                       in (pos + dirToDelta dir, ng)

approach :: StdGen -> Pos -> (Int,StdGen) 
approach g (V2 dx dy) = let ixp = dx >= 0
                            iyp = dy >= 0
                            abx = abs dx
                            aby = abs dy
                            (intForBool,ng) = uniformR (0::Int,1) g
                            randBool = intForBool==1
                            ixly = abx > aby || (abx==aby && randBool)
                            dirNum 
                              | ixly = if ixp then 1 else 3
                              | iyp = 4
                              | otherwise = 2 
                         in (dirNum,ng)

hitAction :: ObName -> MapSize -> ObMap -> ObMap -> ObMap 
hitAction onm msz om tm = 
  let pPos = getPosByName onm om 
      pDir = getDirByName onm om
      eps = pPos + dirToDelta pDir 
      isShow = isInMap eps msz
   in if isShow then Ob '/' "hit" (TLive LStand) T.empty COn pDir eps:tm else tm 

putAction :: Object -> Dir -> MapSize -> ObMap -> ([PEvent],ObMap,Maybe Object)
putAction tob pDir msz om =
  let pPos = getPosByName "player" om
      oName = getObjName tob
      tps = pPos + dirToDelta pDir   
      canPut = isInMap tps msz && not (isObjOnPos tps om)
   in if canPut then ([PPut oName tps],putObjOnPos tob tps om,Nothing)
                else ([],om,Just tob)    

attackAction :: [TextSection] -> Dir -> MapName 
                      -> ObMap -> ([PEvent],ObMap,Maybe Object)
attackAction txSec pDir mnm om =
  let pPos = getPosByName "player" om
      tps = pPos + dirToDelta pDir   
      tob = getObjByPos tps om
      otp = maybe TBlock getObjType tob
      och = maybe ' ' getObjCh tob
      oName = maybe T.empty getObjName tob
      odf = maybe T.empty getObjDef tob
      isFunc = case otp of TFunc _ -> True; _ -> False
      argTps = case otp of TFunc args -> args; _ -> []
      argLng = length argTps
   in if isFunc then let (resCh,nom) = 
                            exeFunc txSec pDir mnm och tps odf om argLng $
                                                    getArgs pDir tps om argTps  
                      in if resCh==' ' then ([],nom,Nothing)
                                       else ([PFunc oName resCh],nom,Nothing)
                else (case oName of "" -> []; onm -> [PAttack onm],om,Nothing)

getArgs :: Dir -> Pos -> ObMap -> [ObType] -> [ObChar]  
getArgs _ _ _ [] = [] 
getArgs pDir tps om (atp:xs) =   
  let agPos = tps + dirToDelta pDir 
      agObj = getObjByPos agPos om
      agTp = maybe TBlock getObjType agObj
      agCh = maybe ' ' getObjCh agObj
   in if atp==agTp then agCh:getArgs pDir agPos om xs else [] 

exeFunc :: [TextSection] -> Dir -> MapName -> ObChar -> Pos 
                        -> ObDef -> ObMap -> Int -> [ObChar] -> (ObChar,ObMap)
exeFunc txSec pDir mnm och tps df om argLng chs = 
  let defList = makeDef txSec och df
      objList = makeObj txSec mnm 
      resExp = patternMatch chs defList
      isRes = resExp /= T.empty && argLng == length chs + 1 
   in if isRes then let agPosList = zipWith (\ i _y -> 
                          tps + V2 i i*dirToDelta pDir) [1..] chs  
                        nomp = foldl deleteObjByPos om agPosList 
                        resCh = getResult resExp
                        resObj = fromMaybe blankObj $ lookup resCh objList
                        resPos = tps + dirToDelta pDir
                     in trace (show chs) $ (resCh,putObjOnPos resObj resPos nomp) 
               else trace (show chs) (' ',om) 

makeDef :: [TextSection] -> ObChar -> ObDef -> [[T.Text]]
makeDef txSec ch df =
  let obMapText = lookupFromSections txSec df
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

makeObj :: [TextSection] -> MapName -> [(ObChar,Object)]
makeObj txSec mnm =
  let objTxts = T.lines $ lookupFromSections txSec ("obj"<>mnm)
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
