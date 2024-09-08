module Converter where

--import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (find)
import Define

type Width = Int
type Height = Int
type Scroll = Int

getText :: T.Text -> [TextSection]
getText = getSections . T.lines

type PlyPos = Pos
type MapWinSize = Size 
type MapPos = Pos
type IsDiagonal = Bool
type FlatMap = [T.Text]

inpToDir :: WkEvent -> Dir
inpToDir p = case p of
  WRight -> East; WUp -> North; WLeft -> West; WDown -> South; _ -> NoDir

dirToDelta :: Dir -> Pos
dirToDelta dr = case dr of
  East -> V2 1 0; North -> V2 0 (-1); West -> V2 (-1) 0; South -> V2 0 1; NoDir -> V2 0 0
      
setMapStartPos :: PlyPos -> MapWinSize -> MapSize -> MapPos
setMapStartPos (V2 x y) (V2 w h) (V2 mw mh) =
  let (V2 cx cy) = V2 (div w 2) (div h 2)  -- map window center
      fp = if x > cx then x-cx else 0
      fq = if y > cy then y-cy else 0
      sp = if mw < fp+w then mw-w else fp
      sq = if mh < fq+h then mh-h else fq
      p = max sp 0 
      q = max sq 0
   in V2 p q

putMapInFrame :: MapWinSize -> MapPos -> T.Text -> T.Text
putMapInFrame (V2 mw mh) (V2 mx my) mpText =
  let lns = map (T.take mw . T.drop mx) $ take mh $ drop my $ T.lines mpText
   in T.unlines lns

makeObjectMap :: T.Text -> (ObMap,MapSize) 
makeObjectMap tx =  
  let lns = T.lines tx 
      w = if not (null lns) then T.length (head lns) else 0
      txnc = T.replace "\n" "" tx
      searchResult = searchObject 0 (T.unpack txnc)
   in (map (\(i,ch)->
       let p = mod i w ; q = div i w 
           oname = if ch=='@' then "player" else T.empty
        in Ob ch oname TLive T.empty CBlock North (V2 p q) ) searchResult,V2 w (length lns))

setObjectData :: [T.Text] -> ObMap -> ObMap
setObjectData _ [] = [] 
setObjectData obdts (ob@(Ob ch _ _ _ _ _ ps):obs) =
  let tgtDt = fromMaybe T.empty $
                 find (\obdt -> fmap fst (T.uncons obdt) == Just ch) obdts
      dtList = T.splitOn "," tgtDt
      newObj = case dtList of
        [_,tnm,ttp,tdf,tcn,tdr] ->
               Ob ch tnm (txToType ttp) tdf (txToCon tcn) (txToDir tdr) ps 
        _ -> ob
   in newObj:setObjectData obdts obs

txToType :: T.Text -> ObType
txToType "" = TBlock
txToType txt = let txts = T.words txt
                in case txts of
                    ("func":tps) -> TFunc (map findType tps)
                    [tp] -> findType tp
                    _ -> TBlock

findType :: T.Text -> ObType
findType txt = fromMaybe TBlock $ lookup txt txType

txToCon :: T.Text -> ObCon
txToCon txt = fromMaybe CBlock $ lookup txt txCon 

txToDir :: T.Text -> Dir
txToDir txt = fromMaybe NoDir $ lookup txt txDir

txType :: [(T.Text,ObType)]
txType = [("kazu",TKazu),("mozi",TMozi),("live",TLive),("food",TFood),("tool",TTool),("block",TBlock)]

txCon :: [(T.Text,ObCon)]
txCon = [("block",CBlock),("move",CMove),("get",CGet),("on",COn),("enter",CEnter)]

txDir :: [(T.Text,Dir)]
txDir = [("east",East),("north",North),("west",West),("south",South),("nodir",NoDir)]

searchObject :: Int -> String -> [(Int,Char)]
searchObject _ [] = []
searchObject i (x:xs) = if x=='*' then searchObject (i+1) xs
                                  else (i,x):searchObject (i+1) xs

showMap :: MapSize -> ObMap -> ObMap -> T.Text
showMap ms om tm = T.unlines $ showObMap (om <> tm) (makeFlatMap ms) 

makeFlatMap :: MapSize -> FlatMap
makeFlatMap (V2 w h) = replicate h $ T.pack (replicate w '.')

showObMap :: ObMap -> FlatMap -> [T.Text]
showObMap [] mtx = mtx 
showObMap ((Ob ch _ _ _ _ _ (V2 x y)):xs) mtx = showObMap xs (insertChar ch x y mtx)

insertChar :: Char -> Int -> Int -> [T.Text] -> [T.Text]
insertChar ch x y txs =  
  let ln = txs!!y
      (hd,tl) = T.splitAt x ln
      nln = hd <> T.singleton ch <> T.tail tl
      (bln,aln) = splitAt y txs
    in bln ++ [nln] ++ tail aln 

getSections :: [T.Text] -> [TextSection]
getSections = getSections' Nothing []

getSections' :: Maybe T.Text -> [T.Text] -> [T.Text] -> [TextSection]
getSections' _ [] [] = []
getSections' Nothing [] (x:xs) 
    | x==T.empty = getSections' Nothing [] xs
    | T.last x == ':' = getSections' (Just (T.init x)) [] xs 
    | otherwise = getSections' Nothing [] xs
getSections' Nothing _ _ = []
getSections' (Just tx) acc [] = [TS tx (T.unlines acc)]
getSections' (Just tx) acc (x:xs) 
    | x==T.empty = getSections' (Just tx) (acc++[" "]) xs
    | T.last x == ':' = TS tx ((T.unlines . init) acc) : getSections' (Just (T.init x)) [] xs 
    | otherwise = getSections' (Just tx) (acc++[x]) xs  
                      
type IsStop = Bool
type IsTyping = Bool
type IsCode = Bool

getInfoFromChar :: T.Text -> Int -> (IsStop,IsTyping,IsCode,Char,T.Text,Int)
getInfoFromChar wtx i = 
  let ch = T.index wtx i
      isCode = ch==';'
      isStop = ch=='。'
      isTyping = not (isStop || ch=='、' || isCode)
      codeText = if isCode then T.tail (T.takeWhile (/='\n') (T.drop i wtx)) else T.empty
      scanLength = if isCode then T.length codeText + 1 else 1
   in (isStop,isTyping,isCode,ch,codeText,scanLength)

lookupFromSections :: [TextSection] -> T.Text -> T.Text
lookupFromSections textSections tx = 
  let tsKeyValues = map (\(TS ti t) -> (ti,t)) textSections
   in fromMaybe T.empty (lookup tx tsKeyValues)  

