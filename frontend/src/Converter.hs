{-# LANGUAGE OverloadedStrings #-}
module Converter where

--import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))
import qualified Data.Text as T
import Define

type Width = Int
type Height = Int
type Scroll = Int


takeWidth :: Scroll -> Width -> T.Text -> T.Text
takeWidth s w tx = let lngT = T.length tx 
                    in if w > lngT then T.replicate (w-lngT) "　" <> tx 
                                   else T.take w (T.drop s tx)

takeHeight :: Height -> T.Text -> [T.Text]
takeHeight h tx
  | tx==T.empty = []
  | otherwise = let t = T.take h tx 
                    lngT = T.length t
                 in if lngT < h then [t <> T.replicate (h-lngT) "　"]   
                                else t : takeHeight h (T.drop h tx)

getText :: T.Text -> [TextSection]
getText = getSections . T.lines


type PlyPos = Pos
type MapSize = Pos
type MapWinSize = Pos
type MapPos = Pos
type IsDiagonal = Bool

--inpToDir :: IsDiagonal -> Input -> Direction
--inpToDir True p = case p of Ri -> EN; Up -> NW; Lf -> WS; Dn -> SE; _ -> NoDir
--inpToDir False p = case p of
--  Ri -> East; Up -> North; Lf -> West; Dn -> South; _ -> NoDir

--dirToDelta :: Direction -> Pos
--dirToDelta dr = case dr of
--  East -> V2 1 0; EN -> V2 1 (-1); North -> V2 0 (-1); NW -> V2 (-1) (-1)
--  West -> V2 (-1) 0; WS -> V2 (-1) 1; South -> V2 0 1; SE -> V2 1 1; NoDir -> V2 0 0
      
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
  let mw' = fromIntegral mw; mh' = fromIntegral mh
      mx' = fromIntegral mx; my' = fromIntegral my
      lns = map (T.take mw' . T.drop mx') $ take mh' $ drop my' $ T.lines mpText
   in T.unlines lns

--convertMap :: T.Text -> MapWhole
--convertMap tx =
--  let lns = T.lines tx
--   in map (map (\ch -> toEnum (fromMaybe 0 (T.findIndex (==ch) mapCh)) :: MapCell) . T.unpack) lns

type PropNLayerNums = [(Int,Int)]

makeObjectMap :: T.Text -> (ObMap,MapSize) 
makeObjectMap tx =  
  let lns = T.lines tx 
      w = if not (null lns) then T.length (head lns) else 0
      txnc = T.replace "\n" "" tx
      searchResult = searchObject 0 (T.unpack txnc)
   in (map (\(i,ch)->
       let p = mod i w ; q = div i w 
        in Ob ch T.empty Cha T.empty (V2 p q) ) searchResult,V2 w (length lns))


searchObject :: Int -> String -> [(Int,Char)]
searchObject _ [] = []
searchObject i (x:xs) = if x=='*' then searchObject (i+1) xs
                                  else (i,x):searchObject (i+1) xs


--showMap :: MapObject -> MapObject -> MapWhole -> T.Text
--showMap mo mt mw = T.unlines $ showMapObject (sortByLayers mo <> mt) $ showMapWhole mw 

--showMapObject :: MapObject -> [T.Text] -> [T.Text]
--showMapObject [] mtx = mtx 
--showMapObject ((Ob ch _ _ (V2 x y) _):xs) mtx =
--  let x' = fromIntegral x; y' = fromIntegral y
--   in showMapObject xs (insertChar ch x' y' mtx)
--
insertChar :: Char -> Int -> Int -> [T.Text] -> [T.Text]
insertChar ch x y txs =  
  let ln = txs!!y
      (hd,tl) = T.splitAt x ln
      nln = hd <> T.singleton ch <> T.tail tl
      (bln,aln) = splitAt y txs
    in bln ++ [nln] ++ tail aln 

--sortByLayers :: MapObject -> MapObject
--sortByLayers [] = []
--sortByLayers (ob@(Ob _ _ x _ _):xs) = sortSmaller ++ [ob] ++ sortLarger
--  where sortSmaller = [o | o@(Ob _ _ l _ _) <- xs, l <= x] 
--        sortLarger = [o | o@(Ob _ _ l _ _) <- xs, l > x]

--showMapWhole ::  MapWhole -> [T.Text]
--showMapWhole = map (T.pack. map (\mc -> if mc==Wall || mc==Block || mc==Water then '#' else '.')) 

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
