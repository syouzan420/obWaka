module Converter where

import Linear.V2 (V2(..))
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (find,deleteBy,sort)
import Data.Tuple (swap)
import System.Random (mkStdGen)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Define
import Initialize (newGame)
import Object (blankObj)

makeLenses ''Game

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

dirToText :: Dir -> T.Text
dirToText dr = case dr of
  East -> "→"; North -> "↑"; West -> "←"; South -> "↓"; NoDir -> ""
      
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
  let lns = map (takeHTML mw . dropHTML mx) $ take mh $ drop my $ T.lines mpText
   in T.unlines lns

dropHTML :: Int -> T.Text -> T.Text
dropHTML 0 txt = txt
dropHTML i txt = if T.isPrefixOf "<span" txt then dropHTML (i-1) $ T.drop 7 $ snd 
                                                           $ T.breakOn "</span>" txt
                                             else dropHTML (i-1) $ T.tail txt

takeHTML :: Int -> T.Text -> T.Text
takeHTML 0 _ = T.empty 
takeHTML i txt 
        | T.isPrefixOf "<span" txt = 
            let (fs,sc) = T.breakOn "</span>" txt 
             in fs<>T.take 7 sc<>takeHTML (i-1) (T.drop 7 sc)
        | txt==T.empty = T.empty
        | otherwise = T.singleton (T.head txt)<>takeHTML (i-1) (T.tail txt)


makeObjectMap :: T.Text -> (ObMap,MapSize) 
makeObjectMap tx =  
  let lns = T.lines tx 
      w = if not (null lns) then T.length (head lns) else 0
      txnc = T.replace "\n" "" tx
      searchResult = searchObject 0 (T.unpack txnc)
   in (map (\(i,ch)->
       let p = mod i w ; q = div i w 
           oname 
              | ch==pChar = "player"
              | ch==oLeave = "leave"
              | otherwise = T.empty
           otype 
              | ch==pChar = TLive LStand
              | ch==oLeave = TTile
              | otherwise = TBlock
           ocon
              | ch==oLeave = COn
              | otherwise = CBlock
           ocolor
              | ch==pChar = Orange 
              | otherwise = Black
        in Ob ch oname otype T.empty ocon North ocolor (V2 p q))
                               searchResult,V2 w (length lns))

setObjectData :: [T.Text] -> ObMap -> ObMap
setObjectData _ [] = [] 
setObjectData obdts (ob@(Ob ch _ _ _ _ _ _ ps):obs) =
  let tgtDt = fromMaybe T.empty $
        find (\obdt -> 
          let dtl = T.splitOn "," obdt
           in case dtl of
                [_,_,_,_,_,_,_,tpx,tpy] ->
                   V2 ((read . T.unpack) tpx) ((read . T.unpack) tpy) == ps
                [tch,_,_,_,_,_,_] -> tch==T.singleton ch
                [tch,_,_,_,_,_] -> tch==T.singleton ch
                _ -> False ) obdts
      dtList = T.splitOn "," tgtDt
      newObj = case dtList of
        (_:tnm:ttp:tdf:tcn:tdr:tco:_) ->
               Ob ch tnm (txToType ttp) tdf (txToCon tcn) (txToDir tdr) (txToCol tco) ps 
        (_:tnm:ttp:tdf:tcn:tdr:_) ->
               Ob ch tnm (txToType ttp) tdf (txToCon tcn) (txToDir tdr) Black ps 
        _ -> ob
   in newObj:setObjectData obdts obs

makeObjectByName :: ObName -> [T.Text] -> Maybe Object
makeObjectByName _ [] = Nothing
makeObjectByName oname (obdt:xs) =
  let dtList = T.splitOn "," obdt 
   in case dtList of
        [tch,tnm,ttp,tdf,tcn,tdr,tco] ->
            if tnm==oname then 
                Just (Ob (T.head tch) tnm (txToType ttp) tdf 
                        (txToCon tcn) (txToDir tdr) (txToCol tco) (V2 0 0))
                          else makeObjectByName oname xs 
        [tch,tnm,ttp,tdf,tcn,tdr] ->
            if tnm==oname then 
                Just (Ob (T.head tch) tnm (txToType ttp) tdf 
                                 (txToCon tcn) (txToDir tdr) Black (V2 0 0))
                          else makeObjectByName oname xs 
        _ -> makeObjectByName oname xs 

txToType :: T.Text -> ObType
txToType "" = TBlock
txToType txt = let txts = T.words txt
                in case txts of
                    ("func":tps) -> TFunc (map findType tps)
                    ("move":ct:c) -> TLive (LMove ((read . T.unpack) ct) 
                                (if null c then 0 else (read . T.unpack) (head c)))
                    ("approach":rg:ct:c) -> 
                      TLive (LApproach ((read . T.unpack) rg) ((read . T.unpack) ct)
                                (if null c then 0 else (read . T.unpack) (head c)))
                    ("shoot":ct:c) -> TLive (LShoot ((read . T.unpack) ct)
                                (if null c then 0 else (read . T.unpack) (head c)))
                    ("bullet":ct:c) -> TLive (LBullet ((read . T.unpack) ct)
                                (if null c then 0 else (read . T.unpack) (head c)))
                    [tp] -> findType tp
                    _ -> TBlock

findType :: T.Text -> ObType
findType txt = fromMaybe TBlock $ lookup txt txType

txToCon :: T.Text -> ObCon
txToCon txt = fromMaybe CBlock $ lookup txt txCon 

txToDir :: T.Text -> Dir
txToDir txt = fromMaybe NoDir $ lookup txt txDir

txToCol :: T.Text -> Color
txToCol txt = fromMaybe Black $ lookup txt txColor 

txType :: [(T.Text,ObType)]
txType = [("kazu",TKazu),("mozi",TMozi),("live",TLive LStand),("food",TFood),("tool",TTool),("block",TBlock)]

txCon :: [(T.Text,ObCon)]
txCon = [("block",CBlock),("move",CMove),("get",CGet),("on",COn),("enter",CEnter)]

txDir :: [(T.Text,Dir)]
txDir = [("east",East),("north",North),("west",West),("south",South),("nodir",NoDir)]

txColor :: [(T.Text,Color)]
txColor = [("black",Black),("gray",Gray),("red",Red),("orange",Orange),("blue",Blue),("cyan",Cyan)]

searchObject :: Int -> String -> [(Int,Char)]
searchObject _ [] = []
searchObject i (x:xs) = if x==oNon then searchObject (i+1) xs
                                  else (i,x):searchObject (i+1) xs

showMap :: MapSize -> ObMap -> ObMap -> T.Text
showMap ms om tm = T.unlines $ showObMap (om <> tm) (makeFlatMap ms) 

makeFlatMap :: MapSize -> FlatMap
makeFlatMap (V2 w h) = replicate h $ T.pack (replicate w oNon)

showObMap :: ObMap -> FlatMap -> [T.Text]
showObMap [] mtx = mtx 
showObMap ((Ob ch _ _ _ _ _ co (V2 x y)):xs) mtx = showObMap xs (insertChar ch co x y mtx)

insertChar :: Char -> Color -> Int -> Int -> [T.Text] -> [T.Text]
insertChar ch co x y txList =  
  let ln = txList!!y
      (hd,tl) = (takeHTML x ln,dropHTML (x+1) ln)
      chtx = T.singleton ch
      insTxt = if co==Black then chtx else
         "<span style=\"color:"<>coToText co<>";\">"<>chtx<>"</span>" 
      nln = hd <> insTxt <> tl
      (bln,aln) = splitAt y txList 
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
                      
type IsTyping = Bool

getInfoFromChar :: T.Text -> Int 
              -> (IsTyping,DType,Char,T.Text,Int)
getInfoFromChar wtx i = 
  let ch = T.index wtx i
      isCode = ch==';'
      isStop = ch=='。'
      isRubi = ch=='$' && T.index wtx (i+1)=='r'
      dtype
        | isStop = DStop
        | isRubi = DRubi
        | isCode = DCode
        | otherwise = DPlane Black
      isTyping = not (isStop || ch=='、' || isCode)
      doc
        | isCode = T.tail (T.takeWhile (/='\n') (T.drop i wtx))
        | isRubi = T.takeWhile (/=' ') (T.drop (i+2) wtx)
        | otherwise = T.empty
      scanLength 
        | isCode = T.length doc + 1 
        | isRubi = T.length doc + 3
        | otherwise = 1
   in (isTyping,dtype,ch,doc,scanLength)

makeRubiHtml :: T.Text -> T.Text
makeRubiHtml rubiText =
  let txRubi = T.splitOn "-" rubiText
   in case txRubi of
        [tx,rb] -> "<ruby>"<>tx<>"<rp>(</rp><rt>"<>rb<>"</rt><rp>)</rp></ruby>"
        _ -> T.empty

lookupFromSections :: [TextSection] -> T.Text -> T.Text
lookupFromSections textSections tx = 
  let tsKeyValues = map (\(TS ti t) -> (ti,t)) textSections
   in fromMaybe T.empty (lookup tx tsKeyValues)  

updateTextSection :: TextSection -> [TextSection] -> [TextSection]
updateTextSection _ [] = [] 
updateTextSection ts@(TS ti ntx) (TS title ptx:xs) = 
  if ti==title then TS title ntx:xs 
               else TS title ptx:updateTextSection ts xs

updateMapData :: T.Text -> ObMap -> T.Text
updateMapData tx obMap = 
  let mpLines = T.lines tx
      newMpLines = makeMpLines 0 obMap mpLines
   in T.unlines newMpLines

makeMpLines :: Int -> ObMap -> [T.Text] -> [T.Text]
makeMpLines _ _ [] = []
makeMpLines i obMap (ml:xs) =
  let listXCh = makeListXCh i obMap
      plInd = T.findIndex (==pChar) ml
   in T.pack (makeMpLine plInd (T.length ml - 1) (reverse $ sort listXCh))
          :makeMpLines (i+1) obMap xs
      
makeListXCh :: Int -> ObMap -> [(Int,ObChar)]
makeListXCh _ [] = []
makeListXCh i (Ob ch _ _ _ _ _ _ (V2 px py):xs) 
  = if i==py then (px,ch):makeListXCh i xs else makeListXCh i xs 

makeMpLine :: Maybe Int -> Int -> [(Int,ObChar)] -> String
makeMpLine Nothing 0 [] = [oNon]  
makeMpLine (Just n) 0 [] = if n==0 then [pChar] else [oNon]  
makeMpLine _ 0 [(0,ch)] = [ch]
makeMpLine Nothing x [] = makeMpLine Nothing (x-1) []<>[oNon]
makeMpLine (Just n) x [] = makeMpLine (Just n) (x-1) []
                                <>if n==x then [pChar] else [oNon] 
makeMpLine Nothing x xch@((i,ch):xs) = 
  if x==i then makeMpLine Nothing (x-1) xs <> [ch]
          else makeMpLine Nothing (x-1) xch <> [oNon] 
makeMpLine (Just n) x xch@((i,ch):xs) = 
  if x==i then makeMpLine (Just n) (x-1) xs <> [ch]
          else makeMpLine (Just n) (x-1) xch <> if x==n then [pChar] else [oNon] 


updateObjectData :: T.Text -> Object -> T.Text
updateObjectData tx (Ob ch nm tp df cn dr co (V2 px py)) = 
  let txList = T.lines tx
      traceText = T.singleton ch <> "," <> nm 
      traceLng = T.length traceText
      newList = deleteBy (\t1 t2-> t1==T.take traceLng t2) traceText txList 
      tpTx = tpToText tp 
      cnTx = cnToText cn
      drTx = drToText dr 
      coTx = coToText co
   in T.unlines $ traceText<>","<>tpTx<>","<>df<>","<>cnTx<>","<>drTx<>","
                    <>coTx<>","<>(T.pack . show) px<>","<>(T.pack . show) py:newList

makeObjectDatas :: ObMap -> [T.Text]
makeObjectDatas  = map objToText 

objToText :: Object -> T.Text
objToText (Ob ch nm tp df cn dr co (V2 px py)) =
  let tpTx = tpToText tp
      cnTx = cnToText cn
      drTx = drToText dr 
      coTx = coToText co
   in T.singleton ch<>","<>nm<>","<>tpTx<>","<>df<>","<>cnTx<>","
         <>drTx<>","<>coTx<>","<>(T.pack . show) px<>","<>(T.pack . show) py

drToText :: Dir -> T.Text
drToText dr = fromMaybe "nodir" $ lookup dr $ map swap txDir

cnToText :: ObCon -> T.Text
cnToText cn = fromMaybe T.empty $ lookup cn $ map swap txCon 

coToText :: Color -> T.Text
coToText co = fromMaybe "black" $ lookup co $ map swap txColor  
   
tpToText :: ObType -> T.Text
tpToText (TFunc []) = "func"
tpToText (TFunc tps) = "func "<>T.intercalate " " (map tpToText tps) 
tpToText (TLive (LMove ct c)) = "move "<>(T.pack . show) ct<>" "<>(T.pack . show) c
tpToText (TLive (LApproach rg ct c)) = 
    "approach "<>(T.pack . show) rg<>" "<>(T.pack . show) ct<>" "<>(T.pack . show) c
tpToText (TLive (LShoot ct c)) = 
                    "shoot "<>(T.pack . show) ct<>" "<>(T.pack . show) c
tpToText (TLive (LBullet ct c)) = 
                    "bullet "<>(T.pack . show) ct<>" "<>(T.pack . show) c
tpToText tp = fromMaybe T.empty $ lookup tp $ map swap txType

makeGameStateText :: Game -> T.Text
makeGameStateText gs =
  let imdText = (T.pack . show) (gs^.imd) 
      txsText = txsToText (gs^.txs) 
      ompText = T.intercalate ":" $ makeObjectDatas (gs^.omp) 
      mszText = posToText (gs^.msz)
      mimText = (T.pack . show) (gs^.mim)
      mpsText = posToText (gs^.mps) 
      pmpText = pmpToText (gs^.pmp)
      evasText = evasToText (gs^.evas)
      havText = maybe T.empty objToText (gs^.hav) 
      cntsText = cntsToText (gs^.cnts)
      lifText = (T.pack . show) (gs^.lif)
      llcText = (T.pack . show) (gs^.llc)
      gmcText = (T.pack . show) (gs^.gmc)
   in T.intercalate "~" [imdText,txsText,ompText,gs^.mnm,mszText,mimText,mpsText,pmpText,evasText,havText,cntsText,lifText,llcText,gmcText]

pmpToText :: [(MapName,Pos,ObMap)] -> T.Text
pmpToText [] = "^"
pmpToText ((pMapName,pMapPos,pObMap):xs) = "^"<> pMapName <>"^"<> posToText pMapPos
              <>"^"<> T.intercalate ":" (makeObjectDatas pObMap) <> pmpToText xs

posToText :: Pos -> T.Text
posToText (V2 x y) = (T.pack . show) x <> ":" <> (T.pack . show) y 

txsToText :: [TextSection] -> T.Text
txsToText tTxs = T.intercalate ":" $ foldr (\(TS ti tx) acc -> ti:tx:acc) [] tTxs

cntsToText :: [Counter] -> T.Text
cntsToText tCnts = T.intercalate ":" $ 
    foldr (\(tx,n) acc -> tx:(T.pack . show) n:acc) [] tCnts

evasToText :: [EvAct] -> T.Text
evasToText tEvas = T.intercalate ":" $
    foldr (\(EA pe cd i n) acc -> 
          (T.pack . show) pe:cd:(T.pack . show) i:(T.pack . show) n:acc) [] tEvas

toGameState :: T.Text -> Game
toGameState tx = case T.splitOn "~" tx of 
    [imdText,txsText,ompText,nmnm,mszText,mimText,mpsText,pmpText,evasText,havText,cntsText,lifText,llcText,gmcText] -> 
        let nimd = read (T.unpack imdText) :: IMode
            ntxs = txToTxs txsText
            nomp = txToOmp ompText
            nmsz = txToPos mszText
            nmim = (read . T.unpack) mimText
            nmps = txToPos mpsText
            npmp = txToPmp pmpText
            nevas = txToEvas evasText
            nhav = if havText==T.empty then Nothing else Just (txToObject havText) 
            ncnts = txToCnts cntsText
            nlif = (read . T.unpack) lifText 
            nllc = (read . T.unpack) llcText
            ngmc = (read . T.unpack) gmcText
         in Game{_imd=nimd,_txs=ntxs,_txw=T.empty,_txv=T.empty,_tct=0,_tcs=0
                ,_itx=False,_iths=False,_ims=True,_omp=nomp,_tmp=[],_mnm=nmnm
                ,_msz=nmsz,_mim=nmim
                ,_mps=nmps,_pmp=npmp,_evas=nevas,_chn=0,_hav=nhav,_cho=[],_tip=[]
                ,_stg=mkStdGen 100,_cnts=ncnts,_etr=NoEvent,_lif=nlif
                ,_lnt=T.empty,_lnu=T.empty,_cnn=0,_llc=nllc,_gmc=ngmc}
    _ -> newGame 

txToPmp :: T.Text -> [(MapName,Pos,ObMap)]
txToPmp "^" = []
txToPmp tx = let tls = T.splitOn "^" ((T.tail . T.init) tx)
              in txToPmp' tls

txToPmp' :: [T.Text] -> [(MapName,Pos,ObMap)]
txToPmp' [] = []
txToPmp' [_] = []
txToPmp' [_,_] = []
txToPmp' (txMapName:txPos:txObMap:xs) = 
              (txMapName,txToPos txPos,txToOmp txObMap):txToPmp' xs

txToCnts :: T.Text -> [Counter]
txToCnts tx = txToCnts' (T.splitOn ":" tx)

txToCnts' :: [T.Text] -> [Counter]
txToCnts' [] = []
txToCnts' [_] = []
txToCnts' (tx:txN:xs) = (tx,(read . T.unpack) txN):txToCnts' xs

txToEvas :: T.Text -> [EvAct]
txToEvas tx = txToEvas' (T.splitOn ":" tx)

txToEvas' :: [T.Text] -> [EvAct]
txToEvas' [] = []
txToEvas' [_] = []
txToEvas' [_,_] = []
txToEvas' [_,_,_] = []
txToEvas' (txPe:cd:txI:txN:xs) = 
            EA ((read . T.unpack) txPe :: PEvent) cd 
                      ((read . T.unpack) txI) ((read . T.unpack) txN) :txToEvas' xs

txToPos :: T.Text -> Pos
txToPos tx = case T.splitOn ":" tx of
                [px,py] -> V2 ((read . T.unpack) px) ((read . T.unpack) py)
                _ -> V2 0 0

txToTxs :: T.Text -> [TextSection]
txToTxs tx = txToTxs' (T.splitOn ":" tx)

txToTxs' :: [T.Text] -> [TextSection]
txToTxs' [] = []
txToTxs' [_] = []
txToTxs' (ti:tx:xs) = TS ti tx:txToTxs' xs

txToOmp :: T.Text -> ObMap
txToOmp tx = let nomp = txToOmp' (T.splitOn ":" tx)
              in if nomp==[blankObj] then [] else nomp

txToOmp' :: [T.Text] -> ObMap
txToOmp'  = map txToObject 

txToObject :: T.Text -> Object
txToObject obtx =
  case T.splitOn "," obtx of
    (txCh:nm:txTp:df:txCn:txDr:xs) ->
        let ch = maybe ' ' fst $ T.uncons txCh 
            tp = txToType txTp
            cn = txToCon txCn
            dr = txToDir txDr
            co = case xs of
                  [txCol,_,_] -> txToCol txCol 
                  [txCol] -> txToCol txCol 
                  _ -> Black
            ps = case xs of 
                  [_,txPx,txPy] -> 
                        V2 ((read . T.unpack) txPx) ((read . T.unpack) txPy)
                  _ -> V2 0 0
         in Ob ch nm tp df cn dr co ps
    _ -> blankObj 
