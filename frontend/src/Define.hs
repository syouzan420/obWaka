module Define where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import System.Random (StdGen)

type Pos = V2 Int
type Size = V2 Int
type Title = T.Text
type Counter = (T.Text,Int)
type Life = Maybe T.Text

data ETR = NoEvent | Save | LSave deriving stock (Eq,Show) 

data TextSection = TS Title T.Text deriving stock (Eq,Show)

type MapName = T.Text
type MapSize = Size 

type ObChar = Char
type ObName = T.Text

data Dir = NoDir | East | North | West | South deriving stock (Eq,Show,Enum)

data ObCon = CBlock | CMove | CGet | COn | CEnter deriving stock (Eq,Show)

data ObType = TKazu | TMozi | TFood | TTool | TTile | TBlock |
                      TFunc [ObType] | TLive ObLive
                                              deriving stock (Eq,Show) 

data ObLive = LStand | LMove Int Int | LApproach Int Int Int | LShoot Int Int |
              LBullet Int Int
                                               deriving stock (Eq,Show)


type ObDef = T.Text

data Object = Ob ObChar ObName ObType ObDef ObCon Dir Pos
                                                deriving stock (Eq,Show)

type ObMap = [Object]

data PEvent = PMove Pos | PBlock ObName | PPush ObName | PGet ObName | POn ObName |
              PPushTo ObName ObName | PPut ObName Pos |
              PFunc ObName ObChar | PEnter Pos ObName | PLeave |
              PConsume ObName | PAttack ObName | PShoot ObName | PNoLife |
              PNon
                                                 deriving stock (Eq,Show,Read)

type Code = T.Text

data WkEvent = WTick | WOk | WSub | WLeft | WUp | WDown | WRight
                                             deriving stock (Eq,Show)

data EvAct = EA PEvent Code Int Int deriving stock (Eq,Show)

data Ast = NAct | TAct | EAct deriving stock (Eq,Show)

data IMode = Txt | Cho | Mov | Ply | Wai | End | Ext deriving stock (Eq,Show,Read)

data Input = Ok | Sb | Ri | Up | Lf | Dn | Dm deriving stock (Eq,Show)

data HitSt = HNon | HBullet | HBulletTo ObName deriving stock (Eq,Show)


--imd: input mode (Txt or Cho or Ply)
--txs: text sections
--txw: tate text whole
--txv: tate text view
--tct: text count
--tcs: text count sub
--itx: is text showing?
--iths: is text high speed?
--ims: is map show?
--omp: object map
--tmp: temporary object map (for animation effect)
--mnm: map name
--msz: map size
--mps: map position
--pmp: previous map (Name,Position)
--evas: event actions
--chn: character number (0:NoChara)
--hav: something having (not having: Nothing)
--cho: choice destination (titles of texts)
--stg: standard random generator
--cnts: counters
--etr: event trigger
--lnt: link text
--lnu: link url
--cnn: count numbers
--llc: life lost count
--gmc: game clear numbers
data Game = Game {_imd :: !IMode
                 ,_txs :: ![TextSection]
                 ,_txw :: !T.Text
                 ,_txv :: !T.Text
                 ,_tct :: !Int
                 ,_tcs :: !Int
                 ,_itx :: !Bool
                 ,_iths :: !Bool
                 ,_ims :: !Bool
                 ,_omp :: !ObMap
                 ,_tmp :: !ObMap
                 ,_mnm :: !MapName
                 ,_msz :: !MapSize
                 ,_mps :: !Pos
                 ,_pmp :: !(MapName,Pos,ObMap)
                 ,_evas :: ![EvAct]
                 ,_chn :: !Int
                 ,_hav :: !(Maybe Object)
                 ,_cho :: ![T.Text]
                 ,_stg :: !StdGen
                 ,_cnts :: ![Counter]
                 ,_etr :: !ETR
                 ,_lif :: !Life
                 ,_lnt :: !T.Text
                 ,_lnu :: !T.Text
                 ,_cnn :: !Int
                 ,_llc :: !Int
                 ,_gmc :: !Int
                 } deriving stock (Eq,Show)

mapWinSize :: Size 
mapWinSize = V2 12 6
