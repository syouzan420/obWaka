module Define where

import qualified Data.Text as T
import Linear.V2 (V2(..))

type Pos = V2 Int
type Size = V2 Int
type Title = T.Text

data TextSection = TS Title T.Text deriving stock (Eq,Show)

type MapName = T.Text
type MapSize = Size 

type ObChar = Char
type ObName = T.Text

data Dir = East | North | West | South | NoDir deriving stock (Eq,Show)

data ObCon = CBlock | CMove | CGet | COn | CEnter deriving stock (Eq,Show)

data ObType = TKazu | TMozi | TLive | TFood | TTool | TBlock | TFunc [ObType] 
                                              deriving stock (Eq,Show) 


type ObDef = T.Text

data Object = Ob ObChar ObName ObType ObDef ObCon Dir Pos
                                                deriving stock (Eq,Show)

type ObMap = [Object]

data PEvent = PMove Pos | PBlock ObName | PPush ObName | PPut ObName Pos | PNon deriving stock (Eq,Show)

type Code = T.Text

data WkEvent = WTick | WOk | WLeft | WUp | WDown | WRight deriving stock (Eq,Show)

data EvAct = EA PEvent Code Int Int deriving stock (Eq,Show)

data Ast = NAct | TAct | EAct deriving stock (Eq,Show)

data IMode = Txt | Ply deriving stock (Eq,Show)

data Input = Ok | Cn | Ri | Up | Lf | Dn | Dm deriving stock (Eq,Show)


--imd: input mode (Txt or Ply)
--txs: text sections
--txw: tate text whole
--txv: tate text view
--tct: text count
--tcs: text count sub
--itx: is text showing?
--iths: is text high speed?
--omp: object map
--tmp: temporary object map (for animation effect)
--mnm: map name
--msz: map size
--mps: map position
--evas: event actions
--chn: character number (0:NoChara)
--hav: something having (not having: Nothing)
data Game = Game {_imd :: !IMode
                 ,_txs :: ![TextSection]
                 ,_txw :: !T.Text
                 ,_txv :: !T.Text
                 ,_tct :: !Int
                 ,_tcs :: !Int
                 ,_itx :: !Bool
                 ,_iths :: !Bool
                 ,_omp :: !ObMap
                 ,_tmp :: !ObMap
                 ,_mnm :: !MapName
                 ,_msz :: !MapSize
                 ,_mps :: !Pos
                 ,_evas :: ![EvAct]
                 ,_chn :: !Int
                 ,_hav :: !(Maybe Object)
                 } deriving stock (Eq,Show)

mapWinSize :: Size 
mapWinSize = V2 12 6
