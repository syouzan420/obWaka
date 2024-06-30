module Define where

import qualified Data.Text as T
import Linear.V2 (V2(..))

type Pos = V2 Int
type Title = T.Text

data TextSection = TS Title T.Text deriving stock (Eq,Show)


type ObChar = Char
type ObName = T.Text
data ObType = Kaz | Moz | Cha | Fod | Tol | Blk | Fun [ObType] 
                                              deriving stock (Eq,Show) 

type ObDef = T.Text

data Object = Ob ObChar ObName ObType ObDef Pos deriving stock (Eq,Show)

type ObMap = [Object]

data PEvent = PMove Pos | PBlock ObName | ENon deriving stock (Eq,Show)

type Code = T.Text

data EvAct = EA PEvent Code Int deriving stock (Eq,Show)

data Ast = NAct | TAct | EAct deriving stock (Eq,Show)

data IMode = Txt | Ply deriving stock (Eq,Show)

data Input = Ok | Cn | Ri | Up | Lf | Dn | Dm deriving stock (Eq,Show)

data Dir = East | EN | North | NW | West | WS | South | SE | NoDir deriving stock (Eq,Show)

--txs: text sections
--txw: tate text whole
--txv: tate text view
--tct: text count
--tsc: text scroll (from end)
--itx: is text showing?
--omp: object map
--mps: map position
--evas: event actions
data Game = Game {_imd :: !IMode
                 ,_txs :: ![TextSection]
                 ,_txw :: !T.Text
                 ,_txv :: !T.Text
                 ,_tct :: !Int
                 ,_tsc :: !Int
                 ,_itx :: !Bool
                 ,_omp :: !ObMap
                 ,_mps :: !Pos
                 ,_evas :: ![EvAct]
                 } deriving stock (Eq,Show)

data WkEvent = WTick | WOk | WCancel | WLeft | WUp | WDown | WRight deriving stock (Eq,Show)

textWidth :: Int
textWidth = 15

textHeight :: Int
textHeight = 18

mapWinSize :: Pos
mapWinSize = V2 12 6
