module Initialize where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import Define

newGame :: Game
newGame = Game{_imd=Txt
              ,_txs=[],_txw=T.empty,_txv=T.empty
              ,_tct=0,_tcs=0
              ,_itx=True
              ,_omp=[],_msz=V2 0 0,_mps=V2 0 0
              ,_evas=[]
              }
