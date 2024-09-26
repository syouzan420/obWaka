module Initialize where

import qualified Data.Text as T
import Linear.V2 (V2(..))
import System.Random (mkStdGen)
import Define

newGame :: Game
newGame = Game{_imd=Txt
              ,_txs=[],_txw=T.empty,_txv=T.empty
              ,_tct=0,_tcs=0
              ,_itx=True, _iths=False
              ,_ims=True
              ,_omp=[],_tmp=[]
              ,_mnm=T.empty,_msz=V2 0 0,_mps=V2 0 0
              ,_pmp=(T.empty,V2 0 0,[])
              ,_evas=[]
              ,_chn=0
              ,_hav=Nothing
              ,_cho=[]
              ,_stg=mkStdGen 100
              ,_cnts=[]
              ,_etr=NoEvent
              ,_lif=Nothing
              ,_lnt=T.empty, _lnu=T.empty
              ,_cnn=0, _llc=0, _gmc=0
              }
