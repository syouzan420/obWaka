module CWidget ( elChara, elSpace, evElButton, evElButtonH, mkHidden
               , evElNumberPad, dyElTimer, dyElCharaAnime) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Obelisk.Generated.Static (static)

import Reflex.Dom.Core 
  ( text, dynText, el, elAttr, divClass, elAttr', blank
  , (=:), leftmost, elDynAttr, elDynAttr' ,holdDyn, domEvent
  , current, gate, tickLossyFromPostBuildTime
  , DomBuilder, PerformEvent, TriggerEvent
  , PostBuild, Event, EventName(Click), MonadHold ,Dynamic
  , Performable, TickInfo(..)
  )

mkHidden :: Bool -> Map.Map T.Text T.Text
mkHidden False = "hidden" =: ""
mkHidden True = mempty

elSpace :: DomBuilder t m => m ()
elSpace = el "p" $ text ""

evElButton :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
evElButton c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

evElButtonH ::
  ( DomBuilder t m
  , PostBuild t m
  ) => Dynamic t Bool -> T.Text -> T.Text -> m (Event t ())
evElButtonH dyB c s = do
  (e, _) <- elDynAttr' "button" dyBHide $ text s
  return $ domEvent Click e
  where mkHidden' False = "hidden" =: "" <> otherAttr
        mkHidden' True = otherAttr
        dyBHide = mkHidden' <$> dyB
        otherAttr = "type" =: "button" <> "class" =: c

evElNumberPad :: DomBuilder t m => Int -> m (Event t T.Text)
evElNumberPad i = do
  divClass "gr" $ do
    evts <- mapM (\n -> (toText n <$) <$> evElNumberButton (toText n)) [1..i]
    return $ leftmost evts 
  where
    evElNumberButton = evElButton "pad" 
    toText = T.pack . show

dyElTimer :: 
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  ) => Dynamic t Bool -> m (Dynamic t T.Text)
dyElTimer b = do
  evTime <- tickLossyFromPostBuildTime 1 
  let beBool = current b 
  let evNTime = gate beBool evTime
  let evTimeText = T.pack . show . (+1) . _tickInfo_n <$> evNTime
  holdDyn "start" evTimeText 
  
dyElCharaAnime :: 
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  ) => Dynamic t Bool -> m (Dynamic t T.Text)
dyElCharaAnime dyBool = do
  dyTime <- dyElTimer dyBool 
  let dToggle = fmap (\tx -> (tx/="start") &&
                     (rem ((read . T.unpack) tx) 2==(0::Int))) dyTime
  let 
    dNotToggle = not <$> dToggle
    dHide1 = mkHidden <$> dToggle
    dHide2 = mkHidden <$> dNotToggle
  el "p" $ text ""
  elDynAttr "div" dHide1 $ do elChara0; dynText dyTime 
  elDynAttr "div" dHide2 $ do elChara1; dynText dyTime 
  pure dyTime 
    
  
elChara :: DomBuilder t m => m ()
elChara = elAttr "img" ("src" =: $(static "chara0_mid.png")) blank

elChara0 :: DomBuilder t m => m ()
elChara0 = elAttr "img" ("src" =: $(static "chara0.png")) blank

elChara1 :: DomBuilder t m => m ()
elChara1 = elAttr "img" ("src" =: $(static "chara1.png")) blank

