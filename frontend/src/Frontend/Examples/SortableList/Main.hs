{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Frontend.Examples.SortableList.Main
  (app)
  where

{-
 - DOM list sorting
 -}

import           Control.Lens
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Text    as T
import           Reflex.Dom
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Data.Dependent.Map (DMap)
import           Data.Functor.Constant
import           Data.Functor.Misc
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Time.Clock
import           System.Random

import           Reflex.Patch.MapWithMove

addRow :: DomBuilder t m => _ -> (Int, Int, Int) -> m ()
addRow _ (a, b, c) = el "tr" $ do
  -- mapM_ (el "td" . text . T.pack . show) [a, b, c]
  el "td" . text . T.pack . show $ a
  el "td" . text . T.pack . show $ b
  el "td" . text . T.pack . show $ c
  _ <- el "td" $ inputElement def
  return ()

app :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => m ()
app = do
  let numKeys = 1000 :: Int
      keys = take numKeys [0 :: Int ..]
      values :: [(Int, Int, Int)]
      values = zip3 (randoms (mkStdGen 0)) (randoms (mkStdGen 1)) (randoms (mkStdGen 2))
      testMap = Map.fromList $ zip keys values
      resortButtons = el "div" $ leftmost <$> sequence
        [
          (comparing (view _1) <$) <$> button "A ASC"
        , (flip (comparing (view _1)) <$) <$> button "A DESC"
        , (comparing (view _2) <$) <$> button "B ASC"
        , (flip (comparing (view _2)) <$) <$> button "B DESC"
        , (comparing (view _3) <$) <$> button "C ASC"
        , (flip (comparing (view _3)) <$) <$> button "C DESC"
        ]
  el "h1" $ text "Improved list sorting"
  el "p" $ text $ "This app shows a table of " <> T.pack (show numKeys) <> " rows, and allows you to re-sort by columns a, b, and c, which are full of pseudorandom numbers.  Each row also includes an element with internal state (a textbox) to demonstrate that the state is preserved, only when using the new way."
  el "h3" $ text "Re-sort the list the OLD way (by redrawing everything)"
  resortSlow <- resortButtons
  displayRedrawTime resortSlow
  el "h3" $ text "Re-sort the list the NEW way (by reordering existing elements)"
  resort <- resortButtons
  displayRedrawTime resort
  el "hr" blank
  el "table" $ do
    el "tr" $ do
      el "th" $ text "A"
      el "th" $ text "B"
      el "th" $ text "C"
      el "th" $ text "Stateful Element (textbox)"
    simpleSortableList addRow testMap resort resortSlow
  return ()

displayRedrawTime :: (DomBuilder t m, MonadHold t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => Event t a -> m ()
displayRedrawTime e = do
  et1 <- performEvent $ liftIO getCurrentTime <$ e
  et1' <- delay 0 et1
  diff <- performEvent $ ffor et1' $ \t1 -> do
    t2 <- liftIO getCurrentTime
    return $ t2 `diffUTCTime` t1
  text "Time: "
  dynText =<< holdDyn "not yet run" (T.pack . show <$> diff)

simpleSortableList :: forall t m k v. (MonadHold t m, MonadFix m, Adjustable t m, Ord k) => (k -> v -> m ()) -> Map k v -> Event t (v -> v -> Ordering) -> Event t (v -> v -> Ordering) -> m ()
simpleSortableList f m0 resortFunc resortSlowFunc = do
  rec let resortPatchFast = attachWith (flip patchThatSortsMapWith) (currentIncremental m) resortFunc
          redrawPatch :: Map k v -> (v -> v -> Ordering) -> PatchMapWithMove k v
          -- redrawPatch d cmp = unsafePatchMapWithMove $ fmap (MapEdit_Insert False) $ Map.fromList $ zip (Map.keys d) (sortBy cmp $ Map.elems d)
          -- TODO: Check if the old code above (doesn't compile) is roughly equivalent (in terms of performance) to the new one below
          redrawPatch = flip patchThatSortsMapWith
          resortPatchSlow = attachWith redrawPatch (currentIncremental m) resortSlowFunc
          resortPatch = leftmost
            [ resortPatchFast
            , resortPatchSlow
            ]
      m <- holdIncremental m0 resortPatch
  _ <- mapMapWithAdjustWithMove f m0 resortPatch
  return ()
