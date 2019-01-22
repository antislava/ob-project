{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase        #-}

module Frontend.Examples.DataTable.Main
  (app)
  where

{-
 - DOM list sorting
 -}

import           Control.Error
import           Data.Aeson
import           Data.Bifunctor.Tannen
import           Data.Functor.Compose
import qualified Data.Text    as T
import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Data.List.NonEmptyZipper (NonEmptyZipper)
import qualified Data.List.NonEmptyZipper as NEZ
import qualified Text.Fuzzy as F
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Common
import           Text.Regex.TDFA.Text
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Vanishing hiding (goneWhen)

import           Language.Javascript.JSaddle

import           Control.Lens
import qualified Data.Map as Map
import           Data.Map (Map)
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

import           Common.Examples.DataTable.PackageData
import           Frontend.Examples.DataTable.Types

------------------------------------------------------------------------------
-- from Contrib.Vanishing Relaxing exxessive type constraint. TODO: fix Contrib
------------------------------------------------------------------------------
-- | This function is a little different than the others.  Instead of setting
-- a display:none or visibility:hidden style when the element is invisible,
-- this function removes the widget from the DOM completely.
goneWhen
    -- :: MonadWidget t m
    :: (DomBuilder t m, PostBuild t m)
    => m a
    -> Dynamic t DomExistence
    -> m a
    -> m (Event t a)
goneWhen defWidget existence widget = dyn (dontShow <$> existence)
  where
    dontShow NotInDom = defWidget
    dontShow InDom = widget

-- | Convenience function to decode JSON-encoded responses.
decodeXhrEither :: FromJSON a => XhrResponse -> Either String a
decodeXhrEither = join . note "error retrieving data" .
                  fmap (eitherDecode . BL.fromStrict . encodeUtf8) .
                  _xhrResponse_responseText

------------------------------------------------------------------------------

-- main' :: IO ()
-- main' = mainWidget $ do
app ::
    ( DomBuilder t m
    , PostBuild t m
    , MonadFix m
    , MonadHold t m
    -- , DomBuilderSpace m ~ GhcjsDomSpace -- TAKEN CARE OF BY PRERENDER
    , TriggerEvent t m
    , Prerender js m
    , PerformEvent t m
    , PerformEvent t m
    , MonadIO (Performable m)
    )
    => m ()
app = prerender blank $ do
  pb <- getPostBuild
  resp :: Event _t XhrResponse <- performRequestAsync (xhrRequest "GET" "/get-data/dummy" def <$ pb)
  let respText = _xhrResponse_responseText <$> resp
  respTextFirst :: Event _t (Maybe Text) <- headE respText
  -- -- performEvent_ $ (liftIO $ getCurrentTime >>= print) <$ respTextFirst
  -- -- performEvent_ $ liftIO . print <$> respTextFirst
  -- performEvent_ $ respTextFirst <&>
  --   liftIO . (BL.putStr "RESPONSE STATS: " <>) . print .
  --   fmap length . (decodeText @[PackageData] =<<)
  zzz :: Dynamic _t Text <- (fmap.fmap) (fromMaybe "") $
    holdDyn Nothing $ _xhrResponse_responseText <$> resp
  packages :: _ <- holdDyn InFlight (eitherToFlight . decodeXhrEither <$> resp)
  -- DEBUG LOCAL
  packagesFirst <- headE $ updated packages
  performEvent_ $ packagesFirst <&> liftIO . \case
    InFlight -> print "InFlight"
    ReturnedError e -> print e
    ReturnedValue p -> print p
  si <- menu
  _ <- elAttr "div" ("class" =: "ui container") $ do
      -- dynText zzz
      dyn (res si <$> packages)
  blank

res ::
    -- MonadWidget t m
    ( DomBuilder t m
    , PostBuild t m
    , MonadFix m
    , MonadHold t m
    )
    => Dynamic t SearchInfo
    -> FlightStatus [PackageData]
    -> m ()
res _ InFlight = do
  divClass "ui" $ do
    divClass "ui active inverted dimmer" $ do
      divClass "ui medium text loader" $ text "Loading"
    el "p" $ text "\160"
    el "p" $ text "\160"
    el "p" $ text "\160"
res _ (ReturnedError e) = do
  divClass "ui negative message" $
    el "p" $ text $ T.pack e
res si (ReturnedValue ps) = do
  resultsBox si ps
  return ()

menu ::
  -- MonadWidget t m
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadJSM (Performable m)
  , Prerender js m
  )
  => m (Dynamic t SearchInfo)
menu = mdo
  divClass "ui top fixed menu" $ do
    divClass "header item" $ do
      text "Haskell "
      elAttr "img" ("src" =: "img/haskell-logo.svg") blank
      text " Package Explorer"
    divClass "ui right menu" $ mdo
      let addError staticAttrs en =
            "class" =: either (const $ T.unwords [staticAttrs, "error"])
                              (const staticAttrs) en
      let changePlaceholder staticAttrs en =
            "placeholder" =: either (const "Invalid regex")
                              (const staticAttrs) en
      str <- prerender (return "") $ divClass "ui right aligned category search item" $
        elDynAttr "div" (addError "ui transparent icon input" <$> needle) $ do
          ti <- textInput $ def
            & attributes .~ (changePlaceholder "Search for a package" <$> needle)
          f <- delay 0 =<< getPostBuild
          _ <- performEvent $
                 liftJSM (pToJSVal (_textInput_element ti) ^. js0 ("focus" :: String)) <$ f
          elClass "i" "search icon" blank
          return $ value ti

      algo <- semUiDropdownMenu "algo-dropdown" "Search Type" $ mdo
        s <- algoItem curAlgo SubstringAlgo
        f <- algoItem curAlgo FuzzyAlgo
        r <- algoItem curAlgo RegexAlgo
        curAlgo <- holdDyn (_siSearchAlgo defSI) $ leftmost [s,f,r]
        return curAlgo
      fields <- semUiDropdownMenu "field-includes" "Fields" $ mdo
        divClass "ui header" $ text "Include"
        (n,on) <- fieldCheckboxDyn "Package Name" (_sfName defFields) ups
        (s,os) <- fieldCheckboxDyn "Synopsis" (_sfSynopsis defFields) ups
        (d,od) <- fieldCheckboxDyn "Description" (_sfDescription defFields) ups
        (c,oc) <- fieldCheckboxDyn "Categories" (_sfCategories defFields) ups
        (a,oa) <- fieldCheckboxDyn "Author" (_sfAuthor defFields) ups
        (m,om) <- fieldCheckboxDyn "Maintainer" (_sfMaintainer defFields) ups
        let ups = leftmost [on, os, od, oc, oa, om]
        return $ SearchFields <$> n <*> s <*> d <*> c <*> a <*> m
      csDyn <- menuToggle "Case Sensitive" (_siCaseSensitive defSI)

      let needle = do
            cs <- csDyn
            a <- algo
            s <- str
            let opt = CompOption cs False True True False
            return $ case a of
              SubstringAlgo -> Right $ StrNeedle s
              FuzzyAlgo -> Right $ FuzzyNeedle s
              RegexAlgo -> RegexNeedle <$> compile opt (ExecOption False) s

      let esi = getCompose $
            SearchInfo <$> Compose needle
                       <*> Compose (fmap pure fields)
                       <*> Compose (fmap pure csDyn)
                       <*> Compose (fmap pure algo)

      holdDyn defSI $ leftmost
        [ ffilter longNeedle $ fmapMaybe hush $ updated esi
        ]

------------------------------------------------------------------------------

algoItem
    -- :: MonadWidget t m
    :: (DomBuilder t m, PostBuild t m)
    => Dynamic t SearchAlgo
    -> SearchAlgo
    -> m (Event t SearchAlgo)
algoItem d algo = do
    (e,_) <- elDynAttr' "a" (attrs algo <$> d) $ text $ showAlgo algo
    return $ algo <$ domEvent Click e
  where
    attrs a b = "class" =: ("item" <> if a == b then " active" else "")

longNeedle :: SearchInfo -> Bool
longNeedle si = case _siNeedle si of
                  StrNeedle t -> T.length t > 1
                  FuzzyNeedle t -> T.length t > 1
                  RegexNeedle _ -> True

menuToggle label iv = mdo
    val <- toggle iv (domEvent Click e)
    let attrs v = "class" =: ("item" <> if v then " active" else "")
    (e,_) <- elDynAttr' "div" (attrs <$> val) $ text label
    return val

semUiDropdownMenu menuId name menuBody = mdo
    isOpen <- holdDyn False $ leftmost
      [ True <$ domEvent Mouseover e
      , False <$ domEvent Mouseout e
      ]
    (e,res) <- elAttr' "div" ("class" =: "ui dropdown item") $ do
      text name
      elClass "i" "dropdown icon" blank
      let attrs open = "id" =: menuId <>
            "class" =: ("left menu transition " <>
                        if open then "visible" else "hidden")
      elDynAttr "div" (attrs <$> isOpen) menuBody
    return res

clearIncludes :: SearchInfo -> SearchInfo
clearIncludes si = si & siFields .~ noFields

fieldCheckboxDyn
    :: (DomBuilder t m, PostBuild t m, MonadFix m)
    => Text
    -> Bool
    -> Event t ()
    -> m (Dynamic t Bool, Event t ())
fieldCheckboxDyn label iv clear = do
    divClass "item" $ mdo
      res <- divClass "ui checkbox" $ do
        ck <- checkbox iv $ def & checkboxConfig_setValue .~
                -- We actually NEED leftmost here because we want the True value
                -- from this box to inhibit the false value it will get from the
                -- collective "only" event.
                leftmost [True <$ domEvent Click e, False <$ clear]
        el "label" $ text label
        return $ value ck
      (e,_) <- elAttr' "a" ("class" =: "select-only") $ text "only"
      return $ (res, domEvent Click e)

buttonOption
    :: (Eq a, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
    => (a -> Text)
    -> NonEmptyZipper a
    -> m (Dynamic t a)
buttonOption render items = mdo
    cur <- holdDyn (NEZ._current items) evt
    let mkItem val = do
          (e,_) <- elDynAttr' "span" (mkAttrs val <$> cur) $ text $ render val
          return $ val <$ domEvent Click e
    evt <- divClass "ui buttons" $ do
      leftmost . NEZ.toList <$> mapM mkItem items
    return cur
  where
    mkAttrs this selected = if this == selected
                              then ("class" =: "ui positive button")
                              else ("class" =: "ui button")

sortHeader
    :: (DomBuilder t m, PostBuild t m)
    => Dynamic t SortInfo
    -> Column
    -> CssClass
    -> Text
    -> m (Event t SortInfo)
sortHeader curSort col klass nm = do
    (e,_) <- elDynKlass' "th" (mkClass <$> curSort) $ text nm
    return $ newSortEvent <$> tag (current curSort) (domEvent Click e)
  where
    newSortEvent (so, sc) =
      if sc == col
        then (flipSortOrder so, col)
        else (Ascending, col)
    mkClass (so, sc) =
      if sc /= col
        then klass
        else manyClasses ["sorted", orderClass so] `mappend` klass

resultsBox
    :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t SearchInfo -> [PackageData] -> m ()
resultsBox si ps = do
  elAttr "table" ("class" =: "ui selectable sortable celled table") $ do
    curSort <- el "thead" $ el "tr" $ mdo
      cs <- holdDyn (Ascending, Col1) $ traceEvent "sort click" $ leftmost [a,b,c,d,e]
      a <- sortHeader cs Col1 mempty "Package Name"
      b <- sortHeader cs Col2 mempty "Synopsis"
      c <- sortHeader cs Col3 mempty "Maintainer"
      d <- sortHeader cs Col4 mempty "Downloads"
      e <- sortHeader cs Col5 (manyClasses ["two", "wide"]) "Last Uploaded"
      return cs
    el "tbody" $ dyn $ mapM_ packageWidget <$> (filterPackages <$> fmap (applySort ps) curSort <*> si)
    return ()

colSortFunc :: SortOrder -> Column -> PackageData -> PackageData -> Ordering
colSortFunc Ascending c =
    case c of
      Col1 -> comparing packageDataName
      Col2 -> comparing packageDataSynopsis
      Col3 -> comparing packageDataMaintainer
      Col4 -> comparing packageDataDownloads
      Col5 -> comparing packageDataLastUploaded
colSortFunc Descending c =
    case c of
      Col1 -> comparing (Down . packageDataName)
      Col2 -> comparing (Down . packageDataSynopsis)
      Col3 -> comparing (Down . packageDataMaintainer)
      Col4 -> comparing (Down . packageDataDownloads)
      Col5 -> comparing (Down . packageDataLastUploaded)

applySort :: [PackageData] -> (SortOrder, Column) -> [PackageData]
applySort ps (o,c) = sortBy (colSortFunc o c) ps

packageWidget :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => PackageData -> m ()
packageWidget (PackageData n s d cat auth maint nd lu) = mdo
    (e,_) <- el' "tr" $ do
      el "td" $
        elAttr "a" ("href" =: ("http://hackage.haskell.org/package/" <> n) <>
                    "target" =: "_blank") $ text n
      el "td" $ text s
      el "td" $ text maint
      el "td" $ text $ maybe mempty (T.pack . show) nd
      el "td" $ text $ maybe mempty (T.pack . show) lu
      return ()
    showDesc <- foldDyn ($) NotInDom (flipDom <$ domEvent Click e)
    goneWhen blank showDesc $ do
      el "tr" $ do
        el "td" $ text "Author"
        elAttr "td" ("colspan" =: "4") $ text auth
      el "tr" $ do
        el "td" $ text "Categories"
        elAttr "td" ("colspan" =: "4") $ text cat
      el "tr" $ do
        el "td" $ text "Description"
        elAttr "td" ("colspan" =: "4") $ text d
    return ()
  -- where
  --   desc _ = blank
  --   desc False = blank
  --   desc True = elClass "div" "four wide" $ do
  --     el "p" $ text $ "Author: " <> auth
  --     el "p" $ text $ "Maintainer: " <> maint
  --     el "p" $ text $ "Categories: " <> cat
  --     el "p" $ text d

flipDom NotInDom = InDom
flipDom InDom = NotInDom

filterPackages ps si =
    take 100 $ filter (packageVisible si) ps

packageVisible (SearchInfo str (SearchFields sn ss sd scats sauthor smaint) sc sa)
               (PackageData nm syn desc cat auth maint _ _) =
    (sn && search str (cf nm)) ||
    (ss && search str (cf syn)) ||
    (sd && search str (cf desc)) ||
    (scats && search str (cf cat)) ||
    (sauthor && search str (cf auth)) ||
    (smaint && search str (cf maint))
  where
    search needle haystack =
      case needle of
        StrNeedle n -> T.isInfixOf (cf n) haystack
        FuzzyNeedle n -> F.test (cf n) haystack
        RegexNeedle r -> match r haystack
    cf = if sc then id else T.toCaseFold
