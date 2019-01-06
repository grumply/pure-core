{-# LANGUAGE PatternSynonyms, ViewPatterns, ScopedTypeVariables, RecordWildCards, OverloadedStrings, BangPatterns, TemplateHaskell #-}
module Pure.Data.View.Patterns
  ( pattern SimpleHTML
  , pattern SimpleSVG
  , pattern Portal
  , pattern LibraryComponent, pattern Component
  , pattern LibraryComponentIO, pattern ComponentIO
  , pattern Null
  , pattern Raw
  , pattern Keyed
  , HasFeatures(..), pattern Features, pattern SetFeatures
  , pattern Class, pattern Classes, pattern SetClasses
  , pattern Style, pattern Styles, pattern SetStyles
  , pattern Property, pattern Properties, pattern SetProperties
  , pattern Attribute, pattern Attributes, pattern SetAttributes
  , pattern Lifecycle, pattern Lifecycles, pattern SetLifecycles
  , pattern Listener, pattern Listeners, pattern SetListeners
  , HasXLinks(..), pattern XLink, pattern XLinks, pattern SetXLinks
  , HasChildren(..), pattern Children, pattern SetChildren
  , HasKeyedChildren(..), pattern KeyedChildren, pattern SetKeyedChildren
  , (<|), (<||>), (|>)
  , (<||#>), (|#>)
  , lazy, lazy2, lazy3
  , text, txt, string
  ) where

-- This module exposes some hacky patterns due to GHCs lack of unidirectional expression patterns.
-- They exist to create a consistent and readable syntax for view construction without overlapping
-- with Haskell's reserved words. It is best to avoid using these patterns for inspection/pattern-
-- matching.

-- from pure-default
import Pure.Data.Default (Default(..))

-- from pure-core (local)
import Pure.Data.View

-- from pure-lifted
import Pure.Data.Lifted (Element)

-- from pure-txt
import Pure.Data.Txt (Txt,ToTxt(..))

-- from base
import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Monoid ((<>))
import Data.Typeable (Typeable,TypeRep(),typeOf,typeRepFingerprint)
import Data.List as List (null)
import Data.Map.Lazy as Map (fromList,null,empty,union,toList,insert)
import Data.Set as Set (empty,fromList,null,empty,union,toList,insert)
import Unsafe.Coerce (unsafeCoerce)

pattern EmptyMap <- (Map.null -> True) where
  EmptyMap = Map.empty

pattern EmptySet <- (Set.null -> True) where
  EmptySet = Set.empty

-- OverloadedLists makes this necessary
pattern EmptyList :: [a]
pattern EmptyList <- (List.null -> True) where
  EmptyList = []

-- Lazy

lazy :: Pure p => (a -> p) -> a -> View
lazy = LazyView

lazy2 :: Pure p => (a -> b -> p) -> a -> b -> View
lazy2 f a b = lazy (\a -> lazy (\b -> f a b) b) a

lazy3 :: Pure p => (a -> b -> c -> p) -> a -> b -> c -> View
lazy3 f a b c = lazy (\a -> lazy (\b -> lazy (\c -> f a b c) c) b) a

-- text

text :: (ToTxt a) => a -> View
text = lazy (TextView Nothing . toTxt)

txt :: Txt -> View
txt = text

string :: String -> View
string = text

-- Component

pattern LibraryComponent :: forall m props state. (Typeable m, Typeable props, Typeable state) => (Ref m props state -> Comp m props state) -> props -> View
pattern LibraryComponent v p <- ComponentView (sameTypeWitness $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|]) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  LibraryComponent v p = ComponentView $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|])p Nothing v

pattern Component :: forall m props state. (Typeable m, Typeable props, Typeable state) => (Ref m props state -> Comp m props state) -> props -> View
pattern Component v p <- ComponentView (sameTypeWitness $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|]) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  Component v p = ComponentView $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|])p Nothing v

pattern LibraryComponentIO :: forall props state. (Typeable props, Typeable state) => (Ref IO props state -> Comp IO props state) -> props -> View
pattern LibraryComponentIO v p <- ComponentView (sameTypeWitness $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|]) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  LibraryComponentIO v p = ComponentView $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|])p Nothing (\ref -> (v ref) { performIO = id, execute = id })

pattern ComponentIO :: forall props state. (Typeable props, Typeable state) => (Ref IO props state -> Comp IO props state) -> props -> View
pattern ComponentIO v p <- ComponentView (sameTypeWitness $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|]) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  ComponentIO v p = ComponentView $([|TypeWitness $! typeRepFingerprint $! typeOf (undefined :: IO (props,state))|]) p Nothing (\ref -> (v ref) { performIO = id, execute = id })

-- Null

pattern Null :: View
pattern Null <- (NullView _) where
  Null = NullView Nothing

-- HTML

pattern SimpleHTML :: Txt -> View
pattern SimpleHTML tag = HTMLView Nothing tag (Features_ EmptySet EmptyMap EmptyMap EmptyMap EmptyList EmptyList) EmptyList

-- SVG

pattern SimpleSVG :: Txt -> View
pattern SimpleSVG tag = SVGView Nothing tag (Features_ EmptySet EmptyMap EmptyMap EmptyMap EmptyList EmptyList) EmptyMap EmptyList

-- Raw

toRaw :: View -> View
toRaw HTMLView {..} = RawView { content = "", .. }
toRaw SVGView {..} = RawView { content = "", .. }
toRaw KHTMLView {..} = RawView { content = "", .. }
toRaw KSVGView {..} = RawView { content = "", .. }
toRaw PortalView {..} = PortalView { portalView = toRaw portalView, .. }
toRaw v = v

setContent :: Txt -> View -> View
setContent c RawView {..} = RawView { content = c, .. }
setContent _ v = v

-- Raw Div content <| props
pattern Raw :: View -> Txt -> View
pattern Raw v r <- ((id &&& id) -> (RawView _ _ _ r,v)) where
  Raw v r = setContent r (toRaw v)

-- Portal

pattern Portal :: Element -> View -> View
pattern Portal host v = PortalView Nothing host v

-- -- Keyed

isKeyed :: View -> Bool
isKeyed KSVGView{} = True
isKeyed KHTMLView{} = True
isKeyed PortalView{..} = isKeyed portalView
isKeyed _ = False

keyed :: View -> View
keyed SVGView {..} = KSVGView { keyedChildren = [], .. }
keyed HTMLView {..} = KHTMLView { keyedChildren = [], .. }
keyed PortalView {..} = PortalView { portalView = keyed portalView, .. }
keyed v = v

pattern Keyed :: View -> View
pattern Keyed v <- (isKeyed &&& id -> (True,v)) where
  Keyed v = keyed v

-- Features

class HasFeatures a where
  getFeatures :: a -> Features
  setFeatures :: Features -> a -> a
  addFeatures :: Features -> a -> a
  addFeatures fs a = setFeatures (getFeatures a <> fs) a

instance HasFeatures View where
  getFeatures NullView {} = mempty
  getFeatures TextView {} = mempty
  getFeatures ComponentView {} = mempty
  getFeatures SomeView {} = mempty
  getFeatures PortalView{..} = getFeatures portalView
  getFeatures v = features v
  setFeatures _ v@NullView {} = v
  setFeatures _ v@TextView {} = v
  setFeatures _ v@ComponentView {} = v
  setFeatures _ v@SomeView {} = v
  setFeatures fs v@PortalView{..} = PortalView { portalView = setFeatures fs portalView, .. }
  setFeatures fs v = v { features = fs }

instance HasFeatures Features where
  getFeatures = id
  setFeatures = const
  addFeatures = (<>)

pattern SetFeatures :: HasFeatures a => Features -> a -> a
pattern SetFeatures fs a <- ((getFeatures &&& id) -> (fs,a)) where
  SetFeatures fs a = setFeatures fs a

pattern Features :: HasFeatures a => Features -> a -> a
pattern Features fs a <- ((getFeatures &&& id) -> (fs,a)) where
  Features fs a = addFeatures fs a

-- Classes

pattern Class :: HasFeatures a => Txt -> a -> a
pattern Class c a <- ((const "" &&& id) -> (c,a)) where
  Class c a =
    let fs = getFeatures a
        cs = Set.insert c (classes fs)
    in setFeatures fs { classes = cs } a

pattern SetClasses :: HasFeatures a => [Txt] -> a -> a
pattern SetClasses cs a <- (((Set.toList . classes . getFeatures) &&& id) -> (cs,a)) where
  SetClasses cs a =
    let cs' = foldr Set.insert Set.empty cs
        fs  = getFeatures a
    in setFeatures fs { classes = cs' } a

pattern Classes :: HasFeatures a => [Txt] -> a -> a
pattern Classes cs a <- SetClasses cs a where
  Classes cs a =
    let fs = getFeatures a
    in setFeatures (fs { classes = Set.union (Set.fromList cs) (classes fs) }) a

-- Styles

pattern Style :: HasFeatures a => Txt -> Txt -> a -> a
pattern Style k v a <- ((const ("","") &&& id) -> ((k,v),a)) where
  Style k v a =
    let fs = getFeatures a
    in setFeatures (fs { styles = Map.insert k v (styles fs) }) a

pattern SetStyles :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern SetStyles ss v <- (((Map.toList . styles . getFeatures) &&& id) -> (ss,v)) where
  SetStyles ss v = setFeatures ((getFeatures v) { styles = Map.fromList ss }) v

pattern Styles :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern Styles ss v <- SetStyles ss v where
  Styles ss v =
    let fs = getFeatures v
    in setFeatures (fs { styles = Map.union (Map.fromList ss) (styles fs) }) v

-- Listeners

pattern Listener :: HasFeatures a => Listener -> a -> a
pattern Listener l a <- ((const (On "" ElementTarget def (\_ -> return ()) (return ())) &&& id) -> (l,a)) where
  Listener l a =
    let fs = getFeatures a
    in setFeatures (fs { listeners = l : listeners fs }) a

pattern SetListeners :: HasFeatures a => [Listener] -> a -> a
pattern SetListeners ls v <- (((listeners . getFeatures) &&& id) -> (ls,v)) where
  SetListeners ls v = setFeatures ((getFeatures v) { listeners = ls }) v

pattern Listeners :: HasFeatures a => [Listener] -> a -> a
pattern Listeners ls v <- SetListeners ls v where
  Listeners ls v =
    let fs = getFeatures v
    in setFeatures (fs { listeners = ls ++ listeners fs }) v

-- Attributes

pattern Attribute :: HasFeatures a => Txt -> Txt -> a -> a
pattern Attribute k v a <- ((const ("","") &&& id) -> ((k,v),a)) where
  Attribute k v a =
    let fs = getFeatures a
    in setFeatures (fs { attributes = Map.insert k v (attributes fs) }) a

pattern SetAttributes :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern SetAttributes as v <- (((Map.toList . attributes . getFeatures) &&& id) -> (as,v)) where
  SetAttributes as v = setFeatures ((getFeatures v) { attributes = Map.fromList as }) v

pattern Attributes :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern Attributes as v <- SetAttributes as v where
  Attributes as v =
    let fs = getFeatures v
    in setFeatures (fs { attributes = Map.union (Map.fromList as) (attributes fs) }) v

-- Properties

pattern Property :: HasFeatures a => Txt -> Txt -> a -> a
pattern Property k v a <- ((const ("","") &&& id) -> ((k,v),a)) where
  Property k v a =
    let fs = getFeatures a
        ps = properties fs
        ps' = Map.insert k v ps
    in setFeatures fs { properties = ps' } a

pattern SetProperties :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern SetProperties ps v <- (((Map.toList . properties . getFeatures) &&& id) -> (ps,v)) where
  SetProperties ps v =
    let fs = getFeatures v
        ps' = foldr (\(k,v) -> Map.insert k v) Map.empty ps
    in setFeatures fs { properties = ps' } v

pattern Properties :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern Properties ps v <- SetProperties ps v where
  Properties ps v =
    let fs = getFeatures v
    in setFeatures (fs { properties = Map.union (Map.fromList ps) (properties fs) }) v

-- Lifecycles

pattern Lifecycle :: HasFeatures a => Lifecycle -> a -> a
pattern Lifecycle l a <- ((const (HostRef (\_ -> return ())) &&& id) -> (l,a)) where
  Lifecycle l a =
    let fs = getFeatures a
    in setFeatures (fs { lifecycles = l : lifecycles fs }) a

pattern SetLifecycles :: HasFeatures a => [Lifecycle] -> a -> a
pattern SetLifecycles lc v <- (((lifecycles . getFeatures) &&& id) -> (lc,v)) where
  SetLifecycles lc v = setFeatures ((getFeatures v) { lifecycles = lc }) v

pattern Lifecycles :: HasFeatures a => [Lifecycle] -> a -> a
pattern Lifecycles lc v <- (((lifecycles . getFeatures) &&& id) -> (lc,v)) where
  Lifecycles lc v =
    let fs = getFeatures v
    in setFeatures (fs { lifecycles = lc ++ lifecycles fs }) v

-- XLinks

class HasXLinks a where
  getXLinks :: a -> [(Txt,Txt)]
  setXLinks :: [(Txt,Txt)] -> a -> a
  addXLinks :: [(Txt,Txt)] -> a -> a
  addXLinks xl a = setXLinks (getXLinks a ++ xl) a

instance HasXLinks View where
  getXLinks SVGView {..} = Map.toList xlinks
  getXLinks KSVGView {..} = Map.toList xlinks
  getXLinks PortalView {..} = getXLinks portalView
  getXLinks _ = []
  setXLinks xl khtml@SVGView {} = khtml { xlinks = Map.fromList xl }
  setXLinks xl ksvg@KSVGView {} = ksvg { xlinks = Map.fromList xl }
  setXLinks xl PortalView {..}  = PortalView { portalView = setXLinks xl portalView, .. }
  setXLinks _ v = v
  addXLinks xl v@SVGView {} = v { xlinks = Map.union (Map.fromList xl) (xlinks v) }
  addXLinks xl v@KSVGView {} = v { xlinks = Map.union (Map.fromList xl) (xlinks v) }
  addXLinks xl PortalView {..} = PortalView { portalView = addXLinks xl portalView, .. }
  addXLinks _ v = v

pattern XLink :: HasXLinks a => Txt -> Txt -> a -> a
pattern XLink k v a <- ((const ("","") &&& id) -> ((k,v),a)) where
  XLink k v a =
    let xls = getXLinks a
    in setXLinks ((k,v):xls) a

pattern SetXLinks :: HasXLinks a => [(Txt,Txt)] -> a -> a
pattern SetXLinks xl v <- ((getXLinks &&& id) -> (xl,v)) where
  SetXLinks xl v = setXLinks xl v

pattern XLinks :: HasXLinks a => [(Txt,Txt)] -> a -> a
pattern XLinks xl v <- ((getXLinks &&& id) -> (xl,v)) where
  XLinks xl v = addXLinks xl v

-- Combinators

class HasChildren a where
  getChildren :: a -> [View]
  setChildren :: [View] -> a -> a
  addChildren :: [View] -> a -> a
  addChildren cs a = setChildren (getChildren a ++ cs) a

instance HasChildren View where
  getChildren v@HTMLView {} = children v
  getChildren v@SVGView {} = children v
  getChildren PortalView {..} = getChildren portalView
  getChildren _  = []
  setChildren cs v@HTMLView {} = v { children = cs }
  setChildren cs v@SVGView {} = v { children = cs }
  setChildren cs PortalView {..} = PortalView { portalView = setChildren cs portalView, .. }
  setChildren _ v = v
  addChildren cs v@HTMLView {} = v { children = children v ++ cs }
  addChildren cs v@SVGView {} = v { children = children v ++ cs }
  addChildren cs PortalView {..} = PortalView { portalView = setChildren cs portalView, .. }
  addChildren _ v = v

pattern SetChildren :: HasChildren a => [View] -> a -> a
pattern SetChildren cs v <- ((getChildren &&& id) -> (cs,v)) where
  SetChildren cs v = setChildren cs v

pattern Children :: HasChildren a => [View] -> a -> a
pattern Children cs v <- ((getChildren &&& id) -> (cs,v)) where
  Children cs v = addChildren cs v

-- Keyed Children

class HasKeyedChildren a where
  getKeyedChildren :: a -> [(Int,View)]
  setKeyedChildren :: [(Int,View)] -> a -> a
  addKeyedChildren :: [(Int,View)] -> a -> a
  addKeyedChildren cs a = setKeyedChildren (getKeyedChildren a ++ cs) a

instance HasKeyedChildren View where
  getKeyedChildren v@KHTMLView {} = keyedChildren v
  getKeyedChildren v@SVGView {} = keyedChildren v
  getKeyedChildren PortalView {..} = getKeyedChildren portalView
  getKeyedChildren _ = []
  setKeyedChildren cs v@KHTMLView {} = v { keyedChildren = cs }
  setKeyedChildren cs v@KSVGView {} = v { keyedChildren = cs }
  setKeyedChildren cs PortalView {..} = PortalView { portalView = setKeyedChildren cs portalView, .. }
  setKeyedChildren _ v = v
  addKeyedChildren cs v@KHTMLView {} = v { keyedChildren = keyedChildren v ++ cs }
  addKeyedChildren cs v@KSVGView {} = v { keyedChildren = keyedChildren v ++ cs }
  addKeyedChildren cs PortalView {..} = PortalView { portalView = setKeyedChildren cs portalView, .. }
  addKeyedChildren _ v = v

pattern SetKeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
pattern SetKeyedChildren ks v <- ((getKeyedChildren &&& id) -> (ks,v)) where
  SetKeyedChildren ks v = setKeyedChildren ks v

pattern KeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
pattern KeyedChildren ks v <- ((getKeyedChildren &&& id) -> (ks,v)) where
  KeyedChildren ks v = addKeyedChildren ks v

infixl 8 <|
{-# INLINE (<|) #-}
(<|) :: ToView b => a -> (a -> b) -> View
(<|) a f = toView (f a)

{-# INLINE (<||>) #-}
(<||>) :: (ToView a, HasChildren a) => a -> [View] -> View
(<||>) v cs = toView (setChildren cs v)

{-# INLINE (<||#>) #-}
(<||#>) :: (ToView a, HasKeyedChildren a) => a -> [(Int,View)] -> View
(<||#>) v cs = toView (setKeyedChildren cs v)

{-# INLINE (|>) #-}
infixr 9 |>
(|>) :: HasChildren a => (a -> a) -> [View] -> a -> a
(|>) f cs = f . setChildren cs

{-# INLINE (|#>) #-}
infixr 9 |#>
(|#>) :: HasKeyedChildren a => (a -> a) -> [(Int,View)] -> a -> a
(|#>) f cs = f . setKeyedChildren cs

