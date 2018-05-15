{-# LANGUAGE PatternSynonyms, ViewPatterns, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Pure.Data.View.Patterns
  ( pattern SimpleHTML
  , pattern SimpleSVG
  , Style(..)
  , Property(..)
  , SVGProperty(..)
  , Attribute(..)
  , SVGAttribute(..)
  , XLink(..)
  , pattern LibraryComponent, pattern Component
  , pattern Null
  , pattern Raw
  , pattern Keyed
  , HasClasses(..), pattern Classes, pattern AddClasses
  , HasStyles(..), pattern Styles, pattern AddStyles
  , HasProperties(..), pattern Properties, pattern AddProperties
  , HasSVGProperties(..), pattern SVGProperties, pattern AddSVGProperties
  , HasAttributes(..), pattern Attributes, pattern AddAttributes
  , HasSVGAttributes(..), pattern SVGAttributes, pattern AddSVGAttributes
  , HasXLinks(..), pattern XLinks, pattern AddXLinks
  , HasChildren(..), pattern Children, pattern AddChildren
  , HasKeyedChildren(..), pattern KeyedChildren, pattern AddKeyedChildren
  , (<|), (<||>), (|>)
  , (<|#|>), (|#>)
  ) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.Txt (Txt)

import Control.Arrow ((&&&))

import Control.Monad (void)

import Data.Coerce
import Data.Typeable

import Data.List as List (null)
import Data.Map.Strict as Map (fromList,null,empty,union,toList)
import Data.IntMap.Strict as IntMap (empty)
import Data.Set as Set (Set,fromList,null,empty,union,toList)

import GHC.Exts

import Unsafe.Coerce

newtype Style = Style (Txt,Txt)
coerceFromStyles :: [Style] -> [(Txt,Txt)]
coerceFromStyles = coerce
coerceToStyles :: [(Txt,Txt)] -> [Style]
coerceToStyles = coerce

newtype Property = Property (Txt,Txt)
coerceFromProperties :: [Property] -> [(Txt,Txt)]
coerceFromProperties = coerce
coerceToProperties :: [(Txt,Txt)] -> [Property]
coerceToProperties = coerce

newtype SVGProperty = SVGProperty (Txt,Txt)
coerceFromSVGProperties :: [SVGProperty] -> [(Txt,Txt)]
coerceFromSVGProperties = coerce
coerceToSVGProperties :: [(Txt,Txt)] -> [SVGProperty]
coerceToSVGProperties = coerce

newtype Attribute = Attribute (Txt,Txt)
coerceFromAttributes :: [Attribute] -> [(Txt,Txt)]
coerceFromAttributes = coerce
coerceToAttributes :: [(Txt,Txt)] -> [Attribute]
coerceToAttributes = coerce

newtype SVGAttribute = SVGAttribute (Txt,Txt)
coerceFromSVGAttributes :: [SVGAttribute] -> [(Txt,Txt)]
coerceFromSVGAttributes = coerce
coerceToSVGAttributes :: [(Txt,Txt)] -> [SVGAttribute]
coerceToSVGAttributes = coerce

newtype XLink = XLink (Txt,Txt)
coerceFromXLinks :: [XLink] -> [(Txt,Txt)]
coerceFromXLinks = coerce
coerceToXLinks :: [(Txt,Txt)] -> [XLink]
coerceToXLinks = coerce

pattern EmptyMap <- (Map.null -> True) where
  EmptyMap = Map.empty

pattern EmptySet <- (Set.null -> True) where
  EmptySet = Set.empty

-- OverloadedLists makes this necessary
pattern EmptyList :: [a]
pattern EmptyList <- (List.null -> True) where
  EmptyList = []

-- Component

pattern LibraryComponent :: forall m props state. Typeable props => (Ref m props state -> Comp m props state) -> props -> View
pattern LibraryComponent v p <- ComponentView ((== (show (typeOf (undefined :: props)))) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  LibraryComponent v p = ComponentView (show (typeOf p)) p Nothing v

pattern Component :: forall m props state. Typeable props => (Ref m props state -> Comp m props state) -> props -> View
pattern Component v p <- ComponentView ((==) (tyCon (undefined :: props)) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  Component v p = ComponentView (tyCon p) p Nothing v

-- Null

pattern Null :: View
pattern Null <- (NullView _) where
  Null = NullView Nothing

-- HTML

pattern SimpleHTML :: Txt -> View
pattern SimpleHTML tag = HTMLView Nothing tag (Features EmptySet EmptyMap EmptyMap EmptyMap EmptyList EmptyList) EmptyList

-- SVG

pattern SimpleSVG :: Txt -> View
pattern SimpleSVG tag = SVGView Nothing tag (Features EmptySet EmptyMap EmptyMap EmptyMap EmptyList EmptyList) EmptyMap EmptyList

-- Raw

toRaw :: View -> View
toRaw HTMLView {..} = RawView { content = "", .. }
toRaw SVGView {..} = RawView { content = "", .. }
toRaw KHTMLView {..} = RawView { content = "", .. }
toRaw KSVGView {..} = RawView { content = "", .. }
toRaw v = v

setContent :: Txt -> View -> View
setContent c RawView {..} = RawView { content = c, .. }
setContent _ v = v

-- Raw Div content <| props
pattern Raw :: View -> Txt -> View
pattern Raw v r <- ((id &&& id) -> (RawView _ _ _ r,v)) where
  Raw v r = setContent r (toRaw v)

-- Keyed

isKeyed :: View -> Bool
isKeyed KSVGView{} = True
isKeyed KHTMLView{} = True
isKeyed _ = False

keyed :: View -> View
keyed SVGView {..} = KSVGView { keyedChildren = [], childMap = IntMap.empty, .. }
keyed HTMLView {..} = KHTMLView { keyedChildren = [], childMap = IntMap.empty, .. }
keyed v = v

pattern Keyed :: View -> View
pattern Keyed v <- (isKeyed &&& id -> (True,v)) where
  Keyed v = keyed v

class HasFeatures a where
  getFeatures :: a -> Features
  setFeatures :: Features -> a -> a
  addFeatures :: Features -> a -> a
  {-# INLINE addFeatures #-}
  addFeatures fs a = setFeatures (getFeatures a <> fs) a

instance HasFeatures View where
  {-# INLINE getFeatures #-}
  getFeatures NullView {} = mempty
  getFeatures TextView {} = mempty
  getFeatures ComponentView {} = mempty
  getFeatures SomeView {} = mempty
  getFeatures v = features v
  {-# INLINE setFeatures #-}
  setFeatures _ v@NullView {} = v
  setFeatures _ v@TextView {} = v
  setFeatures _ v@ComponentView {} = v
  setFeatures _ v@SomeView {} = v
  setFeatures fs v = v { features = fs }

pattern Feats :: HasFeatures a => Features -> a -> a
pattern Feats fs a <- ((getFeatures &&& id) -> (fs,a)) where
  Feats fs a = setFeatures fs a

pattern AddFeats :: HasFeatures a => Features -> a -> a
pattern AddFeats fs a <- ((getFeatures &&& id) -> (fs,a)) where
  AddFeats fs a = addFeatures fs a

-- Classes

class HasClasses a where
  getClasses :: a -> [Txt]
  setClasses :: [Txt] -> a -> a
  addClasses :: [Txt] -> a -> a
  {-# INLINE addClasses #-}
  addClasses cs a = setClasses (getClasses a ++ cs) a

instance HasClasses View where
  {-# INLINE getClasses #-}
  getClasses NullView {} = []
  getClasses TextView {} = []
  getClasses ComponentView {} = []
  getClasses SomeView {} = []
  getClasses v = Set.toList (classes (features v))
  {-# INLINE setClasses #-}
  setClasses _ v@NullView {} = v
  setClasses _ v@TextView {} = v
  setClasses _ v@ComponentView {} = v
  setClasses _ v@SomeView {} = v
  setClasses cs v = v { features = (features v) { classes = Set.fromList cs } }
  {-# INLINE addClasses #-}
  addClasses _ v@NullView {} = v
  addClasses _ v@TextView {} = v
  addClasses _ v@ComponentView {} = v
  addClasses _ v@SomeView {} = v
  addClasses cs v = v { features = (features v) { classes = Set.union (Set.fromList cs) (classes (features v)) } }

pattern Classes :: HasClasses a => [Txt] -> a -> a
pattern Classes cs v <- ((getClasses &&& id) -> (cs,v)) where
  Classes cs v = setClasses cs v

pattern AddClasses :: HasClasses a => [Txt] -> a -> a
pattern AddClasses cs v <- ((getClasses &&& id) -> (cs,v)) where
  AddClasses cs v = addClasses cs v

-- Styles

class HasStyles a where
  getStyles :: a -> [Style]
  setStyles :: [Style] -> a -> a
  addStyles :: [Style] -> a -> a
  {-# INLINE addStyles #-}
  addStyles ss a = setStyles (getStyles a ++ ss) a

instance HasStyles View where
  {-# INLINE getStyles #-}
  getStyles NullView {} = []
  getStyles (TextView _ _) = []
  getStyles ComponentView {} = []
  getStyles SomeView {} = []
  getStyles v = coerceToStyles (Map.toList (styles (features v)))
  {-# INLINE setStyles #-}
  setStyles _ v@NullView {} = v
  setStyles _ v@TextView {} = v
  setStyles _ v@ComponentView {} = v
  setStyles _ v@SomeView {} = v
  setStyles ss v = v { features = (features v) { styles = Map.fromList (coerceFromStyles ss) } }
  {-# INLINE addStyles #-}
  addStyles _ v@NullView {} = v
  addStyles _ v@TextView {} = v
  addStyles _ v@ComponentView {} = v
  addStyles _ v@SomeView {} = v
  addStyles cs v = v { features = (features v) { styles = Map.union (Map.fromList (coerceFromStyles cs)) (styles (features v)) } }

pattern Styles :: HasStyles a => [Style] -> a -> a
pattern Styles ss v <- ((getStyles &&& id) -> (ss,v)) where
  Styles ss v = setStyles ss v

pattern AddStyles :: HasStyles a => [Style] -> a -> a
pattern AddStyles ss v <- ((getStyles &&& id) -> (ss,v)) where
  AddStyles ss v = addStyles ss v

-- Listeners

class HasListeners a where
  getListeners :: a -> [Listener]
  setListeners :: [Listener] -> a -> a
  addListeners :: [Listener] -> a -> a
  {-# INLINE addListeners #-}
  addListeners ls a = setListeners (getListeners a ++ ls) a

instance HasListeners View where
  {-# INLINE getListeners #-}
  getListeners NullView {} = []
  getListeners TextView {} = []
  getListeners ComponentView {} = []
  getListeners SomeView {} = []
  getListeners v = listeners (features v)
  {-# INLINE setListeners #-}
  setListeners _ v@NullView {} = v
  setListeners _ v@TextView {} = v
  setListeners _ v@ComponentView {} = v
  setListeners _ v@SomeView {} = v
  setListeners ls v = v { features = (features v) { listeners = ls } }
  {-# INLINE addListeners #-}
  addListeners _ v@NullView {} = v
  addListeners _ v@TextView {} = v
  addListeners _ v@ComponentView {} = v
  addListeners _ v@SomeView {} = v
  addListeners ls v = v { features = (features v) { listeners = ls ++ (listeners (features v)) } }

pattern Listeners :: [Listener] -> View -> View
pattern Listeners ls v <- ((getListeners &&& id) -> (ls,v)) where
  Listeners ls v = setListeners ls v

pattern AddListeners :: [Listener] -> View -> View
pattern AddListeners ls v <- ((getListeners &&& id) -> (ls,v)) where
  AddListeners ls v = addListeners ls v

-- Attributes

class HasAttributes a where
  getAttributes :: a -> [Attribute]
  setAttributes :: [Attribute] -> a -> a
  addAttributes :: [Attribute] -> a -> a
  {-# INLINE addAttributes #-}
  addAttributes as a = setAttributes (getAttributes a ++ as) a

instance HasAttributes View where
  {-# INLINE getAttributes #-}
  getAttributes v@HTMLView {} = coerceToAttributes (Map.toList (attributes (features v)))
  getAttributes v@KHTMLView {} = coerceToAttributes (Map.toList (attributes (features v)))
  getAttributes v@RawView {} = coerceToAttributes (Map.toList (attributes (features v)))
  getAttributes _ = []
  {-# INLINE setAttributes #-}
  setAttributes as v@HTMLView {} = v { features = (features v) { attributes = Map.fromList (coerceFromAttributes as) } }
  setAttributes as v@KHTMLView {} = v { features = (features v) { attributes = Map.fromList (coerceFromAttributes as) } }
  setAttributes as v@RawView {} = v { features = (features v) { attributes = Map.fromList (coerceFromAttributes as) } }
  setAttributes _ v = v
  {-# INLINE addAttributes #-}
  addAttributes as v@HTMLView{} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromAttributes as)) (attributes (features v)) } }
  addAttributes as v@KHTMLView{} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromAttributes as)) (attributes (features v)) } }
  addAttributes as v@RawView{} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromAttributes as)) (attributes (features v)) } }
  addAttributes _ v = v

pattern Attributes :: HasAttributes a => [Attribute] -> a -> a
pattern Attributes as v <- ((getAttributes &&& id) -> (as,v)) where
  Attributes as v = setAttributes as v

pattern AddAttributes :: HasAttributes a => [Attribute] -> a -> a
pattern AddAttributes as v <- ((getAttributes &&& id) -> (as,v)) where
  AddAttributes as v = addAttributes as v

-- SVGAttributes

class HasSVGAttributes a where
  getSVGAttributes :: a -> [SVGAttribute]
  setSVGAttributes :: [SVGAttribute] -> a -> a
  addSVGAttributes :: [SVGAttribute] -> a -> a
  {-# INLINE addSVGAttributes #-}
  addSVGAttributes as a = setSVGAttributes (getSVGAttributes a ++ as) a

instance HasSVGAttributes View where
  {-# INLINE getSVGAttributes #-}
  getSVGAttributes v@SVGView {} = coerceToSVGAttributes (Map.toList (attributes (features v)))
  getSVGAttributes v@KSVGView {} = coerceToSVGAttributes (Map.toList (attributes (features v)))
  getSVGAttributes _ = []
  {-# INLINE setSVGAttributes #-}
  setSVGAttributes as v@SVGView {} = v { features = (features v) { attributes = Map.fromList (coerceFromSVGAttributes as) } }
  setSVGAttributes as v@KSVGView {} = v { features = (features v) { attributes = Map.fromList (coerceFromSVGAttributes as) } }
  setSVGAttributes _ v = v
  {-# INLINE addSVGAttributes #-}
  addSVGAttributes as v@SVGView {} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromSVGAttributes as)) (attributes (features v)) } }
  addSVGAttributes as v@KSVGView {} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromSVGAttributes as)) (attributes (features v)) } }
  addSVGAttributes _ v = v

pattern SVGAttributes :: HasSVGAttributes a => [SVGAttribute] -> a -> a
pattern SVGAttributes as v <- ((getSVGAttributes &&& id) -> (as,v)) where
  SVGAttributes as v = setSVGAttributes as v

pattern AddSVGAttributes :: HasSVGAttributes a => [SVGAttribute] -> a -> a
pattern AddSVGAttributes as v <- ((getSVGAttributes &&& id) -> (as,v)) where
  AddSVGAttributes as v = addSVGAttributes as v

-- Properties

class HasProperties a where
  getProperties :: a -> [Property]
  setProperties :: [Property] -> a -> a
  addProperties :: [Property] -> a -> a
  {-# INLINE addProperties #-}
  addProperties ps a = setProperties (getProperties a ++ ps) a

instance HasProperties View where
  {-# INLINE getProperties #-}
  getProperties v@HTMLView{} = coerceToProperties (Map.toList (properties (features v)))
  getProperties v@KHTMLView{} = coerceToProperties (Map.toList (properties (features v)))
  getProperties v@RawView{} = coerceToProperties (Map.toList (properties (features v)))
  getProperties _ = []
  {-# INLINE setProperties #-}
  setProperties ps v@HTMLView{} = v { features = (features v) { properties = Map.fromList (coerceFromProperties ps) } }
  setProperties ps v@KHTMLView{} = v { features = (features v) { properties = Map.fromList (coerceFromProperties ps) } }
  setProperties ps v@RawView{} = v { features = (features v) { properties = Map.fromList (coerceFromProperties ps) } }
  setProperties _ v = v
  {-# INLINE addProperties #-}
  addProperties ps v@HTMLView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromProperties ps)) (properties (features v)) } }
  addProperties ps v@KHTMLView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromProperties ps)) (properties (features v)) } }
  addProperties ps v@RawView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromProperties ps)) (properties (features v)) } }
  addProperties _ v = v

pattern Properties :: HasProperties a => [Property] -> a -> a
pattern Properties ps v <- ((getProperties &&& id) -> (ps,v)) where
  Properties ps v = setProperties ps v

pattern AddProperties :: HasProperties a => [Property] -> a -> a
pattern AddProperties as v <- ((getProperties &&& id) -> (as,v)) where
  AddProperties as v = addProperties as v

-- SVG Properties

class HasSVGProperties a where
  getSVGProperties :: a -> [SVGProperty]
  setSVGProperties :: [SVGProperty] -> a -> a
  addSVGProperties :: [SVGProperty] -> a -> a
  {-# INLINE addSVGProperties #-}
  addSVGProperties ps a = setSVGProperties (getSVGProperties a ++ ps) a

instance HasSVGProperties View where
  {-# INLINE getSVGProperties #-}
  getSVGProperties v@SVGView{} = coerceToSVGProperties (Map.toList (properties (features v)))
  getSVGProperties v@KSVGView{} = coerceToSVGProperties (Map.toList (properties (features v)))
  getSVGProperties _ = []
  {-# INLINE setSVGProperties #-}
  setSVGProperties ps v@SVGView{} = v { features = (features v) { properties = Map.fromList (coerceFromSVGProperties ps) } }
  setSVGProperties ps v@KSVGView{} = v { features = (features v) { properties = Map.fromList (coerceFromSVGProperties ps) } }
  setSVGProperties _ v = v
  {-# INLINE addSVGProperties #-}
  addSVGProperties ps v@SVGView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromSVGProperties ps)) (properties (features v)) } }
  addSVGProperties ps v@KSVGView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromSVGProperties ps)) (properties (features v)) } }
  addSVGProperties _ v = v

pattern SVGProperties :: HasSVGProperties a => [SVGProperty] -> a -> a
pattern SVGProperties ps v <- ((getSVGProperties &&& id) -> (ps,v)) where
  SVGProperties ps v = setSVGProperties ps v

pattern AddSVGProperties :: HasSVGProperties a => [SVGProperty] -> a -> a
pattern AddSVGProperties as v <- ((getSVGProperties &&& id) -> (as,v)) where
  AddSVGProperties as v = addSVGProperties as v

-- Lifecycles

class HasLifecycles a where
  getLifecycles :: a -> [Lifecycle]
  setLifecycles :: [Lifecycle] -> a -> a
  addLifecycles ::  [Lifecycle] -> a -> a
  {-# INLINE addLifecycles #-}
  addLifecycles ls a = setLifecycles (getLifecycles a ++ ls) a

instance HasLifecycles View where
  {-# INLINE getLifecycles #-}
  getLifecycles NullView {} = []
  getLifecycles TextView {} = []
  getLifecycles ComponentView {} = []
  getLifecycles SomeView {} = []
  getLifecycles v = lifecycles (features v)
  {-# INLINE setLifecycles #-}
  setLifecycles _ v@NullView {} = v
  setLifecycles _ v@TextView {} = v
  setLifecycles _ v@ComponentView {} = v
  setLifecycles _ v@SomeView {} = v
  setLifecycles lc v = v { features = (features v) { lifecycles = lc } }
  {-# INLINE addLifecycles #-}
  addLifecycles _ v@NullView {} = v
  addLifecycles _ v@TextView {} = v
  addLifecycles _ v@ComponentView {} = v
  addLifecycles _ v@SomeView {} = v
  addLifecycles as v = v { features = (features v) { lifecycles = as ++ (lifecycles (features v)) } }

pattern Lifecycles :: HasLifecycles a => [Lifecycle] -> a -> a
pattern Lifecycles lc v <- ((getLifecycles &&& id) -> (lc,v)) where
  Lifecycles lc v = setLifecycles lc v

pattern AddLifecycles :: HasLifecycles a => [Lifecycle] -> a -> a
pattern AddLifecycles lc v <- ((getLifecycles &&& id) -> (lc,v)) where
  AddLifecycles lc v = addLifecycles lc v

-- XLinks

class HasXLinks a where
  getXLinks :: a -> [XLink]
  setXLinks :: [XLink] -> a -> a
  addXLinks :: [XLink] -> a -> a
  {-# INLINE addXLinks #-}
  addXLinks xl a = setXLinks (getXLinks a ++ xl) a

instance HasXLinks View where
  {-# INLINE getXLinks #-}
  getXLinks SVGView {..} = coerceToXLinks (Map.toList xlinks)
  getXLinks KSVGView {..} = coerceToXLinks (Map.toList xlinks)
  getXLinks _ = []
  {-# INLINE setXLinks #-}
  setXLinks xl khtml@SVGView {} = khtml { xlinks = Map.fromList (coerceFromXLinks xl) }
  setXLinks xl ksvg@KSVGView {} = ksvg { xlinks = Map.fromList (coerceFromXLinks xl) }
  setXLinks _ v = v
  {-# INLINE addXLinks #-}
  addXLinks xl v@SVGView {} = v { xlinks = Map.union (Map.fromList (coerceFromXLinks xl)) (xlinks v) }
  addXLinks xl v@KSVGView {} = v { xlinks = Map.union (Map.fromList (coerceFromXLinks xl)) (xlinks v) }
  addXLinks _ v = v

pattern XLinks :: HasXLinks a => [XLink] -> a -> a
pattern XLinks xl v <- ((getXLinks &&& id) -> (xl,v)) where
  XLinks xl v = setXLinks xl v

pattern AddXLinks :: HasXLinks a => [XLink] -> a -> a
pattern AddXLinks xl v <- ((getXLinks &&& id) -> (xl,v)) where
  AddXLinks xl v = addXLinks xl v

-- Combinators

class HasChildren a where
  getChildren :: a -> [View]
  setChildren :: [View] -> a -> a
  addChildren :: [View] -> a -> a
  {-# INLINE addChildren #-}
  addChildren cs a = setChildren (getChildren a ++ cs) a

instance HasChildren View where
  {-# INLINE getChildren #-}
  getChildren v@HTMLView {} = children v
  getChildren v@SVGView {} = children v
  getChildren _  = []
  {-# INLINE setChildren #-}
  setChildren cs v@HTMLView {} = v { children = cs }
  setChildren cs v@SVGView {} = v { children = cs }
  setChildren _ v = v
  {-# INLINE addChildren #-}
  addChildren cs v@HTMLView {} = v { children = children v ++ cs }
  addChildren cs v@SVGView {} = v { children = children v ++ cs }
  addChildren _ v = v

pattern Children :: HasChildren a => [View] -> a -> a
pattern Children cs v <- ((getChildren &&& id) -> (cs,v)) where
  Children cs v = setChildren cs v

pattern AddChildren :: HasChildren a => [View] -> a -> a
pattern AddChildren cs v <- ((getChildren &&& id) -> (cs,v)) where
  AddChildren cs v = addChildren cs v

-- Keyed Children

class HasKeyedChildren a where
  getKeyedChildren :: a -> [(Int,View)]
  setKeyedChildren :: [(Int,View)] -> a -> a
  addKeyedChildren :: [(Int,View)] -> a -> a
  {-# INLINE addKeyedChildren #-}
  addKeyedChildren cs a = setKeyedChildren (getKeyedChildren a ++ cs) a

instance HasKeyedChildren View where
  {-# INLINE getKeyedChildren #-}
  getKeyedChildren v@KHTMLView {} = keyedChildren v
  getKeyedChildren v@SVGView {} = keyedChildren v
  getKeyedChildren _ = []
  {-# INLINE setKeyedChildren #-}
  setKeyedChildren cs v@KHTMLView {} = v { keyedChildren = cs }
  setKeyedChildren cs v@KSVGView {} = v { keyedChildren = cs }
  setKeyedChildren _ v = v
  {-# INLINE addKeyedChildren #-}
  addKeyedChildren cs v@KHTMLView {} = v { keyedChildren = keyedChildren v ++ cs }
  addKeyedChildren cs v@KSVGView {} = v { keyedChildren = keyedChildren v ++ cs }
  addKeyedChildren _ v = v

pattern KeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
pattern KeyedChildren ks v <- ((getKeyedChildren &&& id) -> (ks,v)) where
  KeyedChildren ks v = setKeyedChildren ks v

pattern AddKeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
pattern AddKeyedChildren ks v <- ((getKeyedChildren &&& id) -> (ks,v)) where
  AddKeyedChildren ks v = addKeyedChildren ks v

infixr 0 <|
{-# INLINE (<|) #-}
(<|) :: ToView b => a -> (a -> b) -> View
(<|) a f = toView (f a)

{-# INLINE (<||>) #-}
(<||>) :: HasChildren a => a -> [View] -> a
(<||>) v cs = Children cs v

{-# INLINE (<|#|>) #-}
(<|#|>) :: HasKeyedChildren a => a -> [(Int,View)] -> a
(<|#|>) v cs = KeyedChildren cs v

infixl 1 |>
{-# INLINE (|>) #-}
(|>) :: HasChildren a => (a -> c) -> [View] -> a -> c
(|>) f cs = f . setChildren cs

infixl 1 |#>
{-# INLINE (|#>) #-}
(|#>) :: HasKeyedChildren a => (a -> c) -> [(Int,View)] -> a -> c
(|#>) f cs = f . setKeyedChildren cs

