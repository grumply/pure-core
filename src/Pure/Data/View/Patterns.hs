{-# LANGUAGE PatternSynonyms, ViewPatterns, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Pure.Data.View.Patterns where

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

-- Children

{-# INLINE getChildren #-}
getChildren v@HTMLView {} = Just (children v)
getChildren v@SVGView {} = Just (children v)
getChildren _  = Nothing

{-# INLINE setChildren #-}
setChildren v@HTMLView {} cs = v { children = cs }
setChildren v@SVGView {} cs = v { children = cs }
setChildren v _ = v

pattern Children :: [View] -> View -> View
pattern Children cs v <- ((getChildren &&& id) -> (Just cs,v)) where
  Children cs v = setChildren v cs

-- Keyed Children

{-# INLINE setKeyedChildren #-}
setKeyedChildren v@KHTMLView {} cs = v { keyedChildren = cs }
setKeyedChildren v@KSVGView {} cs = v { keyedChildren = cs }
setKeyedChildren v _ = v

{-# INLINE getKeyedChildren #-}
getKeyedChildren v@KHTMLView {} = Just (keyedChildren v)
getKeyedChildren v@SVGView {} = Just (keyedChildren v)
getKeyedChildren _ = Nothing

pattern KeyedChildren :: [(Int,View)] -> View -> View
pattern KeyedChildren ks v <- ((getKeyedChildren &&& id) -> (Just ks,v)) where
  KeyedChildren ks v = setKeyedChildren v ks

-- Classes

{-# INLINE getClasses #-}
getClasses NullView {} = Nothing
getClasses TextView {} = Nothing
getClasses ComponentView {} = Nothing
getClasses SomeView {} = Nothing
getClasses v = Just (Set.toList (classes (features v)))

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

pattern Classes :: [Txt] -> View -> View
pattern Classes cs v <- ((getClasses &&& id) -> (Just cs,v)) where
  Classes cs v = setClasses cs v

pattern AddClasses :: [Txt] -> View -> View
pattern AddClasses cs v <- ((getClasses &&& id) -> (Just cs,v)) where
  AddClasses cs v = addClasses cs v

-- Styles

{-# INLINE getStyles #-}
getStyles NullView {} = Nothing
getStyles (TextView _ _) = Nothing
getStyles ComponentView {} = Nothing
getStyles SomeView {} = Nothing
getStyles v = Just (coerceToStyles (Map.toList (styles (features v))))

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

pattern Styles :: [Style] -> View -> View
pattern Styles ss v <- ((getStyles &&& id) -> (Just ss,v)) where
  Styles ss v = setStyles ss v

pattern AddStyles :: [Style] -> View -> View
pattern AddStyles ss v <- ((getStyles &&& id) -> (Just ss,v)) where
  AddStyles ss v = addStyles ss v

-- Listeners

{-# INLINE getListeners #-}
getListeners NullView {} = Nothing
getListeners TextView {} = Nothing
getListeners ComponentView {} = Nothing
getListeners SomeView {} = Nothing
getListeners v = Just (listeners (features v))

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
pattern Listeners ls v <- ((getListeners &&& id) -> (Just ls,v)) where
  Listeners ls v = setListeners ls v

pattern AddListeners :: [Listener] -> View -> View
pattern AddListeners ls v <- ((getListeners &&& id) -> (Just ls,v)) where
  AddListeners ls v = addListeners ls v

-- Attributes

{-# INLINE getAttributes #-}
getAttributes v@HTMLView {} = Just (coerceToAttributes (Map.toList (attributes (features v))))
getAttributes v@KHTMLView {} = Just (coerceToAttributes (Map.toList (attributes (features v))))
getAttributes v@RawView {} = Just (coerceToAttributes (Map.toList (attributes (features v))))
getAttributes _ = Nothing

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

pattern Attributes :: [Attribute] -> View -> View
pattern Attributes as v <- ((getAttributes &&& id) -> (Just as,v)) where
  Attributes as v = setAttributes as v

pattern AddAttributes :: [Attribute] -> View -> View
pattern AddAttributes as v <- ((getAttributes &&& id) -> (Just as,v)) where
  AddAttributes as v = addAttributes as v

-- SVGAttributes

{-# INLINE getSVGAttributes #-}
getSVGAttributes v@SVGView {} = Just (coerceToSVGAttributes (Map.toList (attributes (features v))))
getSVGAttributes v@KSVGView {} = Just (coerceToSVGAttributes (Map.toList (attributes (features v))))
getSVGAttributes _ = Nothing

{-# INLINE setSVGAttributes #-}
setSVGAttributes as v@SVGView {} = v { features = (features v) { attributes = Map.fromList (coerceFromSVGAttributes as) } }
setSVGAttributes as v@KSVGView {} = v { features = (features v) { attributes = Map.fromList (coerceFromSVGAttributes as) } }
setSVGAttributes _ v = v

{-# INLINE addSVGAttributes #-}
addSVGAttributes as v@SVGView {} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromSVGAttributes as)) (attributes (features v)) } }
addSVGAttributes as v@KSVGView {} = v { features = (features v) { attributes = Map.union (Map.fromList (coerceFromSVGAttributes as)) (attributes (features v)) } }
addSVGAttributes _ v = v

pattern SVGAttributes :: [SVGAttribute] -> View -> View
pattern SVGAttributes as v <- ((getSVGAttributes &&& id) -> (Just as,v)) where
  SVGAttributes as v = setSVGAttributes as v

pattern AddSVGAttributes :: [SVGAttribute] -> View -> View
pattern AddSVGAttributes as v <- ((getSVGAttributes &&& id) -> (Just as,v)) where
  AddSVGAttributes as v = addSVGAttributes as v

-- Properties

{-# INLINE getProperties #-}
getProperties v@HTMLView{} = Just (coerceToProperties (Map.toList (properties (features v))))
getProperties v@KHTMLView{} = Just (coerceToProperties (Map.toList (properties (features v))))
getProperties v@RawView{} = Just (coerceToProperties (Map.toList (properties (features v))))
getProperties _ = Nothing

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

pattern Properties :: [Property] -> View -> View
pattern Properties ps v <- ((getProperties &&& id) -> (Just ps,v)) where
  Properties ps v = setProperties ps v

pattern AddProperties :: [Property] -> View -> View
pattern AddProperties as v <- ((getProperties &&& id) -> (Just as,v)) where
  AddProperties as v = addProperties as v

-- SVG Properties

{-# INLINE getSVGProperties #-}
getSVGProperties v@SVGView{} = Just (coerceToSVGProperties (Map.toList (properties (features v))))
getSVGProperties v@KSVGView{} = Just (coerceToSVGProperties (Map.toList (properties (features v))))
getSVGProperties _ = Nothing

{-# INLINE setSVGProperties #-}
setSVGProperties ps v@SVGView{} = v { features = (features v) { properties = Map.fromList (coerceFromSVGProperties ps) } }
setSVGProperties ps v@KSVGView{} = v { features = (features v) { properties = Map.fromList (coerceFromSVGProperties ps) } }
setSVGProperties _ v = v

{-# INLINE addSVGProperties #-}
addSVGProperties ps v@SVGView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromSVGProperties ps)) (properties (features v)) } }
addSVGProperties ps v@KSVGView {} = v { features = (features v) { properties = Map.union (Map.fromList (coerceFromSVGProperties ps)) (properties (features v)) } }
addSVGProperties _ v = v

pattern SVGProperties :: [SVGProperty] -> View -> View
pattern SVGProperties ps v <- ((getSVGProperties &&& id) -> (Just ps,v)) where
  SVGProperties ps v = setSVGProperties ps v

pattern AddSVGProperties :: [SVGProperty] -> View -> View
pattern AddSVGProperties as v <- ((getSVGProperties &&& id) -> (Just as,v)) where
  AddSVGProperties as v = addSVGProperties as v

-- Lifecycles

{-# INLINE getLifecycles #-}
getLifecycles NullView {} = Nothing
getLifecycles TextView {} = Nothing
getLifecycles ComponentView {} = Nothing
getLifecycles SomeView {} = Nothing
getLifecycles v = Just (lifecycles (features v))

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

pattern Lifecycles :: [Lifecycle] -> View -> View
pattern Lifecycles lc v <- ((getLifecycles &&& id) -> (Just lc,v)) where
  Lifecycles lc v = setLifecycles lc v

pattern AddLifecycles :: [Lifecycle] -> View -> View
pattern AddLifecycles lc v <- ((getLifecycles &&& id) -> (Just lc,v)) where
  AddLifecycles lc v = addLifecycles lc v

-- XLinks

{-# INLINE getXLinks #-}
getXLinks SVGView {..} = Just (coerceToXLinks (Map.toList xlinks))
getXLinks KSVGView {..} = Just (coerceToXLinks (Map.toList xlinks))
getXLinks _ = Nothing

{-# INLINE setXLinks #-}
setXLinks xl khtml@SVGView {} = khtml { xlinks = Map.fromList (coerceFromXLinks xl) }
setXLinks xl ksvg@KSVGView {} = ksvg { xlinks = Map.fromList (coerceFromXLinks xl) }
setXLinks _ v = v

{-# INLINE addXLinks #-}
addXLinks xl v@SVGView {} = v { xlinks = Map.union (Map.fromList (coerceFromXLinks xl)) (xlinks v) }
addXLinks xl v@KSVGView {} = v { xlinks = Map.union (Map.fromList (coerceFromXLinks xl)) (xlinks v) }
addXLinks _ v = v

pattern XLinks :: [XLink] -> View -> View
pattern XLinks xl v <- ((getXLinks &&& id) -> (Just xl,v)) where
  XLinks xl v = setXLinks xl v

pattern AddXLinks :: [XLink] -> View -> View
pattern AddXLinks xl v <- ((getXLinks &&& id) -> (Just xl,v)) where
  AddXLinks xl v = addXLinks xl v

-- Combinators

infixr 0 <|
{-# INLINE (<|) #-}
(<|) :: a -> (a -> b) -> b
(<|) = flip ($)

{-# INLINE (<||>) #-}
(<||>) :: View -> [View] -> View
(<||>) v cs = Children cs v

{-# INLINE (<|#|>) #-}
(<|#|>) :: View -> [(Int,View)] -> View
(<|#|>) v cs = KeyedChildren cs v

infixl 1 |>
{-# INLINE (|>) #-}
(|>) :: (View -> c) -> [View] -> View -> c
(|>) f cs = f . flip setChildren cs

infixl 1 |#>
{-# INLINE (|#>) #-}
(|#>) :: (View -> c) -> [(Int,View)] -> View -> c
(|#>) f cs = f . flip setKeyedChildren cs

