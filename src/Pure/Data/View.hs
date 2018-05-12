{-# LANGUAGE ExistentialQuantification, TypeFamilies, PatternSynonyms, ViewPatterns, ScopedTypeVariables, RankNTypes #-}
module Pure.Data.View where

-- pure-default
import Pure.Data.Default (Default(..))

-- pure-json
import Pure.Data.JSON (ToJSON,FromJSON)

-- pure-txt
import Pure.Data.Txt (FromTxt(..),ToTxt(..),Txt)

-- pure-queue
import Pure.Data.Queue (Queue)

-- base
import Control.Concurrent (MVar)
import Control.Monad.ST (ST)
import Data.IORef (IORef)
import Data.Proxy (Proxy(..))
import Data.STRef (STRef)
import Data.String (IsString(..))
import Data.Typeable (Typeable(..),TypeRep,typeRep,tyConName,typeRepTyCon)
import GHC.Generics (Generic(..))
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- containers
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Set (Set)

-- ghcjs-dom
import GHCJS.DOM.Element (Element)
import GHCJS.DOM.Node (Node)
import GHCJS.DOM.Text (Text)

data Property
  = Property
      { pName  :: {-# UNPACK #-}!Txt
      , pValue :: {-# UNPACK #-}!Txt
      }

data Attribute
  = Attribute
      { aName  :: {-# UNPACK #-}!Txt
      , aValue :: {-# UNPACK #-}!Txt
      }

data XLink
  = XLink
      { lName  :: {-# UNPACK #-}!Txt
      , lValue :: {-# UNPACK #-}!Txt
      }

data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  } deriving (Eq,Show)
instance Default Options where
  def = Options False False True

data Evt = Evt
  { evtObj            :: !Any
  , evtTarget         :: {-# UNPACK #-}!Node
  -- , evtRemoveListener :: IO () -- bad?
  }

data Target = ElementTarget | WindowTarget | DocumentTarget deriving Eq

data Listener =
  On
    { eventName     :: {-# UNPACK #-}!Txt
    , eventTarget   :: !Target
    , eventOptions  :: {-# UNPACK #-}!Options
    , eventAction   :: !(Evt -> IO ())
    , eventStopper  :: !(IO ())
    }

data Lifecycle =
  HostRef
    { withHost :: !(Node -> IO ())
    }

data Comp (m :: * -> *) props state =
    Comp
      { execute      :: forall a. m a -> IO a
      , construct    :: m state
      , initialize   :: state -> m state
      , initialized  :: m ()
      , mount        :: state -> m state
      , mounted      :: m ()
      , receiveProps :: props -> state -> m state
      , forceUpdate  :: props -> state -> m Bool
      , update       :: props -> state -> m ()
      , renderer     :: props -> state -> View
      , updated      :: props -> state -> View -> m ()
      , unmount      :: m ()
      , destruct     :: m ()
      }

pattern Component :: String -> props -> (Ref m props state -> Comp m props state) -> View
pattern Component nm p v <- ComponentView nm p _ v where
  Component nm p v = ComponentView nm p Nothing v

instance Monad m => Default (Comp m props state) where
  def =
    Comp
      { execute      = return (error "Component.execute: no executor specified")
      , renderer     = \_ _ -> Null
      , destruct     = return ()
      , unmount      = return ()
      , updated      = \_ _ _ -> return ()
      , update       = \_ _ -> return ()
      , forceUpdate  = \_ _ -> return True
      , receiveProps = \_ -> return
      , mounted      = return ()
      , mount        = return
      , initialized  = return ()
      , initialize   = return
      , construct    = return (error "Component.construct: no initial state.")
      }

data ComponentPatch m props state
  = Unmount (forall s. STRef s [IO ()] -> View -> ST s ()) (MVar (IO ()))
  | UpdateProperties props
  | UpdateState (props -> state -> m (state,m ()))

data Ref m props state
  = Ref
      { crType       :: {-# UNPACK #-}!Txt
      , crView       :: {-# UNPACK #-}!(IORef View)
      , crProps      :: {-# UNPACK #-}!(IORef props)
      , crState      :: {-# UNPACK #-}!(IORef state)
      , crComponent  :: {-# UNPACK #-}!(Comp m props state)
      , crPatchQueue :: {-# UNPACK #-}!(IORef (Maybe (Queue (ComponentPatch m props state))))
      }

data View
  -- NullView must have a presence on the page for proper diffing
  = NullView
        { elementHost :: Maybe Element
        }

  | TextView
        { textHost :: Maybe Text
        , content  :: Txt
        }

  | RawView
       { nodeHost   :: Maybe Node
       , tag        :: Txt
       , classes    :: Set Txt
       , styles     :: Map Txt [Txt]
       , attributes :: Set Attribute
       , properties :: Set Property
       , listeners  :: [Listener]
       , lifecycles :: [Lifecycle]
       , content    :: Txt
       }

  | HTMLView
       { elementHost :: Maybe Element
       , tag         :: Txt
       , classes     :: Set Txt
       , styles      :: Map Txt [Txt]
       , attributes  :: Set Attribute
       , properties  :: Set Property
       , listeners   :: [Listener]
       , lifecycles  :: [Lifecycle]
       , children    :: [View]
       }

  | KHTMLView
       { elementHost   :: Maybe Element
       , tag           :: Txt
       , classes       :: Set Txt
       , styles        :: Map Txt [Txt]
       , attributes    :: Set Attribute
       , properties    :: Set Property
       , listeners     :: [Listener]
       , lifecycles    :: [Lifecycle]
       , keyedChildren :: [(Int,View)]
       , childMap      :: IntMap View
       }

  | forall m props state. ComponentView
       { name   :: String
       , props  :: props
       , record :: Maybe (Ref m props state)
       , view   :: Ref m props state -> Comp m props state
       }

  | SVGView
       { elementHost :: Maybe Element
       , tag         :: Txt
       , classes     :: Set Txt
       , styles      :: Map Txt [Txt]
       , xlinks      :: Set XLink
       , attributes  :: Set Attribute
       , properties  :: Set Property
       , listeners   :: [Listener]
       , lifecycles  :: [Lifecycle]
       , children    :: [View]
       }

  | KSVGView
       { elementHost   :: Maybe Element
       , tag           :: Txt
       , classes       :: Set Txt
       , styles        :: Map Txt [Txt]
       , xlinks        :: Set XLink
       , attributes    :: Set Attribute
       , properties    :: Set Property
       , listeners     :: [Listener]
       , lifecycles    :: [Lifecycle]
       , keyedChildren :: [(Int,View)]
       , childMap      :: IntMap View
       }

  | forall a. Pure a => SomeView
       { name       :: String -- Typeable is too expensive here because of the weight of m
       , renderable :: a
       }

  -- StaticView
  --   :: { staticView :: View ms } -> View ms

instance Default View where
  def = NullView Nothing

instance IsString View where
  fromString = TextView Nothing . fromString

instance FromTxt View where
  fromTxt = TextView Nothing

class Pure a where
  -- TODO:
  --   build :: a m -> IO (View m)
  --   diff :: (m () -> IO ()) -> Node -> View m -> a m -> a m -> IO (View m)
  -- With build and diff the only primitive view elements would be HTML, SVG, Managed, and View.
  -- Great avenue for extensibility and modularity, but I don't see that the expressivity gained
  -- would currently justify the work; it's mostly just a refactoring, but it is a major refactoring.
  render :: a -> View

instance Pure View where
  render (SomeView _ a) = render a
  render a = a

pattern Null :: View
pattern Null <- (NullView _) where
  Null = NullView Nothing

pattern Text :: (ToTxt t, FromTxt t) => t -> View
pattern Text t <- (TextView _ (fromTxt -> t)) where
  Text t = TextView Nothing (toTxt t)

typeOfSomeView :: forall t. Typeable t => t -> TypeRep
typeOfSomeView _ = typeRep (Proxy :: Proxy t)

pattern View :: forall a. (Pure a, Typeable a) => a -> View
pattern View a <- (SomeView ((==) (tyConName (typeRepTyCon (typeOfSomeView (undefined :: a)))) -> True) (unsafeCoerce -> a)) where
  View a = SomeView (tyConName (typeRepTyCon (typeOfSomeView (undefined :: a)))) a

type P = [ IO () ]
type Plan s = STRef s P

type Diff a = a -> a -> a -> IO a
type Diff' s a = a -> a -> a -> ST s a

