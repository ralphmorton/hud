
module HUD.Frontend.Web.Route (
    class IsRoute,
    class Pathable,
    class Route,
    class Unroute,
    PathInfo(..),
    Comp,
    TP(..),
    L,
    Capture,
    QueryParam,
    Target,
    type (:/),
    (:->),
    fromPathInfo,
    toPath,
    fromPathSegment,
    toPathSegment,
    route,
    unroute
) where

import Prelude

import HUD.Dashboard.Data.Relational (AccountKey(..), UserKey(..))

import Control.MonadZero (guard)
import Data.Array (uncons)
import Data.Foldable (find)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String (toCharArray)
import Data.StrMap as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

--
-- IsRoute
--

class IsRoute a where
    fromPathInfo :: PathInfo -> Maybe a
    toPath :: a -> String

--
-- Pathable
--

class Pathable a where
    fromPathSegment :: String -> Maybe a
    toPathSegment :: a -> String

instance pathableString :: Pathable String where
    fromPathSegment = pure
    toPathSegment = id

instance pathableInt :: Pathable Int where
    fromPathSegment = fromString
    toPathSegment = show

instance pathableAccountKey :: Pathable AccountKey where
    fromPathSegment = pure <<< AccountKey <<< { unAccountKey: _ }
    toPathSegment (AccountKey k) = k.unAccountKey

instance pathableUserKey :: Pathable UserKey where
    fromPathSegment = pure <<< UserKey <<< { unUserKey: _ }
    toPathSegment (UserKey k) = k.unUserKey

--
-- PathInfo
--

data PathInfo = PathInfo (Array String) (SM.StrMap String)

--
-- Route
--

class Route p t a | p -> t, p -> a where
    route :: Proxy p -> PathInfo -> t -> Maybe a

--
-- Unroute
--

class Unroute p t | p -> t where
    unroute :: Proxy p -> t -> String

--
-- Comp
--

data Comp a b

infixr 5 type Comp as :/

--
-- TP
--

data TP a b = TP a b

infixr 5 type TP as :->
infixr 5 TP as :->

--
-- Lit (L)
--

data L (a :: Symbol)

instance routeLit :: (Route p t a, IsSymbol s) => Route (L s :/ p) t a where
    route _ (PathInfo px qpx) f = do
        let sval = reflectSymbol (SProxy :: SProxy s)
        r <- uncons px
        guard (r.head == sval)
        let pi = PathInfo r.tail qpx
        route (Proxy :: Proxy p) pi f

instance unrouteLit :: (Unroute p t, IsSymbol s) => Unroute (L s :/ p) t where
    unroute _ f = case rest == "" of
        true -> sval
        false -> sval <> "/" <> rest
        where
        sval = reflectSymbol (SProxy :: SProxy s)
        rest = unroute (Proxy :: Proxy p) f

--
-- Capture
--

data Capture (a :: Symbol) t

instance routeCapture :: (Route p t a, Pathable v) => Route (Capture s v :/ p) (v -> t) a where
    route _ (PathInfo px qpx) f = do
        r <- uncons px
        v <- fromPathSegment r.head
        let pi = PathInfo r.tail qpx
        route (Proxy :: Proxy p) pi (f v)

instance unrouteCapture :: (Unroute p t, Pathable v) => Unroute (Capture s v :/ p) (v :-> t) where
    unroute _ (v :-> t) = case rest == "" of
        true -> toPathSegment v
        false -> toPathSegment v <> "/" <> rest
        where
        rest = unroute (Proxy :: Proxy p) t

--
-- Query Param
--

data QueryParam (a :: Symbol) t

instance routeQueryParam :: (Route p t a, Pathable v, IsSymbol s) => Route (QueryParam s v :/ p) (Maybe v -> t) a where
    route _ pi@(PathInfo px qpx) f = do
        let sval = reflectSymbol (SProxy :: SProxy s)
        case SM.lookup sval qpx of
            Nothing -> route (Proxy :: Proxy p) pi (f Nothing)
            Just sv -> do
                v <- fromPathSegment sv
                route (Proxy :: Proxy p) pi (f (Just v))

instance unrouteQueryParam :: (Unroute p t, Pathable v, IsSymbol s) => Unroute (QueryParam s v :/ p) (Maybe v :-> t) where
    unroute _ (Nothing :-> t) = unroute (Proxy :: Proxy p) t
    unroute _ ((Just v) :-> t) = base <> sep <> reflectSymbol (SProxy :: SProxy s) <> "=" <> toPathSegment v
        where
        base = unroute (Proxy :: Proxy p) t
        sep = maybe "?" (const "&") $ find ((==)'?') (toCharArray base)

--
-- Target
--

data Target t

instance routeTarget :: Route (Target t) t t where
    route _ (PathInfo [] _) t = pure t
    route _ _ _ = Nothing

instance unrouteTarget :: Unroute (Target t) Unit where
    unroute _ _ = ""
