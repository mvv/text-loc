{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This modules provides data types that describe line-column locations
--   within a text.
module Text.Loc
  ( HasLoc(..)
  , LocLens(..)
  , Span(..)
  , SpannedLoc(..)
  , locStart
  , locEnd
  , spanOf
  , startOf
  , endOf
  , SpannedLens(..)
  , lStart
  , lEnd
  , lSpanOf
  , lEndOf
  , lStartOf
  , Located(..)
  , LineCol(..)
  , nextLine
  , nextCol
  , LineColLoc(..)
  , locLine
  , locCol
  , lineColOf
  , lineOf
  , colOf
  , LineColLens(..)
  , lLine
  , lCol
  , lLineColOf
  , lLineOf
  , lColOf
  , SrcLoc(..)
  , srcOf
  , SrcLens(..)
  , lSrcOf
  , InSrc(..)
  , FromLoc(..)
  , fromOf
  , FromLens(..)
  , lFromOf
  , Nested(..)
  ) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Hashable (Hashable(..))
import Data.Word (Word)

-- | Values with associated source locations.
class HasLoc α where
  type LocOf α
  locOf ∷ α → LocOf α

-- | Values with editable associated source locations.
class HasLoc α ⇒ LocLens α where
  lLocOf ∷ Functor f ⇒ (LocOf α → f (LocOf α)) → α → f α

-- | A region of text between two locations.
data Span l = Span l l
              deriving (Typeable, Generic, Show, Read, Eq, Ord,
                        Functor, Foldable, Traversable)

instance Hashable l ⇒ Hashable (Span l) where
  hashWithSalt salt (Span s e) = hashWithSalt salt (s, e)
  {-# INLINE hashWithSalt #-}

instance HasLoc (Span l) where
  type LocOf (Span l) = Span l
  locOf = id
  {-# INLINE locOf #-}

instance LocLens (Span l) where
  lLocOf = id
  {-# INLINE lLocOf #-}

-- | Spanned locations.
class SpannedLoc l where
  type SpanLoc l
  locSpan ∷ l → Span (SpanLoc l)

instance SpannedLoc (Span l) where
  type SpanLoc (Span l) = l
  locSpan = id
  {-# INLINE locSpan #-}

-- | The start of a span.
locStart ∷ SpannedLoc l ⇒ l → SpanLoc l
locStart l = let Span s _ = locSpan l in s
{-# INLINE locStart #-}

-- | The end of a span.
locEnd ∷ SpannedLoc l ⇒ l → SpanLoc l
locEnd l = let Span _ e = locSpan l in e
{-# INLINE locEnd #-}

-- | The span of the associated location.
spanOf ∷ (HasLoc α, SpannedLoc (LocOf α)) ⇒ α → Span (SpanLoc (LocOf α))
spanOf = locSpan . locOf
{-# INLINE spanOf #-}

-- | The start of the associated location.
startOf ∷ (HasLoc α, SpannedLoc (LocOf α)) ⇒ α → SpanLoc (LocOf α)
startOf = locStart . locOf
{-# INLINE startOf #-}

-- | The end of the associated location.
endOf ∷ (HasLoc α, SpannedLoc (LocOf α)) ⇒ α → SpanLoc (LocOf α)
endOf = locEnd . locOf
{-# INLINE endOf #-}

-- | Editable spanned locations.
class SpannedLoc l ⇒ SpannedLens l where
  lSpan ∷ Functor f ⇒ (Span (SpanLoc l) → f (Span (SpanLoc l))) → l → f l

instance SpannedLens (Span l) where
  lSpan = id
  {-# INLINE lSpan #-}

-- | Provides access to the start of a location.
lStart ∷ (Functor f, SpannedLens l) ⇒ (SpanLoc l → f (SpanLoc l)) → l → f l
lStart f = lSpan (\(Span s e) → fmap (flip Span e) (f s))
{-# INLINE lStart #-}

-- | Provides access to the end of a location.
lEnd ∷ (Functor f, SpannedLens l) ⇒ (SpanLoc l → f (SpanLoc l)) → l → f l
lEnd f = lSpan (\(Span s e) → fmap (Span s) (f e))
{-# INLINE lEnd #-}

-- | Provides access to the span of the associated location.
lSpanOf ∷ (Functor f, LocLens α, SpannedLens (LocOf α))
        ⇒ (Span (SpanLoc (LocOf α)) → f (Span (SpanLoc (LocOf α)))) → α → f α
lSpanOf = lLocOf . lSpan
{-# INLINE lSpanOf #-}

-- | Provides access to the start of the associated location.
lStartOf ∷ (Functor f, LocLens α, SpannedLens (LocOf α))
         ⇒ (SpanLoc (LocOf α) → f (SpanLoc (LocOf α))) → α → f α
lStartOf = lLocOf . lStart
{-# INLINE lStartOf #-}

-- | Provides access to the end of the associated location.
lEndOf ∷ (Functor f, LocLens α, SpannedLens (LocOf α))
       ⇒ (SpanLoc (LocOf α) → f (SpanLoc (LocOf α))) → α → f α
lEndOf = lLocOf . lEnd
{-# INLINE lEndOf #-}

-- | A simple way to attach location to a value.
data Located l α = Located { locAt  ∷ l
                           , locVal ∷ α }
                   deriving (Typeable, Generic, Show, Read, Eq,
                             Functor, Foldable, Traversable)

instance (Hashable l, Hashable α) ⇒ Hashable (Located l α) where
  hashWithSalt salt (Located l a) = hashWithSalt salt (l, a)
  {-# INLINE hashWithSalt #-}

instance HasLoc (Located l α) where
  type LocOf (Located l α) = l
  locOf = locAt
  {-# INLINE locOf #-}

instance LocLens (Located l α) where
  lLocOf f (Located l a) = fmap (flip Located a) (f l)
  {-# INLINE lLocOf #-}

-- | Line-column location within a text.
data LineCol = LineCol {-# UNPACK #-} !Word {-# UNPACK #-} !Word
               deriving (Typeable, Generic, Show, Read, Eq, Ord, Bounded)

instance Hashable LineCol where
  hashWithSalt salt (LineCol l c) = hashWithSalt salt (l, c)
  {-# INLINE hashWithSalt #-}

instance HasLoc LineCol where
  type LocOf LineCol = LineCol
  locOf = id
  {-# INLINE locOf #-}

-- | The location of the start of the next line.
nextLine ∷ LineCol → LineCol
nextLine (LineCol l _) = LineCol (l + 1) 1
{-# INLINE nextLine #-}

-- | The location of the next column.
nextCol ∷ LineCol → LineCol
nextCol (LineCol l c) = LineCol l (c + 1)
{-# INLINE nextCol #-}

-- | Locations with line and column numbers.
class LineColLoc l where
  locLineCol ∷ l → LineCol

instance LineColLoc LineCol where
  locLineCol = id
  {-# INLINE locLineCol #-}

-- | The line number of a location.
locLine ∷ LineColLoc l ⇒ l → Word
locLine l = let LineCol ln _ = locLineCol l in ln
{-# INLINE locLine #-}

-- | The column number of a location.
locCol ∷ LineColLoc l ⇒ l → Word
locCol l = let LineCol _ c = locLineCol l in c
{-# INLINE locCol #-}

-- | The line-column numbers of the associated location.
lineColOf ∷ (HasLoc α, LineColLoc (LocOf α)) ⇒ α → LineCol
lineColOf = locLineCol . locOf
{-# INLINE lineColOf #-}

-- | The line number of the associated location.
lineOf ∷ (HasLoc α, LineColLoc (LocOf α)) ⇒ α → Word
lineOf = locLine . lineColOf
{-# INLINE lineOf #-}

-- | The column number of the associated location.
colOf ∷ (HasLoc α, LineColLoc (LocOf α)) ⇒ α → Word
colOf = locCol . lineColOf
{-# INLINE colOf #-}

-- | Locations with editable line and column numbers.
class LineColLens l where
  lLineCol ∷ Functor f ⇒ (LineCol → f LineCol) → l → f l

instance LineColLens LineCol where
  lLineCol = id
  {-# INLINE lLineCol #-}

-- | Provides access to the line number of a location.
lLine ∷ (Functor f, LineColLens l) ⇒ (Word → f Word) → l → f l
lLine f = lLineCol (\(LineCol l c) → fmap (flip LineCol c) (f l))
{-# INLINE lLine #-}

-- | Provides access to the column number of a location.
lCol ∷ (Functor f, LineColLens l) ⇒ (Word → f Word) → l → f l
lCol f = lLineCol (\(LineCol l c) → fmap (LineCol l) (f c))
{-# INLINE lCol #-}

-- | Provides access to the line-column numbers of the associated location.
lLineColOf ∷ (Functor f, LocLens α, LineColLens (LocOf α))
           ⇒ (LineCol → f LineCol) → α → f α
lLineColOf = lLocOf . lLineCol
{-# INLINE lLineColOf #-}

-- | Provides access to the line number of the associated location.
lLineOf ∷ (Functor f, LocLens α, LineColLens (LocOf α))
        ⇒ (Word → f Word) → α → f α
lLineOf = lLocOf . lLine
{-# INLINE lLineOf #-}

-- | Provides access to the column number of the associated location.
lColOf ∷ (Functor f, LocLens α, LineColLens (LocOf α))
       ⇒ (Word → f Word) → α → f α
lColOf = lLocOf . lCol
{-# INLINE lColOf #-}

-- | Locations with a source (e.g. file name).
class SrcLoc l where
  type LocSrc l
  locSrc ∷ l → LocSrc l

-- | The source of the associated location.
srcOf ∷ (HasLoc α, SrcLoc (LocOf α)) ⇒ α → LocSrc (LocOf α)
srcOf = locSrc . locOf
{-# INLINE srcOf #-}

-- | Locations with editable source (e.g. file name).
class SrcLoc l ⇒ SrcLens l where
  lSrc ∷ Functor f ⇒ (LocSrc l → f (LocSrc l)) → l → f l

-- | Provides access to the source of the associated location.
lSrcOf ∷ (Functor f, LocLens α, SrcLens (LocOf α))
       ⇒ (LocSrc (LocOf α) → f (LocSrc (LocOf α))) → α → f α
lSrcOf = lLocOf . lSrc
{-# INLINE lSrcOf #-}

-- | A simple way to add source to a location.
data InSrc s l = InSrc { srcSrc ∷ s
                       , srcLoc ∷ l }
                 deriving (Typeable, Generic, Show, Read, Eq, Ord)

instance (Hashable s, Hashable l) ⇒ Hashable (InSrc s l) where
  hashWithSalt salt (InSrc s l) = hashWithSalt salt (s, l)
  {-# INLINE hashWithSalt #-}

instance HasLoc (InSrc s l) where
  type LocOf (InSrc s l) = InSrc s l
  locOf = id
  {-# INLINE locOf #-}

instance LocLens (InSrc s l) where
  lLocOf = id
  {-# INLINE lLocOf #-}

instance SpannedLoc l ⇒ SpannedLoc (InSrc s l) where
  type SpanLoc (InSrc s l) = InSrc s (SpanLoc l)
  locSpan (InSrc s l) = fmap (InSrc s) (locSpan l)
  {-# INLINE locSpan #-}

instance SpannedLens l ⇒ SpannedLens (InSrc s l) where
  lSpan f (InSrc s l) =
    fmap (InSrc s) (lSpan (fmap (fmap srcLoc) . f . fmap (InSrc s)) l)
  {-# INLINE lSpan #-}

instance LineColLoc l ⇒ LineColLoc (InSrc s l) where
  locLineCol = locLineCol . srcLoc
  {-# INLINE locLineCol #-}

instance LineColLens l ⇒ LineColLens (InSrc s l) where
  lLineCol f (InSrc s l) = fmap (InSrc s) (lLineCol f l)
  {-# INLINE lLineCol #-}

instance SrcLoc (InSrc s l) where
  type LocSrc (InSrc s l) = s
  locSrc = srcSrc
  {-# INLINE locSrc #-}

instance SrcLens (InSrc s l) where
  lSrc f (InSrc s l) = fmap (flip InSrc l) (f s)
  {-# INLINE lSrc #-}

-- | Locations that have parent locations.
class FromLoc l where
  type LocFrom l
  locFrom ∷ l → LocFrom l

instance FromLoc l ⇒ FromLoc (InSrc s l) where
  type LocFrom (InSrc s l) = LocFrom l
  locFrom = locFrom . srcLoc
  {-# INLINE locFrom #-}

instance FromLens l ⇒ FromLens (InSrc s l) where
  lFrom f (InSrc s l) = fmap (InSrc s) (lFrom f l)
  {-# INLINE lFrom #-}

-- | The parent of the associated location.
fromOf ∷ (HasLoc α, FromLoc (LocOf α)) ⇒ α → LocFrom (LocOf α)
fromOf = locFrom . locOf
{-# INLINE fromOf #-}

-- | Locations with editable parent locations.
class FromLoc l ⇒ FromLens l where
  lFrom ∷ Functor f ⇒ (LocFrom l → f (LocFrom l)) → l → f l

-- | Provides access to the parent of the associated location.
lFromOf ∷ (Functor f, LocLens α, FromLens (LocOf α))
        ⇒ (LocFrom (LocOf α) → f (LocFrom (LocOf α))) → α → f α
lFromOf = lLocOf . lFrom
{-# INLINE lFromOf #-}

-- | A simple way to nest a location.
data Nested l p = Nested { nestedLoc  ∷ l
                         , nestedFrom ∷ p }
                  deriving (Typeable, Generic, Show, Read, Eq)

instance (Hashable l, Hashable p) ⇒ Hashable (Nested l p) where
  hashWithSalt salt (Nested l p) = hashWithSalt salt (l, p)
  {-# INLINE hashWithSalt #-}

instance HasLoc (Nested l p) where
  type LocOf (Nested l p) = Nested l p
  locOf = id
  {-# INLINE locOf #-}

instance LocLens (Nested l p) where
  lLocOf = id
  {-# INLINE lLocOf #-}

instance SpannedLoc l ⇒ SpannedLoc (Nested l p) where
  type SpanLoc (Nested l p) = Nested (SpanLoc l) p
  locSpan (Nested l p) = fmap (flip Nested p) (locSpan l)
  {-# INLINE locSpan #-}

instance SpannedLens l ⇒ SpannedLens (Nested l p) where
  lSpan f (Nested l p) =
    fmap (flip Nested p)
         (lSpan (fmap (fmap nestedLoc) . f . fmap (flip Nested p)) l)
  {-# INLINE lSpan #-}

instance LineColLoc l ⇒ LineColLoc (Nested l p) where
  locLineCol = locLineCol . nestedLoc
  {-# INLINE locLineCol #-}

instance LineColLens l ⇒ LineColLens (Nested l p) where
  lLineCol f (Nested l p) = fmap (flip Nested p) (lLineCol f l)
  {-# INLINE lLineCol #-}

instance SrcLoc l ⇒ SrcLoc (Nested l p) where
  type LocSrc (Nested l p) = LocSrc l
  locSrc = locSrc . nestedLoc
  {-# INLINE locSrc #-}

instance SrcLens l ⇒ SrcLens (Nested l p) where
  lSrc f (Nested l p) = fmap (flip Nested p) (lSrc f l)
  {-# INLINE lSrc #-}

instance FromLoc (Nested l p) where
  type LocFrom (Nested l p) = p
  locFrom = nestedFrom
  {-# INLINE locFrom #-}

instance FromLens (Nested l p) where
  lFrom f (Nested l p) = fmap (Nested l) (f p)
  {-# INLINE lFrom #-}
