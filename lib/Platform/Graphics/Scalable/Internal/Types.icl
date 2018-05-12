implementation module Graphics.Scalable.Internal.Types

import Data.List
from StdOrdList import minList, maxList
import Graphics.Scalable.Types
import StdBool, StdInt, StdMisc, StdReal, StdString

:: Span
  = PxSpan      !Real                      // (PxSpan a) is a pixels
  | LookupSpan  !LookupSpan                // (LookupSpan a) needs to be looked up after computing dimensions
  | AddSpan     !Span !Span                // (AddSpan a b) is span a + span b
  | SubSpan     !Span !Span                // (SubSpan a b) is span a - span b
  | MulSpan     !Span !Span                // (MulSpan a b) is span a * span k
  | DivSpan     !Span !Span                // (DivSpan a b) is span a / span k
  | AbsSpan     !Span                      // (AbsSpan a)  is absolute value of span a
  | MinSpan     ![Span]                    // (MinSpan as) is minimum span value in as
  | MaxSpan     ![Span]                    // (MaxSpan as) is maximum span value in as
:: LookupSpan
  = ColumnXSpan !ImageTag !Int             // (ColumnXSpan as a) is x-span of column number a in grid tagged with superset of as
  | RowYSpan    !ImageTag !Int             // (RowYSpan as a) is y-span of row number a in grid tagged with superset of as
  | ImageXSpan  !ImageTag                  // (ImageXSpan as) is x-span of image tagged with superset of as
  | ImageYSpan  !ImageTag                  // (ImageYSpan as) is y-span of image tagged with superset of as
  | TextXSpan   !FontDef !String           // (TextXSpan a b) is width of text b written in font a
  | PathXSpan   !ImageTag                  // (PathXSpan t) is x-span of path element tagged with t
  | PathYSpan   !ImageTag                  // (PathYSpan t) is y-span of path element tagged with t

/* The span computations below simplify their expressions by applying actual computations on (PxSpan _) values as much as possible.
   We denote a value (PxSpan _) with a capital letter. If we do not care or do not know, we denote the value with a lowercase letter.
   For instance: 
       (a * B) / C states that we know the values of B and C, but not of a. This can be simplified to:
       (B / C) * a
   In reflexive operations, we attempt to keep the (PxSpan _) values at the left operand.
   In the simplification functions, the first rule is always the 'eager' rule which applies the actual computation, and the last rule 
   is always the 'deferred' rule which only constructs the span expression lazily.
*/
instance / Span where
  / (PxSpan a)                (PxSpan b)                = PxSpan (a / b)
  / z=:(PxSpan 0.0)           x                         = z                                            // ZERO / x              = ZERO
  / x                         (PxSpan 1.0)              = x                                            // x / ONE               = x
  / (MulSpan (PxSpan b) a)    (MulSpan c (PxSpan d))
     | b == d                                           = a / c                                        // (B * a) / (c * B)     = a / c
  / (MulSpan a (PxSpan b))    (MulSpan c (PxSpan d))
     | b == d                                           = a / c                                        // (a * B) / (c * B)     = a / c
  / (MulSpan (PxSpan b) a)    (MulSpan (PxSpan d) c)
     | b == d                                           = a / c                                        // (B * a) / (B * c)     = a / c
  / (MulSpan a (PxSpan b))    (MulSpan (PxSpan d) c)
     | b == d                                           = a / c                                        // (a * B) / (B * c)     = a / c
  / (MulSpan (PxSpan b) a)    (PxSpan c)                = (PxSpan (b / c)) * a                         // (B * a) / C           = (B / C) * a
  / (MulSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b / c)) * a                         // (a * B) / C           = (B / C) * a
  / (DivSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (1.0 / (b * c))) * a                 // (a / B) / C           = (1 / (B*C)) * a
  / (AddSpan (PxSpan a) b)    (PxSpan c)                = PxSpan (a / c) + b / (PxSpan c)              // (A + b) / C           = A / C + b / C
  / (SubSpan (PxSpan a) b)    (PxSpan c)                = PxSpan (a / c) - b / (PxSpan c)              // (A - b) / C           = A / C - b / C
  / (AddSpan a (PxSpan b))    (PxSpan c)                = PxSpan (b / c) + a / (PxSpan c)              // (a + B) / C           = B / C + a / C
  / (SubSpan a (PxSpan b))    (PxSpan c)                = PxSpan (0.0 - b / c) + a / (PxSpan c)        // (a - B) / C           = (0 - B / C) + a / C
  / l                         r                         = DivSpan l r

instance * Span where
  * (PxSpan a)                (PxSpan b)                = PxSpan (a * b)
  * z=:(PxSpan 0.0)           x                         = z                                            // ZERO * x              = ZERO
  * x                         z=:(PxSpan 0.0)           = z                                            // x * ZERO              = ZERO
  * (PxSpan 1.0)              x                         = x                                            // ONE * x               = x
  * x                         (PxSpan 1.0)              = x                                            // x * ONE               = x
  * (PxSpan a)                (MulSpan (PxSpan b) c)    = (PxSpan (a * b)) * c                         // A * (B * c)           = (A * B) * c
  * (PxSpan a)                (MulSpan b (PxSpan c))    = (PxSpan (a * c)) * b                         // A * (b * C)           = (A * C) * b
  * (MulSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a * c)) * b                         // (A * b) * C           = (A * C) * b
  * (MulSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b * c)) * a                         // (a * B) * C           = (B * C) * a
  * (DivSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a * c)) / b                         // (A / b) * C           = (A * C) / b
  * (DivSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (c / b)) * a                         // (a / B) * C           = (C / B) * a
  * (PxSpan c)                (DivSpan (PxSpan a) b)    = (PxSpan (c * a)) / b                         // C * (A / b)           = (C * A) / b
  * (PxSpan c)                (DivSpan a (PxSpan b))    = (PxSpan (c / b)) * a                         // C * (a / B)           = (C / B) * a
  * (PxSpan a)                (AddSpan (PxSpan b) c)    = (PxSpan (a * b)) + (PxSpan a) * c            // A * (B + c)           = A * B + A * c
  * (AddSpan (PxSpan b) c)    (PxSpan a)                = (PxSpan (a * b)) + (PxSpan a) * c            // (B + c) * A           = A * B + A * c
  * (PxSpan a)                (AddSpan c (PxSpan b))    = (PxSpan (a * b)) + (PxSpan a) * c            // A * (c + B)           = A * B + A * c
  * (AddSpan c (PxSpan b))    (PxSpan a)                = (PxSpan (a * b)) + (PxSpan a) * c            // (c + B) * A           = A * B + A * c
  * (PxSpan a)                (SubSpan (PxSpan b) c)    = (PxSpan (a * b)) - (PxSpan a) * c            // A * (B - c)           = A * B - A * c
  * (SubSpan (PxSpan b) c)    (PxSpan a)                = (PxSpan (a * b)) - (PxSpan a) * c            // (B - c) * A           = A * B - A * c
  * (PxSpan a)                (SubSpan c (PxSpan b))    = (PxSpan (0.0 - a * b)) + (PxSpan a) * c      // A * (c - B)           = (0 - A * B) + A * c
  * (SubSpan c (PxSpan b))    (PxSpan a)                = (PxSpan (0.0 - a * b)) + (PxSpan a) * c      // (c - B) * A           = (0 - A * B) + A * c
  * l                         r                         = MulSpan l r

instance + Span where
  + (PxSpan a)                (PxSpan b)                = PxSpan (a + b)
  + (PxSpan 0.0)              x                         = x                                            // ZERO + x              = x
  + x                         (PxSpan 0.0)              = x                                            // x + ZERO              = x
  + (PxSpan a)                (AddSpan (PxSpan b) c)    = (PxSpan (a + b)) + c                         // A + (B + c)           = (A + B) + c
  + (PxSpan a)                (AddSpan b (PxSpan c))    = (PxSpan (a + c)) + b                         // A + (b + C)           = (A + C) + b
  + (AddSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a + c)) + b                         // (A + b) + C           = (A + C) + b
  + (AddSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b + c)) + a                         // (a + B) + C           = (B + C) + a
  + (SubSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a + c)) - b                         // (A - b) + C           = (A + C) - b
  + (SubSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (c - b)) + a                         // (a - B) + C           = (C - B) + a
  + (PxSpan a)                (SubSpan b (PxSpan c))    = (PxSpan (a - c)) + b                         // A + (b - C)           = (A - C) + b
  + (PxSpan a)                (SubSpan (PxSpan b) c)    = (PxSpan (a + b)) - c                         // A + (B - c)           = (A + B) - c
  + (DivSpan a l=:(PxSpan b)) (DivSpan c r=:(PxSpan d))
     | b == d                                           = (PxSpan (1.0 / b)) * (a + c)                 // (a / B) + (c / B)     = (1 / B) * (a + c)
  + (MulSpan p=:(PxSpan a) b) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (b + d)                                  // A * b + A * d         = A * (b + d)
  + (MulSpan b p=:(PxSpan a)) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (d + b)                                  // b * A + A * d         = A * (d + b)
  + (MulSpan p=:(PxSpan a) b) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b + d)                                  // A * b + d * A         = A * (b + d)
  + (MulSpan b p=:(PxSpan a)) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b + d)                                  // b * A + d * A         = A * (b + d)
//  + a=:(PxSpan _)             (MaxSpan xs)              = maxSpan (strictTRMap ((+) a) xs)             // a + MAX {x_1 ... x_n} = MAX {a + x_1 ... a + x_n}
//  + (MaxSpan xs)              a=:(PxSpan _)             = maxSpan (strictTRMap ((+) a) xs)             // MAX {x_1 ... x_n} + a = MAX {a + x_1 ... a + x_n}
  + s                         t                         = AddSpan s t

instance - Span where
  - (PxSpan a)                (PxSpan b)                = PxSpan (a - b)
  - a                         (PxSpan 0.0)              = a                                            // a - ZERO              = a
  - (SubSpan (PxSpan b) a)    (PxSpan c)                = (PxSpan (b - c)) - a                         // (B - a) - C           = (B - C) - a
  - (SubSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (0.0 - b - c)) + a                   // (a - B) - C           = (0 - B - C) + a
  - (PxSpan c)                (SubSpan a (PxSpan b))    = (PxSpan (c + b)) - a                         // C - (a - B)           = (C + B) - a
  - (PxSpan c)                (SubSpan (PxSpan b) a)    = (PxSpan (c - b)) + a                         // C - (B - a)           = (C - B) + a
  - (AddSpan (PxSpan a) b)    (PxSpan c)                = (PxSpan (a - c)) + b                         // (A + b) - C           = (A - C) + b
  - (AddSpan a (PxSpan b))    (PxSpan c)                = (PxSpan (b - c)) + a                         // (a + B) - C           = (B - C) + a
  - (PxSpan c)                (AddSpan a (PxSpan b))    = (PxSpan (c - b)) - a                         // C - (a + B)           = (C - B) - a
  - (PxSpan c)                (AddSpan (PxSpan a) b)    = (PxSpan (c - a)) - b                         // C - (A + b)           = (C - A) - b
  - (DivSpan a l=:(PxSpan b)) (DivSpan c r=:(PxSpan d))
     | b == d                                           = (PxSpan (1.0 / b)) * (a - c)                 // (a / B) - (c / B)     = (1 / B) * (a - c)
  - (MulSpan p=:(PxSpan a) b) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (b - d)                                  // A * b - A * d         = A * (b - d)
  - (MulSpan b p=:(PxSpan a)) (MulSpan (PxSpan c) d)
     | a == c                                           = p * (b - d)                                  // b * A - A * d         = A * (b - d)
  - (MulSpan p=:(PxSpan a) b) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b - d)                                  // A * b - d * A         = A * (b - d)
  - (MulSpan b p=:(PxSpan a)) (MulSpan d (PxSpan c))
     | a == c                                           = p * (b - d)                                  // b * A - d * A         = A * (b - d)
//  - (MaxSpan xs)              a=:(PxSpan _)             = maxSpan (strictTRMap (\x -> x - a) xs)       // MAX {x_1 ... x_n} - a = MAX {x_1 - a ... x_n - a}
  - s                         t                         = SubSpan s t

instance /.   Span where /. a c                         = a / (PxSpan (toReal c))

instance /.   Int  where /. a c                         = PxSpan (toReal a / toReal c)

instance /.   Real where /. a c                         = PxSpan (a / toReal c)

instance *.   Span where *. a c                         = (PxSpan (toReal c)) * a

instance *.   Int  where *. a c                         = PxSpan (toReal a * toReal c)

instance *.   Real where *. a c                         = PxSpan (a * toReal c)

instance zero Span where zero                           = PxSpan zero

instance abs  Span where abs (PxSpan  x)                = PxSpan (abs x)
                         abs (AbsSpan x)                = AbsSpan x
                         abs span                       = AbsSpan span

instance ~    Span where ~   s                          = zero - s

isPxSpan :: !Span -> Bool
isPxSpan (PxSpan _) = True
isPxSpan _          = False

getPxSpan :: !Span -> Real
getPxSpan (PxSpan r) = r
getPxSpan _          = abort "Fatal error in module Graphics.Scalable.Internal.Types: getPxSpan applied to illegal argument"

px :: !Real -> Span
px a = PxSpan a

textxspan :: !FontDef !String -> Span
textxspan a b = LookupSpan (TextXSpan a b)

imagexspan :: !ImageTag -> Span
imagexspan t = LookupSpan (ImageXSpan t)

imageyspan :: !ImageTag -> Span
imageyspan t = LookupSpan (ImageYSpan t)

columnspan :: !ImageTag !Int -> Span
columnspan t a = LookupSpan (ColumnXSpan t a)

rowspan :: !ImageTag !Int -> Span
rowspan t a = LookupSpan (RowYSpan t a)

minSpan :: ![Span] -> Span
minSpan []  = zero
minSpan [x] = x
minSpan spans
  #! spans` = flattenMinSpans spans []
  = case partition isPxSpan spans` of
      (pxs, [])     -> minPxs pxs
      ([], others)  -> MinSpan others
      (pxs, others) -> MinSpan [minPxs pxs : others]
  where
  minPxs :: ![Span] -> Span
  minPxs pxs = PxSpan (minList [x \\ PxSpan x <- pxs])

  flattenMinSpans :: ![Span] ![Span] -> [Span]
  flattenMinSpans []              acc = acc
  flattenMinSpans [MinSpan os:xs] acc = flattenMinSpans xs (os ++ acc)
  flattenMinSpans [x:xs]          acc = flattenMinSpans xs [x:acc]

maxSpan :: ![Span] -> Span
maxSpan []  = zero
maxSpan [x] = x
maxSpan spans
  #! spans` = flattenMaxSpans spans []
  = case partition isPxSpan spans` of
      (pxs, [])     -> maxPxs pxs
      ([], others)  -> MaxSpan others
      (pxs, others) -> MaxSpan [maxPxs pxs : others]
  where
  maxPxs :: ![Span] -> Span
  maxPxs pxs = PxSpan (maxList [x \\ PxSpan x <- pxs])

  flattenMaxSpans :: ![Span] ![Span] -> [Span]
  flattenMaxSpans []              acc = acc
  flattenMaxSpans [MaxSpan os:xs] acc = flattenMaxSpans xs (os ++ acc)
  flattenMaxSpans [x:xs]          acc = flattenMaxSpans xs [x:acc]

instance == ImageTag where == (ImageTagUser n1 s1) (ImageTagUser n2 s2) = n1 == n2 && s1 == s2
                           == (ImageTagSystem  s1) (ImageTagSystem  s2) = s1 == s2
                           == _                    _                    = False
instance <  ImageTag where <  (ImageTagUser n1 s1) (ImageTagUser n2 s2) = n1 < n2 || (n1 == n2 && s1 < s2)
                           <  (ImageTagUser _  _)  _                    = True
                           <  (ImageTagSystem  s1) (ImageTagSystem  s2) = s1 < s2
                           <  _                    _                    = False
