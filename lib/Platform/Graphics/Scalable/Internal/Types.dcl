definition module Graphics.Scalable.Internal.Types

import Graphics.Scalable.Types

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
  = ColumnXSpan !ImageTag !Int             // (ColumnXSpan t a) is x-span of column number a in grid tagged with t
  | RowYSpan    !ImageTag !Int             // (RowYSpan t a) is y-span of row number a in grid tagged with t
  | ImageXSpan  !ImageTag                  // (ImageXSpan t) is x-span of image tagged with t
  | ImageYSpan  !ImageTag                  // (ImageYSpan t) is y-span of image tagged with t
  | TextXSpan   !FontDef !String           // (TextXSpan a b) is width of text b written in font a
  
  | PathXSpan   !ImageTag                  // (PathXSpan t) is x-span of path element tagged with t
  | PathYSpan   !ImageTag                  // (PathYSpan t) is y-span of path element tagged with t

class (*.) infixl 7 a :: !a !n -> Span | toReal n
class (/.) infixl 7 a :: !a !n -> Span | toReal n

instance zero Span
instance +    Span
instance -    Span
instance abs  Span
instance ~    Span
instance *.   Span, Real, Int
instance *    Span
instance /.   Span, Real, Int
instance /    Span

isPxSpan    :: !Span -> Bool               // returns True only if argument is (PxSpan ...)
getPxSpan   :: !Span -> Real               // returns r only if argument is (PxSpan r), aborts otherwise

px          :: !Real            -> Span    // (px r) is r pixels
textxspan   :: !FontDef !String -> Span    // (textxspan font str) is the x-span of str written in font
imagexspan  :: !ImageTag        -> Span    // (imagexspan t) is x-span of image tagged with t
imageyspan  :: !ImageTag        -> Span    // (imageyspan t) is y-span of image tagged with t
columnspan  :: !ImageTag !Int   -> Span    // (columnspan t i) is x-span of column i (counting from 0) in grid tagged with t
rowspan     :: !ImageTag !Int   -> Span    // (rowspan    t i) is y-span of row    i (counting from 0) in grid tagged with t
minSpan     :: ![Span]          -> Span    // (minSpan as) is the minimum of as (zero if as = [])
maxSpan     :: ![Span]          -> Span    // (maxSpan as) is the maximum of as (zero if as = [])

:: ImageTag
  = ImageTagUser   !ImgTagNo !String
  | ImageTagSystem !ImgTagNo
:: ImgTagNo :== Int                        // internal numbering of (sub) images

instance == ImageTag
instance <  ImageTag
