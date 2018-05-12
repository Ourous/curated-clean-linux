definition module Graphics.Layout

:: GridDimension = Rows !Int | Columns !Int
:: GridLayout  :== (!GridMajor, !GridXLayout, !GridYLayout)
:: GridMajor     = ColumnMajor | RowMajor
:: GridXLayout   = LeftToRight | RightToLeft
:: GridYLayout   = TopToBottom | BottomToTop

:: XAlign
  = AtLeft
  | AtMiddleX
  | AtRight

:: YAlign
  = AtTop
  | AtMiddleY
  | AtBottom

:: XYAlign :== (!XAlign, !YAlign)

class Layout thing ~size ~offset ~host where
  collage :: [offset] [thing m] (host m) -> thing m
  overlay ::                                        [XYAlign]
             [offset] [thing m] (host m) -> thing m
  beside  ::                          [size]        [YAlign]
             [offset] [thing m] (host m) -> thing m
  above   ::                                 [size] [XAlign]
             [offset] [thing m] (host m) -> thing m
  grid    :: GridDimension GridLayout [size] [size] [XYAlign]
             [offset] [thing m] (host m) -> thing m

class Fit thing ~dim where
  fitXY   :: dim dim (thing m) -> thing m
  fitX    ::     dim (thing m) -> thing m
  fitY    ::     dim (thing m) -> thing m

class DimRef tag ~unit where
  xdim    :: tag -> unit
  ydim    :: tag -> unit
