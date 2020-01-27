module VoiceLeading.IO.HorizontalBars where

import           Graphics.Rendering.Chart.Plot.Types
import           Graphics.Rendering.Chart.Plot.Bars
import           Graphics.Rendering.Chart.Geometry
                                         hiding ( x0
                                                , y0
                                                )
import           Graphics.Rendering.Chart.Drawing

import           Data.Colour                    ( opaque )
import           Data.Colour.Names

import           Control.Monad
import           Data.List                      ( nub
                                                , sort
                                                )

plotHBars :: BarsPlotValue x => PlotBars y x -> Plot x y
plotHBars p = Plot { _plot_render     = renderPlotHBars p
                   , _plot_legend     = _plot_legend vPlot
                   , _plot_all_points = allHBarPoints p
                   }
  where vPlot = plotBars p

allHBarPoints :: (BarsPlotValue x) => PlotBars y x -> ([x], [y])
allHBarPoints p = case _plot_bars_style p of
  BarsClustered -> (x0 : concat [ xs | (_, xs) <- pts ], [ y | (y, _) <- pts ])
  BarsStacked ->
    (x0 : concat [ stackH xs | (_, xs) <- pts ], [ y | (y, _) <- pts ])
 where
  pts = _plot_bars_values p
  x0  = _plot_bars_reference p

stackH :: (BarsPlotValue x) => [x] -> [x]
stackH = scanl1 barsAdd

renderPlotHBars
  :: (BarsPlotValue x) => PlotBars y x -> PointMapFn x y -> BackendProgram ()
renderPlotHBars p pmap = case _plot_bars_style p of
  BarsClustered -> forM_ vals clusteredBars
  BarsStacked   -> forM_ vals stackedBars
 where
  clusteredBars (y, xs) = do
    forM_ (zip3 [0, 1 ..] xs styles) $ \(i, x, (fstyle, _)) ->
      withFillStyle fstyle
        $   alignFillPath (barPath xref0 x (offset i) y)
        >>= fillPath
    forM_ (zip3 [0, 1 ..] xs styles) $ \(i, x, (_, mlstyle)) ->
      whenJust mlstyle $ \lstyle ->
        withLineStyle lstyle
          $   alignStrokePath (barPath xref0 x (offset i) y)
          >>= strokePath
    barLine y xs xref0

  offset = case _plot_bars_alignment p of
    BarsLeft     -> \i -> fromIntegral i * height
    BarsRight    -> \i -> fromIntegral (i - nys) * height
    BarsCentered -> \i -> fromIntegral (2 * i - nys) * height / 2

  stackedBars (y, xs) = do
    let x2s = zip (xref0 : stackH xs) (stackH xs)
    let ofs = case _plot_bars_alignment p of
          BarsLeft     -> 0
          BarsRight    -> -height
          BarsCentered -> -(height / 2)
    forM_ (zip x2s styles) $ \((x0, x1), (fstyle, _)) ->
      withFillStyle fstyle $ alignFillPath (barPath x0 x1 ofs y) >>= fillPath
    forM_ (zip x2s styles) $ \((x0, x1), (_, mlstyle)) ->
      whenJust mlstyle $ \lstyle ->
        withLineStyle lstyle
          $   alignStrokePath (barPath x0 x1 ofs y)
          >>= strokePath

  barPath x0 x1 yos y = do
    let (Point x' y') = pmap' (x1, y)
    let (Point x0' _) = pmap' (x0, y)
    rectPath (Rect (Point x0' (y' + yos)) (Point x' (y' + yos + height)))

  barLine y xs x0 = withLineStyle style $ strokePointPath [Point 0 y', pt]
   where
    xmin            = minimum (x0 : xs)
    pt@(Point _ y') = pmap' (xmin, y)
    style           = dashedLine 0.5 [2.0, 8.0] (opaque black)

  xref0  = _plot_bars_reference p
  vals   = _plot_bars_values p
  height = case _plot_bars_spacing p of
    BarsFixGap gap minw ->
      let w = max (minYInterval - gap) minw
      in  case _plot_bars_style p of
            BarsClustered -> w / fromIntegral nys
            BarsStacked   -> w
    BarsFixWidth width' -> width'
  styles = _plot_bars_item_styles p

  minYInterval =
    let diffs = zipWith (-) (tail mys) mys
    in  if null diffs then _plot_bars_singleton_width p else minimum diffs
   where
    ys  = snd (allHBarPoints p)
    mys = nub $ sort $ map mapY ys

  nys   = maximum [ length ys | (_, ys) <- vals ]

  pmap' = mapXY pmap
  mapY y = p_y (pmap' (barsReference, y))

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust _        _ = return ()
