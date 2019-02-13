implementation module iTasks.WF.Derives

import iTasks.WF.Definition
import iTasks.WF.Combinators.Core
import iTasks.UI.Editor.Common
import iTasks.SDS.Sources.System
import iTasks.Internal.IWorld

import Data.Either
import Data.Error
import System.Time
import Text.HTML
import Text.GenJSON
import StdArray 

gEq{|(->)|} _ _ fa fb		= False // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal

gEditor{|{}|} _ _ tjx fjx = emptyEditorWithDefaultInEnterMode_ (JSONEncode{|* -> *|} tjx) (JSONDecode{|* -> *|} fjx) {}
gText{|{}|} _ _ _ = []

derive gDefault   SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   HtmlAttr
derive gEditor    HtmlAttr
derive gText      HtmlAttr

derive JSONEncode		TaskValue, InstanceConstants, InstanceType, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, Timespec, ClockParameter
derive JSONDecode		TaskValue, InstanceConstants, InstanceType, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, Timespec, ClockParameter
derive gDefault			TaskValue, TaskListItem, InstanceConstants, InstanceType, InstanceProgress, ValueStatus, TaskInstance, Action, ClockParameter
derive gEq				TaskValue, InstanceConstants, InstanceProgress, InstanceType, ValueStatus, TaskInstance, TaskListItem, Action, Timespec, ClockParameter
derive gText	        TaskValue, InstanceConstants, InstanceType, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gEditor			TaskValue, InstanceConstants, InstanceType, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, Timespec, ClockParameter

derive class iTask TaskId, TaskListFilter, AttachmentStatus
