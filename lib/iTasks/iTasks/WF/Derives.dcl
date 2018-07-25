definition module iTasks.WF.Derives
/**
* This module provides derived instances for common types from StdEnv and Platform
* such that you don't have to derive them when you use these libraries.
*/
import iTasks.WF.Definition

from iTasks.WF.Combinators.Core import :: Action, :: TaskListItem, :: TaskListFilter, :: AttachmentStatus
from iTasks.Internal.IWorld import :: ClockParameter
from iTasks.SDS.Sources.System import :: TaskInstance

from Data.Either import :: Either
from Data.Error import :: MaybeError
from System.Time import :: Timestamp, :: Timespec

from Text.HTML import :: HtmlTag, :: HtmlAttr
from Text.HTML import :: SVGElt, :: SVGAttr, :: SVGAlign, :: SVGColor, :: SVGDefer, :: SVGFillOpacity, :: SVGFuncIRI, :: SVGLengthAdjust
from Text.HTML import :: SVGLengthUnit, :: SVGLineCap, :: SVGFillRule, :: SVGLineJoin, :: SVGMeetOrSlice, :: SVGStrokeMiterLimit, :: SVGPaint
from Text.HTML import :: SVGStrokeDashArray, :: SVGStrokeDashOffset, :: SVGStrokeWidth, :: SVGTransform, :: SVGZoomAndPan

//Common library types
derive JSONEncode		(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp
derive JSONDecode		(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp
derive gEq				(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp, JSONNode, (->), Dynamic
derive gDefault   HtmlAttr
derive gEditor    HtmlAttr
derive gText      HtmlAttr

derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan

derive gEditor    {}
derive gText      {}

//Common iTasks system types
derive class iTask TaskId, TaskListFilter, AttachmentStatus

derive JSONEncode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, Timespec, ClockParameter
derive JSONDecode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, Timespec, ClockParameter
derive gDefault			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ClockParameter
derive gEq				TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, Timespec, ClockParameter

derive gText	        TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gEditor			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, Timespec, ClockParameter

