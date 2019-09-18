definition module iTasks.Extensions.ScaledEditor
from iTasks.UI.Editor import :: Editor
/**
* This module wraps any editor in a container.
* The editor gets a fixed size, but is scaled down or up with css transforms to always fit
* the container. This is useful for presentations and other applications where you need a
* predictable fixed UI size, but don't know the resolution of the display.
*/
scaledEditor :: Int Int (Editor a) -> Editor a
