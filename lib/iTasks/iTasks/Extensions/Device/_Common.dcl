definition module iTasks.Extensions.Device._Common

/**
 * This module provides common tasks for device operation on an iTasks 
 * (node) server.
 */
 
import iTasks

/**
 * Perform an operation on the device using the TCP interface.
 *
 * @param Request
 * @param Close connection
 * @return Result from device.
 */
deviceRequest :: String (String -> Bool) -> Task String
