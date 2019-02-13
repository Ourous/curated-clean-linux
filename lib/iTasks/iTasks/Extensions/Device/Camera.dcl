definition module iTasks.Extensions.Device.Camera

/**
 * This module provides tasks for device operation with the camera on an iTasks 
 * (node) server.
 */
 
import iTasks
import iTasks.Extensions.Picture.JPEG

/**
 * Take a picture with the device camera.
 */
takePicture :: Task (Maybe JPEGPicture)
