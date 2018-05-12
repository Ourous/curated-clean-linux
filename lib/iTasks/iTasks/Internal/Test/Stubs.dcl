definition module iTasks.Internal.Test.Stubs

from iTasks.Internal.IWorld import :: IWorld
from iTasks.UI.Editor import :: VSt

//TEST STUBS
toStubIWorld :: *World -> *IWorld

fromStubIWorld :: *IWorld -> *World

toStubVSt :: *IWorld -> *VSt

fromStubVSt :: *VSt -> *IWorld
