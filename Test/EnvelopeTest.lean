/-
Test file to verify envelope and bounding box calculations
-/
import Vizagrams

open GeometricPrimitive
open Envelope
open LinearAlgebra

-- Test current text envelope implementation
def testText : Geom := .text ![0, 0] "Hello World" 1.0

-- Test other geometries for comparison
def testCircle : Geom := .circle 1.0 ![0, 0]
def testRect : Geom := .rect ![0, 0] 2.0 1.0
def testEllipse : Geom := .ellipse 2.0 1.0 ![0, 0]  -- rx=2, ry=1, center at origin

-- Test envelope calculations
#eval envelope testText ![1, 0]   -- Right direction
#eval envelope testText ![-1, 0]  -- Left direction
#eval envelope testText ![0, 1]   -- Up direction
#eval envelope testText ![0, -1]  -- Down direction

-- Compare with circle
#eval envelope testCircle ![1, 0]   -- Should be 1.0
#eval envelope testCircle ![-1, 0]  -- Should be -1.0

-- Compare with rectangle
#eval envelope testRect ![1, 0]   -- Should be 2.0
#eval envelope testRect ![-1, 0]  -- Should be 0.0
#eval envelope testRect ![0, 1]   -- Should be 1.0
#eval envelope testRect ![0, -1]  -- Should be 0.0

-- Test ellipse envelopes (should be: right=2, left=-2, up=1, down=-1)
#eval envelope testEllipse ![1, 0]   -- Should be 2.0
#eval envelope testEllipse ![-1, 0]  -- Should be -2.0
#eval envelope testEllipse ![0, 1]   -- Should be 1.0
#eval envelope testEllipse ![0, -1]  -- Should be -1.0

-- Test bounding boxes
#eval boundingBox testText
#eval boundingBox testCircle
#eval boundingBox testRect
#eval boundingBox testEllipse

-- Test with different text positions
def testTextOffCenter : Geom := .text ![2, 1] "Test" 0.5

#eval boundingBox testTextOffCenter