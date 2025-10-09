import Vizagrams.Style

open Sty ProofWidgets.Svg

def defaultStyle : Style := default
#eval defaultStyle

def fillRed : Style := {fillColor := Color.mk 1 0 0}
#eval fillRed

def fillAndStroke : Style := {strokeColor := Color.mk 0 0 1 , fillColor := Color.mk 0 1 0}

#eval (fillRed ++ fillAndStroke)
#eval (fillAndStroke ++ fillRed )

private def testFrame : Frame where
  xmin   := -100
  ymin   := -100
  xSize  := 200
  width  := 200
  height := 200

#eval testFrame


def pxSize : StyleSize := .px 15
#eval pxSize

def absSize : StyleSize := .abs 0.8
#eval absSize

#eval (toSvgSize pxSize testFrame)
#eval (toSvgSize absSize testFrame)

def someSize : Option StyleSize := some (.px 25)
def noSize : Option StyleSize := none

#check (styleToSvgSize someSize testFrame)
#check (styleToSvgSize noSize testFrame)
