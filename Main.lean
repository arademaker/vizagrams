import Vizagrams
import Lean.Data.Xml
import Lean.Data.RBMap

open Lean (RBMap)
open Lean Xml
open Primitive

-- definndo circles
def c1 : circle Float := { center := (50.0, 50.0), radious := 5.0 }
def c2 : circle Float := { center := (0.0, 0.0), radious := 15.0 }

def rect : rectangle Float := { origin := (1.0,1.0), width := 10.0 , height := 10.0 }
#check PrimToSvg.primToSvg c1 -- Type Element

def positionedCircle := placeCircleNextTo (c1) (c2) (1.0 , 1.0)

#eval positionedCircle

def svgElement : Lean.Xml.Element :=
  Lean.Xml.Element.Element "svg"
    (RBMap.fromList [("xmlns", "http://www.w3.org/2000/svg"), ("width", "200"), ("height", "100")] compare)
    #[Content.Element (PrimToSvg.primToSvg c1), Content.Element (PrimToSvg.primToSvg rect)]

#eval svgElement
