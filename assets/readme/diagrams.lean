import Vizagrams

/- # exemple 1 -/

-- Drawing a circle and a Square

def d₁ : GraphicalPrimitive.Prim := {
  geom := GeometricPrimitive.Geom.circle 1 ![0,0]
  style := {fillColor := ProofWidgets.Svg.Color.mk 0 0 0}
}

def d₂ : GraphicalPrimitive.Prim := {
  geom := GeometricPrimitive.Geom.rect ![0,0] 1 1
  style := {fillColor := ProofWidgets.Svg.Color.mk 0 0 0}
}

def fr : ProofWidgets.Svg.Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 10
  width  := 500
  height := 500

def d₃ := translate ![2,2] * d₂

#html VizBackend.draw
  (d₁ + ( translate ![2,0.5] * d₂ : GraphicalPrimitive.Prim) )
  fr

open VizBackend

def c₁ := NewCircle 1 ![0,0]
def c₂ := NewPolygon #[![0,0], ![1,0], ![1,1], ![0,1]]

#html draw (c₁ →[1] c₂ ) fr
