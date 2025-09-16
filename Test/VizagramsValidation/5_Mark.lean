import Vizagrams.Mark

def circle₀ : GeometricPrimitive.Geom := .circle 1 ![0,0]

#eval circle₀

#eval GraphicalPrimitive.mk circle₀

def circle₁ : GraphicalPrimitive.Prim := GraphicalPrimitive.mk circle₀

#check (circle₁ : GraphicalMark.Mark)

def MarkCircle : GraphicalMark.Mark := circle₁

#eval MarkCircle
