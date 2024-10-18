import Vizagrams

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"


/- ideas -/

def test1 := (Primitive.circle.mk (12, 12) 4)
def test2 := (Primitive.rectangle.mk (12, 12) 5 6)

#eval SVG.Primitive.draw test2
