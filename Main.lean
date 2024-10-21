import Vizagrams


def test1 := (Primitive.circle.mk (12, 12) 4)
def test2 := (Primitive.rectangle.mk (10, 10) 30 30)
def test3 := (Primitive.ellipse.mk (75, 75) 20 5)
def test4 := (Primitive.line.mk (10, 110) (50, 150))
def test5 := (Primitive.polyline.mk [(60, 110), (65, 120), (70, 115), (75, 130), (80, 125), (85, 140), (90, 135), (95, 150), (100, 145)]) -- Polyline
def test6 := (Primitive.polygon.mk [(50, 160), (55, 180), (70, 180), (60, 190), (65, 205), (50, 195), (35, 205), (40, 190), (30, 180), (45, 180)]) -- Polígono

#eval SVG.Primitive.draw test1
#eval SVG.Primitive.draw test2
#eval SVG.Primitive.draw test3
#eval SVG.Primitive.draw test4
#eval SVG.Primitive.draw test5
#eval SVG.Primitive.draw test6
