import Vizagrams.Geom

open GeometricPrimitive

def circ : Geom := .circle 2.0 ![3.0, 4.0]
#eval circ
#eval ϕ circ
#eval ψ (ϕ circ)

def ret : Geom := .rect ![1.0, 2.0] 3.0 4.0

#eval ϕ ret  -- deve retornar rect com corner = (1.0, 2.0), p = (4.0, 6.0)
#eval ψ (ϕ ret)  -- deve retornar .rect ![1.0, 2.0] 3.0 4.0

def g : Mat2Vec2 :=  scale 2 -- escala por 2

def circ2 := g * circ

#eval circ2  -- deve retornar um círculo com raio 2×2 = 4.0, centro (6.0, 8.0)

def poly : Geom := .polygon #[![0,0], ![1,0], ![1,1], ![0,1]]

#eval ϕ poly
#eval ψ (ϕ poly)

def txt : Geom := .text ![1.0, 1.0] "hello" 12.0
def txt₁ := g * txt

#eval txt₁ -- a posição deve ser escalada, mas texto e tamanho mantidos
