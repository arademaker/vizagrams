import Vizagrams
import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad

/-- Estilo padrão para todas as curvas:
    - contorno azul (strokeColor),
    - preenchimento verde (fillColor),
    - espessura do contorno: 5px.
-/
def myStyle : Sty.Style :=
  { strokeColor := Color.mk 0 0 1
  , strokeWidth := Sty.StyleSize.px 5 }

def SliceStyle : Sty.Style :=
  { strokeColor := Color.mk 0 0 1
  , fillColor := Color.mk 0 1 0
  , strokeWidth := Sty.StyleSize.px 5 }

/-- Curva em forma de arco de elipse:
    - raios rx = 100, ry = 100
    - centro: (200, -300)
    - rotação do eixo maior: 0 rad
    - ângulo inicial: 0 rad
    - ângulo final: π rad
-/
def arc₁ : Geom :=
  .arc 100.0 100.0 ![200, -300] 0.0 0.0 π

def arc₂ : Prim :=
  { geom := arc₁, style := myStyle }

#html draw arc₂

def slice : Prim :=   { geom := arc₁, style := SliceStyle }
#html draw slice

/-- Curva Bézier quadrática com:
    - pontos base: (100, -100) → (300, -100)
    - ponto de controle: (200, 0)
    Essa configuração forma uma parábola suave ascendente.
-/
def qbez₁ : Geom :=
  .qbezier
    #[![50.0, -150.0], ![150.0, -150.0]]
    #[![100.0, -400.0]]

def qbez₂ : Prim :=
  { geom := qbez₁, style := myStyle }

#html draw qbez₂

def qbez₃ : Prim :=
  { geom := qbez₁, style := SliceStyle }
#html draw qbez₃
/-- Curva Bézier cúbica com:
    - pontos base: (100, -200) → (300, -200)
    - pontos de controle:
        • primeiro: (150, -100) — puxa o início para cima
        • segundo:  (250, -300) — puxa o final para baixo
-/
def cbez₁ : Geom :=
  .cbezier
    #[![100.0, -200.0], ![300.0, -200.0]]
    #[![150.0, -100.0], ![250.0, -300.0]]

def cbez₂ : Prim := { geom := cbez₁, style := myStyle }

#html draw cbez₂

def cbez₃ : Prim := { geom := cbez₁, style := SliceStyle }
#html draw cbez₃

def square₀ : Prim := NewPolygon #[![0.7,0.7],![-0.7,0.7],![-0.7,-0.7],![0.7,-0.7]] SliceStyle

#html draw ( cbez₃ + square₀)

def parabola_geom : Geom :=
  .qbezier
    #[![0.0,0.0], ![1.0,1.0]]
    #[![0.5,0.0]]

def circlt  : Prim := NewCircle 0.1 ![0,0]

-- Deslocamento Δx = 1.0
def dx := 50.0
def dy := 100.0
def parabola_shifted : Geom :=
  .qbezier
    -- P₀ = (0,0) passa a ser (0+dx, 0) = (1, 0)
    -- P₂ = (1,1) passa a ser (1+dx, 1) = (2, 1)
    #[![ 0.0 + dx,  0.0 - dy], 
      ![ 1.0 + dx,  1.0 -dy] ]
    -- P₁ = (0.5,0) passa a ser (0.5+dx, 0) = (1.5, 0)
    #[![ 0.5 + dx,  0.0 - dy] ]


def parabola_prim : Prim :=
  { geom := parabola_shifted, style := myStyle }

#html draw ( parabola_prim + circlt )

-- 2) Função que retorna um Geom de parábola, dadas as transformações:
def makeParabola
  (dx   : Float)  -- deslocamento em X
  (dy   : Float)  -- deslocamento em Y
  (sx   : Float)  -- escala em X
  (sy   : Float)  -- escala em Y
  : Geom :=
  -- Construímos o QBezier a partir de P₀=(0,0), P₁=(½,0), P₂=(1,1),
  -- mas aplicamos: (x',y') = ((x * sx) + dx, (y * sy) + dy).
  .qbezier
    -- P₀′ = ((0·sx)+dx, (0·sy)+dy) = (dx, dy)
    #[![ 0.0 * sx + dx,  0.0 * sy + dy ]
     -- P₂′ = ((1·sx)+dx, (1·sy)+dy) = (sx + dx, sy + dy)
     ,![ 1.0 * sx + dx,  1.0 * sy + dy ] ]
    -- P₁′ = ((½·sx)+dx, (0·sy)+dy) = (0.5*sx + dx, dy)
    #[![ 0.5 * sx + dx,  0.0 * sy + dy ] ]

-- 3) Função auxiliar que gera um Prim já com estilo:
def makeParabolaPrim
  (dx    : Float)
  (dy    : Float)
  (sx    : Float)
  (sy    : Float)
  (style : Sty.Style := myStyle)
  : Prim :=
  { geom := makeParabola dx dy sx sy
  , style := style
  }

def parabola_std : Prim :=
  makeParabolaPrim 0.0  0.0   1.0  1.0

#html draw parabola_std

def parabola_amplificada : Prim :=
  makeParabolaPrim 50.0  (-300.0)   100.0  100.0
#html draw (circlt + parabola_amplificada)

-- “Fábrica” de parábola simétrica em [-1,1], escalada e deslocada:
def makeParabolaSym
  (dx   : Float)  -- deslocamento em X
  (dy   : Float)  -- deslocamento em Y
  (sx   : Float)  -- escala em X
  (sy   : Float)  -- escala em Y
  : Geom :=
  .qbezier
    -- P₀′ = ( (-1)*sx + dx, (1)*sy + dy )
    #[![ -1.0 * sx + dx ,  1.0 * sy + dy ]
     -- P₂′ = ( ( 1)*sx + dx, (1)*sy + dy )
     ,![  1.0 * sx + dx ,  1.0 * sy + dy ] ]
    -- P₁′ = (  0 * sx + dx,  0 * sy + dy )
    #[![  0.0 * sx + dx ,  0.0 * sy + dy ] ]

def makeParabolaSymPrim
  (dx    : Float)
  (dy    : Float)
  (sx    : Float)
  (sy    : Float)
  (style : Sty.Style := myStyle)
  : Prim :=
  { geom := makeParabolaSym dx dy sx sy
  , style := style
  }

-- Exemplo de uso:
--  • quero a parábola y=x^2 em [-1,1] (sem escala e sem deslocamento):
def parabola_base : Prim := 
  makeParabolaSymPrim  0.0  0.0   1.0  1.0
#html draw parabola_base

--  • quero a mesma parábola, mas movida 2 unidades para a direita e 3 para cima:
def parabola_move : Prim :=
  makeParabolaSymPrim  2.0  3.0   1.0  1.0
#html draw parabola_move

--  • quero a mesma parábola, do trecho [-1,1], mas com “zoom” 50× em X e 20× em Y:
def parabola : Prim :=
  makeParabolaSymPrim  100.0  (-335.0)  20.0 20.0
#html draw ( circlt + parabola)
