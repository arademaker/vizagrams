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

/-- Curva Bézier quadrática com:
    - pontos base: (100, -100) → (300, -100)
    - ponto de controle: (200, 0)
    Essa configuração forma uma parábola suave ascendente.
-/
def qbez₁ : Geom :=
  .qbezier
    #[![100.0, -100.0], ![300.0, -100.0]]
    #[![200.0, 0.0]]

def qbez₂ : Prim :=
  { geom := qbez₁, style := myStyle }

#html draw qbez₂

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
