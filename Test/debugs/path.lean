import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad

#eval "Hello World !"

def path : GeometricPrimitive.Geom :=
  .path "M 20 30 Q 40 5 60 30 "--T 100 30"

def pathPrim : GraphicalPrimitive.Prim :=
  { geom := path,
    style :=
      { strokeColor := Color.mk 0 0 1,
        fillColor := Color.mk 0 1 0,
        strokeWidth := Sty.StyleSize.px 5}}

def ℙ : 𝕋 Mark := pathPrim

#html draw ℙ

def 𝕡 : GeometricPrimitive.Geom :=
  .cbezier #[![100, 200], ![100 , 100]] #[![400, 100], ![400,200]]

def quadratic : GraphicalPrimitive.Prim :=
  { geom := 𝕡 ,
    style := {  strokeColor := Color.mk 0 0 1,
                fillColor := Color.mk 0 1 0,
                strokeWidth := Sty.StyleSize.px 5}}

#html draw quadratic

def xsquare : Prim := NewQBezier ![0 ,350] {fst:= ![100,200] , snd := ![200,350]}
                                  { strokeColor := Color.mk 0 0 1,
                                    fillColor := Color.mk 0 1 0,
                                    strokeWidth := Sty.StyleSize.px 5}

#html draw xsquare
