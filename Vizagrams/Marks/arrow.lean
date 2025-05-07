import Vizagrams.VizMark
import Vizagrams.VizBackend
import Vizagrams.VizPrim
import Vizagrams.Transformations
import Vizagrams.Marks.RegularPolygon

open SciLean Scalar RealScalar
open GraphicalMark
open VizBackend
open GeometricPrimitive
open GraphicalPrimitive
open ProofWidgets Svg


/- Implementação do Julia

struct Arrow <: Mark
    pts::Vector
    headsize::Real
    headstyle::Union{S,G,H}
    headmark
    function Arrow(pts, headsize, headstyle, headmark)
        @assert length(pts) > 1 "Must have at least two points."
        @assert headsize ≥ 0 "Head size must be positive"
        return new(pts, headsize, headstyle, headmark)
    end
end

-/

structure Arrow  where
  pts : Float^[2]  -- Direção da Flecha
  -- headsize : Float  -- Tamanho da ponta
  -- headstyle : Style  -- Estilização da ponta
  -- headmarkValue : T  -- O valor da marca na ponta

instance : ToString Arrow where
    toString p := s!"l "

instance : MarkInterface Arrow  where
  θ a := arrow

instance : Coe Arrow Mark where
  coe m := Mark.mk m

def Arrow_o : Arrow := { pts := ⊞[0 , 0]}

#html drawsvg Arrow_o
