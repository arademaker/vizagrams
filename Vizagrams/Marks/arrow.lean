import Vizagrams.VizMark

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

structure Arrow where
  pts : Float^[2]
  headsize : Float
  headstyle : Style
  headmark : Mark
/-
instance : MarkInterface Arrow where
  θ h :=
    let l :=
-/
