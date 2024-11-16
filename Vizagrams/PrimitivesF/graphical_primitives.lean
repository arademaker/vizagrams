-- GraficalPrimitives.lean
namespace GraphicalPrimitives

class GeomPrim (𝔾svg : Type) (𝔾cov : Type) where
  ψ : 𝔾cov → 𝔾svg
  φ : 𝔾svg → 𝔾cov

-- Trasnformações Geométricas e de estilo
-- Type class Primitiva
-- Rotação, Escala Uniforme, Translação, Espelhar,  ( Transformações Conformes )

-- Definir
class GeometricPrimitive (𝔾 : Type) where
  draw : 𝔾 → Nat

class AfineTransforms (𝔾 : Type) where
  translation : 𝔾 → Float × Float → 𝔾
  rotation : 𝔾 → Float → 𝔾

def norma (v : Float × Float) :=
  Float.sqrt ( v.1 * v.1 + v.2 * v.2 )
