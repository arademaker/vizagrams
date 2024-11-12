-- GraficalPrimitives.lean
namespace GraphicalPrimitives

-- 𝔾 Transformação de Grupo ?
class LinearTransform (𝔾 : Type) where
  translation : 𝔾 → Float × Float → 𝔾
  rotation : 𝔾 → Float → 𝔾

def norma (v : Float × Float) :=
  Float.sqrt ( v.1 * v.1 + v.2 * v.2 )
