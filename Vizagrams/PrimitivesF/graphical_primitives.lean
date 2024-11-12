-- GraficalPrimitives.lean
namespace GraphicalPrimitives

-- 𝔾 Transformação de Grupo ?
class LinearTransform (𝔾 : Type) where
  translation : 𝔾 → Float × Float → 𝔾
  rotation : 𝔾 → Float → 𝔾
