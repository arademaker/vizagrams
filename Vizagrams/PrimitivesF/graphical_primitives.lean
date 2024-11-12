-- GraficalPrimitives.lean
namespace GraphicalPrimitives

class LinearTransform (𝔾 : Type) where
  translation : 𝔾 → Float × Float  → 𝔾
  rotation : 𝔾 → Float → 𝔾
