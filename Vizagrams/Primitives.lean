

namespace Primitive

structure circle (α : Type) where
 center : (α × α)
 radious : α
deriving Repr

structure rectangle (α : Type) where
 origin : (α × α)
 a : α
 b : α
deriving Repr

end Primitive
