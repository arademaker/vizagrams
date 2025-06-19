-- # Here we will try to create a tree structure of nodes, where these `Nodes` can be created from a Type u
universe u v
structure Node where
  { a : Type v }
  val : a
#check List
#check (ULift.up 3 : ULift.{7} Nat )
def r := (ULift.up 3 : ULift.{7} Nat)

def t₁ : Node := Node.mk "t₁"
#check t₁

def Node.ulift (x : Node.{u}) : Node.{max u v} where
  a := ULift x.a
  val := ULift.up x.val

#check (ULift.up t₁ : ULift.{7} Node)
def t₇ := (ULift.up t₁ : ULift.{7} Node)
#check t₇

inductive Tree (a : Type u) where
  | pure : a → Tree a
  | comp : Tree a → Tree a → Tree a
deriving Repr
/-
def Tree.ulift (a : Tree.{u} α ) : Tree.{max u v} (ULift α) :=
  match a with
  | .pure x => .pure (ULift.up x )
  | .comp s t => .comp s.ulift t.ulift
-/
def Tree.ulift (a : Tree.{u+1} Node ) : Tree.{(max u v) + 1} (Node) :=
  match a with
  | .pure x => .pure x.ulift
  | .comp s t => .comp s.ulift t.ulift

instance : HAdd (Tree Node.{u}) (Tree Node.{v}) (Tree Node.{max u v}) where
  hAdd m n := Tree.comp m.ulift n.ulift

instance : HAdd Node.{u} (Tree Node.{v}) (Tree Node.{max u v}) where
  hAdd m n := Tree.comp (Tree.pure m.ulift) n.ulift

instance : HAdd (Tree Node.{u}) Node.{v} (Tree Node.{max u v}) where
  hAdd m n := Tree.comp m.ulift (Tree.pure n.ulift)

instance : HAdd Node.{u} Node.{v} (Tree Node.{max u v}) where
  hAdd m n := Tree.comp (Tree.pure m.ulift) (Tree.pure n.ulift)

-- We see that it is possible to add two nodes and obtain a tree
def x : Node := (Node.mk "Node₁")
def y : Node := (Node.mk "Node₂")
#check x + y

-- Now let's define a UnitGraph as one node
structure UnitGraph where
  val : Node

def z : Node := (Node.mk "Node₃")

def G : UnitGraph := {val := z}

-- # And if for some reason, I want to interpret my graph as a node, and add it into my tree of nodes?
def NodeG : Node := {val := G}
#check NodeG + NodeG

#check (x + NodeG)

/-
failed to synthesize
  HAdd Node Node ?m.1842

This error occurs because `x` is `Node : Type 1` and `NodeG` is `Node : Type 2`
-/

--#check ((ULift.up x : ULift.{2} Node ) + (ULift.up NodeG : ULift.{2} Node ) )
#check (ULift.up 3 : ULift.{7} Nat)
