import Lake
open Lake DSL

-- Add Mathlib as a git dependency
require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "master"

-- Configure the main package
package "vizagrams" where
  -- add package configuration options here

lean_lib «Vizagrams» where
  -- add library configuration options here

@[default_target]
lean_exe "vizagrams" where
  root := `Main
