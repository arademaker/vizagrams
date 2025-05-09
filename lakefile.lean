import Lake
open Lake DSL

-- Adiciona Mathlib como dependência via git
require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "master"

-- Configura o pacote principal
package "vizagrams" where
  -- add package configuration options here

lean_lib «Vizagrams» where
  -- add library configuration options here

@[default_target]
lean_exe "vizagrams" where
  root := `Main
