import Lake
open Lake DSL

package "vizagrams" where
  -- add package configuration options here

lean_lib «Vizagrams» where
  -- add library configuration options here

@[default_target]
lean_exe "vizagrams" where
  root := `Main
