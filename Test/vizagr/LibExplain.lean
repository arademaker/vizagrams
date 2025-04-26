import Vizagrams.VizPrim
import Vizagrams.VizBackend
import Vizagrams.VizMark
import Vizagrams.FreeMonad

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark

/- 1. Entendendo os objetos gráficos -/

/-
Temos 4 tipos diferentes de objetos desenhaveis

· Prim ( Representa Primitivas)
· Array Prim ( Representa uma "lista ordenada" de primitivas)
· Mark ( Type que possui uma instancia de MarkInteface θ : Type → Array Prim)
· 𝕋 Mark ( Free Monad em Endofuctors Mark )

Vamos observar como esses objetos se relacionam
-/

-- Vamos desenhar um Círculo como uma primitiva
def circleₚ : Prim := NewCircle 1 ⊞[0,0] -- Utilizamos NewCircle de VizBackend
#eval circleₚ
/-
{ geom := Geom.circle 1.000000 ⊞[0.000000, 0.000000],
  s := Style.mk { strokeColor := none, strokeWidth := none, fillColor := (0.000000, 0.000000, 0.000000) } }
-/
-- Vamos verificar que circleₚ pode ser considerado um Array Prim
#check (circleₚ : Array Prim) -- #[circleₚ] : Array Prim

def circleₐ : Array Prim := circleₚ
#eval circleₐ
/-
#[{ geom := Geom.circle 1.000000 ⊞[0.000000, 0.000000],
    s := Style.mk { strokeColor := none,
                    strokeWidth := none,
                    fillColor := (0.000000, 0.000000, 0.000000) } }]
-/

-- E como uma Mark ?
#check ( circleₚ : Mark ) -- Mark.mk circleₚ : Mark
def circleₘ : Mark := circleₚ
#eval circleₘ -- Alterar toString de Prim
/- Podemos fazer uma coerção Prim → Mark, mas ainda não podemos fazer Array Prim → Mark
Verificar se faz sentido pensar em uma coe Array Prim → Mark pois o usuário trabalhará
apenas com objetos do tipo Mark ou 𝕋 Mark
-/

-- E de circle para 𝕋 Mark ?
#check (circleₚ : FreeMonad.𝕋 Mark) --FreeMonad.𝕋.pure (Mark.mk circleₚ) : FreeMonad.𝕋 Mark
/- Aqui podemos ver incusive como é essa transformação de Prim → 𝕋 Mark
Primeiro o objeto Prim é transformado em uma Mark e depois é utilizado o pure
| pure : α → 𝕋 α

De forma simples uma Monad é uma solução para conseguir compor funções que retornam
valores com efeitos

class Monad (m : Type → Type) where
  pure : α → m α -- aplica o efeito a um valor puro
  bind : m α → (α → m β) → m β -- ajuda a compor funções que retornam valores com efeito

Podemos interpretar nossa 𝕋 Mark como uma árvore, e a operação pure é a que pega
um Mark e adiciona a uma folha
-/

#check (circleₘ : FreeMonad.𝕋 Mark) -- FreeMonad.𝕋.pure circleₘ : FreeMonad.𝕋 Mark
-- Aqui ocorre o mesmo, só que não precisamos transformar circleₘ em Mark

def 𝕋circle : FreeMonad.𝕋 Mark := circleₘ

/- Vamos agora buscar desenhar nossos circles
Utilizamos para isso a ProofWidgets, em VizBackend temos duas funções de desenho
· drawsvg (Array Prim) (frame)
· draw (𝕋 Mark) (frame)
(obs): Aqui seria interesante discutir sobre a necessidade de drawsvg
-/

#html drawsvg circleₚ
#html draw circleₚ

/- Como temos coerções de Prim → Mark e de Mark → 𝕋 Mark, tanto drawsvg
quanto draw funcionam em prim, entretanto draw não funcionaria em um ArrayPrim
#html draw circleₐ
application type mismatch
  @draw circleₐ
argument
  circleₐ
has type
  Array Prim : Type
but is expected to have type
  FreeMonad.𝕋 Mark : Type 1
-/
#html draw circleₘ
#html draw 𝕋circle
/- Agora que temos como visualizar nossos "desenhos", vamos observar algumas
operações que temos disponíveis
· Translação
· Rotação
· Transformações de estilo
· Envelope e BoundBox
-/

-- Translação em Prim
def t_r₁ : GeometricTransformation.G := GeometricTransformation.G.translate ⊞[3,0] -- três unidade para a direita

-- Aplicamos transformações a esquerda do objeto Prim
#eval t_r₁ * circleₚ
def circleₚtoRigth := t_r₁ * circleₚ
#check circleₚtoRigth -- circleₚtoRigth : Prim
-- podemos ver as diferenças
#html drawsvg circleₚ
#html drawsvg circleₚtoRigth
-- Vamos tentar verificar os objetos no mesmo frame
#check circleₚ ⊕ circleₚtoRigth -- circleₚ ⊕ circleₚtoRigth : Array Prim
def two_circles := circleₚ ⊕ circleₚtoRigth
#html drawsvg two_circles
-- Entretanto não é possível  observá-los com draw, pois não temos coe Array Prim → 𝕋 Mark

-- Translação em Array Prim
#eval t_r₁ * two_circles
#html drawsvg ( t_r₁ * two_circles )
/- Aplicar uma translação em um Array Prim significa aplicá-la a cada uma das componentes
  t * #[p₁ , p₂] = #[t * p₁ , t * p₂]
-/

-- Translação em Mark
#check ( t_r₁ * circleₘ ) -- t_r₁ * circleₘ : Array Prim
/- Quando aplicamos uma translação em um Mark, ele é convertido para Array Prim
instance : HMul G Mark (Array Prim) where
  hMul g M  := g * M.θ
-/
#html drawsvg ( t_r₁ * circleₘ )
def circleₘtoRigth := t_r₁ * circleₘ
def twoMarkCircles := circleₘtoRigth ⊕ circleₘ
#check twoMarkCircles -- twoMarkCircles : Array Prim
#html drawsvg twoMarkCircles
/-Isso nos mostra que o tipo Mark sozinho é "instável" pois quase todas as aplicações
em Mark retornam um Array Prim
No final veremos como funcionam as transformações em 𝕋 Mark
-/

-- Rotações


/- Translação em 𝕋 Mark
Em FreeMonad, temos:
structure H where -- Tranformações Gráficas
  s : Style
  g : G

Isso rege as transformações em objetos do tipo 𝕋 Mark
-/
