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
def circleₚtoright := t_r₁ * circleₚ
#check circleₚtoright -- circleₚtoright : Prim
-- podemos ver as diferenças
#html drawsvg circleₚ
#html drawsvg circleₚtoright
-- Vamos tentar verificar os objetos no mesmo frame
#check circleₚ ⊕ circleₚtoright -- circleₚ ⊕ circleₚtoright : Array Prim
def two_circles := circleₚ ⊕ circleₚtoright
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
def circleₘtoright := t_r₁ * circleₘ
def twoMarkCircles := circleₘtoright ⊕ circleₘ
#check twoMarkCircles -- twoMarkCircles : Array Prim
#html drawsvg twoMarkCircles
/-Isso nos mostra que o tipo Mark sozinho é "instável" pois quase todas as aplicações
em Mark retornam um Array Prim
No final veremos como funcionam as transformações em 𝕋 Mark
-/

-- Rotações
set_default_scalar Float
open SciLean Scalar RealScalar in
def g_45 : GeometricTransformation.G := GeometricTransformation.G.rotate (π/4)

-- Vamos precisar definir um novo objeto onde possamos ver os efeitos de rotação
def square₀ : Prim := NewPolygon #[⊞[0,0],⊞[1,0],⊞[1,1],⊞[0,1]]
#html drawsvg square₀
#check g_45 * square₀ -- g_45 * square₀ : Prim

def squareᵣ := g_45 * square₀
#html drawsvg squareᵣ

-- Da mesma forma que em translate, a operação quando aplicada em um Array Prim, é aplicada elemento a elemento
def square₁ : Prim := t_r₁ * square₀
def twoSquares := square₀ ⊕ square₁
#html drawsvg (g_45 * twoSquares)

/- Observe que na rotação vemos um efeito de translação no objeto,
isso ocorre pois o objeto é rotacionado ao redor da origem
`importante: Verificar operação de rotação`.
-/

-- Transformação de escala
def scale : GeometricTransformation.G := GeometricTransformation.G.scale 2
def bigSquare := scale * square₀
#html drawsvg bigSquare
#html drawsvg (scale * twoSquares)

-- Também podemos compor transformações
#html drawsvg ( scale ∘ g_45 ∘ t_r₁ * square₀ )

-- Trasformações de estilo
/- Em `Style.lean` temos
structure Style where
  strokeColor := (none : Option Color)
  strokeWidth := (none : Option StyleSize)
  fillColor   := (none : Option Color)
-/

def borderToblue : Sty.Style := {strokeColor := Color.mk 0 0 1 }
#html drawsvg (borderToblue * bigSquare)

def size : Sty.StyleSize := .px 10
def bigborder : Sty.Style := {strokeWidth := size}
#html drawsvg ( bigborder * (borderToblue * bigSquare))

def Toblue : Sty.Style := {fillColor := Color.mk 0 0 1 }
#html drawsvg (Toblue * bigSquare)
/- As operações de estilo são definidas utilizando rightOption

def rightOption {α : Type} (o1 : Option α) (o2 : Option α) : Option α :=
  match o2 with
  | none => o1
  | some a => some a

Portanto quando tentamos Toblue * bigSquare, nada acontece pois o criamos
utilizando NewPolygon que pre_seleciona fillColor
-/
def newSquare : Prim := {geom := Geom.polygon #[⊞[0,0], ⊞[2,0], ⊞[2,2], ⊞[0,2]] }
#eval newSquare
#html drawsvg newSquare -- Como NewSquare não possui fill, não é possível ver nada
#html drawsvg ( Toblue * newSquare )
-- Também podemos compor diversos estilos
def redborder : Sty.Style := {strokeColor := Color.mk 1 0 0}
#eval redborder.comp Toblue
#html drawsvg ((redborder.comp Toblue) * (bigborder * newSquare))
#html drawsvg ( g_45 * ((redborder.comp Toblue) * (bigborder * newSquare)))

/- Envelope e BoundingBox
O envelope é uma operação bastante interessante, ela pode ser utilizada para definirmos
o frame de nossos diagramas

O que é o envelope ?
Dado uma direção, o envelope de um diagrama naquela direção é a menor distância entre a origem e
a linha de separação ( Reta que divide o espaço em dois, um contendo o diagrame e o outro vazio)
-/
#check (square₀ : Geom) -- `instaciei Coe Prim Geom para esse exemplo `
#eval (envelope square₀ ⊞[1,1])
-- Para calcular o BoundingBox de um diagram, basta calcular o envelope em todas as direções
def BoundingBox_square₀ := boundingBoxPrim square₀
#html drawsvg square₀ (BoundingBox.toFrame BoundingBox_square₀ )
-- Vemos que o boundingbox de um square é ele próprio

def bb_s45 := boundingBoxPrim (g_45 * square₀)
#html drawsvg (g_45 * square₀) (BoundingBox.toFrame bb_s45)
-- Entretanto quando rotacionamos, agora ele não ocupa toda a caixa

-- Temos também um para Array Prim `redundante`
def bb_2s := boundingBoxPrims twoSquares
#html drawsvg twoSquares (BoundingBox.toFrame bb_2s)
def bb_2s45 := boundingBoxPrims (g_45 * twoSquares)
#html drawsvg (g_45 * twoSquares) (BoundingBox.toFrame bb_2s45) -- `não resolve problema de rotação`

-- Envelpe para posicionamento `ìmcompleto !`
-- Outro uso para o envelope é a capacidade posicionar um diagrama ao lado de outro
def h₁ : Float^[2] := ⊞[1,1]
def position := (envelope (scale * circleₚ) h₁) + (envelope (circleₚ) h₁)
/- A ideia é que ao saber o limite ao qual d₁ (um diagrama) se estende em uma direção, e o limite
ao qual d₂ se estende na direção oposta a inicial, sabemos onde desenhar nosso segundo diagrama
-/
#eval position
#html drawsvg ( (scale * circleₚ) ⊕ ( (GeometricTransformation.G.translate ⊞[ position , 0 ]) * circleₚ))
def diagrama₁ := (scale * circleₚ) ⊕ ( (GeometricTransformation.G.translate ⊞[ position ,0]) * circleₚ)
def bb_d := boundingBoxPrims diagrama₁
#html drawsvg (diagrama₁) (BoundingBox.toFrame bb_d)

/- Translação em 𝕋 Mark
Em FreeMonad, temos:
structure H where -- Tranformações Gráficas
  s : Style
  g : G

Isso rege as transformações em objetos do tipo 𝕋 Mark
-/
