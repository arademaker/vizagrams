import Vizagrams.FreeMonad
import Vizagrams.VizBackend

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
def circleₚ : Prim := NewCircle 1 ![0,0] -- Utilizamos NewCircle de VizBackend
#eval circleₚ
/-
{ geom := Geom.circle 1.000000 ![0.000000, 0.000000],
  s := Style.mk { strokeColor := none, strokeWidth := none, fillColor := (0.000000, 0.000000, 0.000000) } }
-/
-- Vamos verificar que circleₚ pode ser considerado um Array Prim
#check (circleₚ : Array Prim) -- #[circleₚ] : Array Prim

def circleₐ : Array Prim := circleₚ
#eval circleₐ
/-
#[{ geom := Geom.circle 1.000000 ![0.000000, 0.000000],
    s := Style.mk { strokeColor := none,
                    strokeWidth := none,
                    fillColor := (0.000000, 0.000000, 0.000000) } }]
-/

-- E como uma Mark ?
#check ( circleₚ : Mark ) -- Mark.mk circleₚ : Mark
def circleₘ : Mark := circleₚ
-- #eval circleₘ -- Alterar toString de Prim
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
def t_r₁ : Mat2Vec2 := translate ![3,0] -- três unidade para a direita

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

def g_45 : Mat2Vec2 := rotate (π/4)

-- Vamos precisar definir um novo objeto onde possamos ver os efeitos de rotação
def square₀ : Prim := NewPolygon #[![0.7,0.7],![-0.7,0.7],![-0.7,-0.7],![0.7,-0.7]]
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
def scale2 : Mat2Vec2 := scale 2
def bigSquare := scale2 * square₀
#html drawsvg bigSquare
#html drawsvg (scale2 * twoSquares)

-- Também podemos compor transformações
#html drawsvg ( (scale2 ∘ g_45 ∘ t_r₁) * square₀ )

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
def newSquare : Prim := {geom := Geom.polygon #[![0,0], ![2,0], ![2,2], ![0,2]] , style := {}}
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
open Envelope
#check (square₀ : Geom) -- `instaciei Coe Prim Geom para esse exemplo `
#eval (envelope square₀ ![1,1])
-- Para calcular o BoundingBox de um diagram, basta calcular o envelope em todas as direções
def BoundingBox_square₀ := boundingBoxPrim square₀
#check BoundingBox_square₀
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

-- Outro uso para o envelope é a capacidade posicionar um diagrama ao lado de outro
def h₁ : Vec2 := normalize ![0,10]

-- Embora a função envelope já normalize os vetores, a translação tem de usar o vetor normalizado
def limite_d₁ : Float := envelope (scale2 * circleₚ) h₁
-- Calculamos o quanto o primeiro diagrama ( O que está fixo ) se estende na direção h₁
def limite_d₂ : Float := envelope (circleₚ) (-h₁)
-- Calculamos o quanto o segundo diagrama ( O que desejamos posicionar ) se estende na direção oposta a h₁
def offset_h₁ : Float := limite_d₁ + limite_d₂
/-
Para garantir que D₂ fique “colado” em D₁ sem sobreposição, basta deslocá-lo em v por uma distância igual a
Editar
offset = d1 + d2
Assim, a face mais próxima de D₂ (na direção –v) encosta exatamente na face mais avançada de D₁ (na direção v).-/
def position := ScalarMul offset_h₁ h₁

#eval position
#html drawsvg ( (scale2 * circleₚ) ⊕ ( (translate position) * circleₚ))
def diagrama₁ := ((scale2 * circleₚ) ⊕ ( (translate position) * circleₚ))
#check diagrama₁
#eval diagrama₁
def bb_d := boundingBoxPrims diagrama₁
#html drawsvg (diagrama₁) (BoundingBox.toFrame bb_d)

/- # Desenvolvimento: Posicionamento por envelope
↑ ← → ↓
-/
def circleₚpositioned := envelopePositionPrim circleₚ ![1,1] circleₚ
#html drawsvg ( circleₚ ⊕ circleₚpositioned )

#html drawsvg ( circleₚ → circleₚ → circleₚ )
#html drawsvg ( circleₚ → circleₚ ↑ square₀)
-- Também podemos posicionar objetos com as operações ↑ ↓ → ← adicionando um espaçamento
#html drawsvg ( circleₚ →[0.5] circleₚ →[0.5] circleₚ )

def stackCircles : Nat → Float → Array Prim
| 0,      _   => #[]                         -- zero círculos
| Nat.succ n, gap =>
  let prev := stackCircles n gap
  if prev.isEmpty then
    -- primeiro círculo
    #[circleₚ]
  else
    -- empilha mais um após o que já existe
    prev →[gap] #[circleₚ]

-- EXEMPLOS

#html drawsvg (circleₚ →[0.5] circleₚ →[0.5] circleₚ)
-- exatamente três círculos com gap = 0.5

def d := stackCircles 5 0.5   -- cinco círculos em fila, gap = 0.5
#html drawsvg d

-- 1) Constantes geométricas do triângulo equilátero de lado 1
def h : Float :=  (3 / 2 : Float).sqrt

-- 2) Triângulo-base como Prim
def triₚ : Prim := NewPolygon #[![0,0], ![1,0], ![0.5,h]]

/--
  Recursão que, para n = 0, retorna o triângulo único;
  para n+1,k produz três cópias de ordem n escaladas a ½,
  deslocadas para (0,0), (0.5,0) e (0.25,h/2).
-/
def sierpinskiPrims : Nat → Array Prim
| 0   => #[triₚ]
| n+1 =>
  let prev := sierpinskiPrims n
  -- escala tudo em 0.5
  let scaled := prev.map (fun p => { geom := scale 0.5 * p.geom, style := p.style })
  -- três posições
  let t1 := scaled
  let t2 := scaled.map (fun p => { geom := translate ![0.5,0]  * p.geom, style := p.style })
  let t3 := scaled.map (fun p => { geom := translate ![0.25,h/2] * p.geom, style := p.style })
  -- concatena
  t1 ++ t2 ++ t3

#html drawsvg  (sierpinskiPrims 4) (BoundingBox.toFrame (boundingBoxPrims (sierpinskiPrims 4)))

/- Translação em 𝕋 Mark
Em FreeMonad, temos:
structure H where -- Tranformações Gráficas
  s : Style
  g : G

Isso rege as transformações em objetos do tipo 𝕋 Mark
-/

-- Vamos voltar a olhar para os objetos do tipo 𝕋 Mark
#check 𝕋circle
#html draw 𝕋circle

-- Como aplicar uma transformação em 𝕋circle ?
def 𝕋translation (x : Vec2) : FreeMonad.ℍ := { s := {} , g := translate x}
def x : Vec2:= ![2,0]
#html draw ( 𝕋translation x * 𝕋circle )
#check ( 𝕋translation x * 𝕋circle )
/- # O que estamos fazendo ?
Intuitivamente, compor marcas é como criar uma arvore
inicialmente temos:

        circle

Mas após a operação 𝕋translation

    H
      \
        circle


def eval (t : FreeMonad.𝕋 Mark) : Array Prim :=
  FreeMonad.algθ (FreeMonad.𝕋.map Mark.θ t)

#eval eval ( 𝕋translation x * 𝕋circle )
-/
def twoCircles : FreeMonad.𝕋 Mark := 𝕋circle + ( 𝕋translation x * 𝕋circle )
#html draw twoCircles
/- # Arvore de twoCircles

      𝕋.comp
    /        \
𝕋circle     𝕋.act H
                \
              𝕋circle
-/
-- Da mesma forma, podemos verificar as outras transformações geométricas
def 𝕋rotate (y : Float) : FreeMonad.ℍ := { s := {} , g := rotate y}
def 𝕋scale (z : Float) : FreeMonad.ℍ := { s := {} , g := scale z}

def 𝕋square : FreeMonad.𝕋 Mark := square₀

#html draw ( 𝕋rotate (π/4) * 𝕋square)
#html draw (𝕋scale 0.3 * 𝕋circle)

-- Vemos também transformações de estilo
/- # Transformações de estilo em Marks
Aqui ainda utilizamos
instance : Mul H where
  mul x y := H.mk (Style.comp x.s y.s) ( x.g )
`Style.comp` para combinar os estilos, portanto permanece o rightOption
-/
def 𝕋style (w : Sty.Style) : FreeMonad.ℍ := { s := w , g := scale 1}
def myStyle : Sty.Style := {strokeColor := Color.mk 0 0 1 , fillColor := Color.mk 1 1 0}

#html draw (𝕋style borderToblue * 𝕋square)
#html draw (𝕋style borderToblue * 𝕋style bigborder * 𝕋square)

/- # Envelopes e BoundingBox -/
def blueBorderSquare : FreeMonad.𝕋 Mark := (𝕋style borderToblue * 𝕋style bigborder * 𝕋square)

/- Da mesma forma que em Prim e Array Prim, podemos utilizar uma função para calcular o
boundingbox e então converter para frame -/
def bb_m₁ := boundingBox𝕋 blueBorderSquare
def bb_m₂ := boundingBox𝕋 twoCircles
#html draw blueBorderSquare (BoundingBox.toFrame bb_m₁)
#html draw twoCircles (BoundingBox.toFrame bb_m₂)

/- Também temos os posicionamentos por envelope -/
#html draw ( twoCircles + envelopePositionMarks twoCircles ![0,1] twoCircles)
#html draw (twoCircles → twoCircles ↑ twoCircles )
#html draw (twoCircles ↑ 𝕋square)

-- Posicionando com espaçamento
def bb_m₃ := boundingBox𝕋 ( twoCircles ↑[0.5] twoCircles)
#html draw ( twoCircles ↑[0.5] twoCircles) (BoundingBox.toFrame bb_m₃)

/- # Criando Marks
Uma funcionalidade bastante útil e agradável é a capacidade de criar novos objetos gráficos.
Para isso podemos observar o próprio código de Mark

class MarkInterface (a : Type) where
  θ : a -> Array Prim

structure Mark where
  {T : Type}
  [inst : MarkInterface T]
  [strg : ToString T]
  val : T

Uma Mark é um tipo que instancia MarkInterface
-/

-- Aqui definimos um Type
structure sierpinski where
  n : Nat

instance : ToString sierpinski where
  toString s := s!" ssierpinski {s.n}"
-- Agora criamos uma instancia de MarkInterface
instance : MarkInterface sierpinski where
  θ s := sierpinskiPrims s.n

def Sierpinski₃ : sierpinski := {n := 3}

instance : Coe sierpinski Mark where
  coe m := Mark.mk m

#html draw₁ Sierpinski₃

def TT : FreeMonad.ℍ := {s := {}, g := translate ![3,0]}
#html draw (TT * (Sierpinski₃: FreeMonad.𝕋 Mark))

-- # Poligonos Regulares

structure RegularPolygon where
  center : Vec2 := ![0,0]
  sides : Nat
  size : Float
  h : sides >= 3 := by decide
  style : Sty.Style

instance : ToString RegularPolygon where
  toString p := s!"Regular Polygon with {p.sides} sides of size {p.size}"

-- Agora precisamos implementar MarkInterface
def findPointbyAngle (x : Float) : Vec2 :=
  ![Float.cos x , Float.sin x]

def create_list (n : ℕ) : Array ℕ :=
  Array.range n |>.map (λ x => x + 1)

def multiply_by_scalar (lst : Array ℕ) (scalar : Float) : Array Float :=
  lst.map (λ x => ↑x.toFloat * scalar)

def regToPoly (p : RegularPolygon) : Array (Vec2) :=
  let passo : Float := ( 2 * π  ) / (p.sides.toFloat)
  let listn : Array ℕ := create_list (p.sides)
  let Arr := multiply_by_scalar listn passo
  Array.map findPointbyAngle Arr

instance : MarkInterface RegularPolygon where
  θ h := NewPolygon (regToPoly h) h.style

instance : Coe RegularPolygon Mark where
  coe m := Mark.mk m

def triangle : RegularPolygon := {sides := 3 , size := 1, style := Toblue }
def triangleₘ : FreeMonad.𝕋 Mark := triangle
#check (triangle : FreeMonad.𝕋 Mark)
#html draw (𝕋rotate (π/4) * triangleₘ)
#html draw (𝕋style redborder * (𝕋style bigborder * triangleₘ))

-- Podemos criar Marks usando Marks

structure Arrow where
  p₁ : Vec2
  p₂ : Vec2
  tip : Mark
  style : Sty.Style

instance : ToString Arrow where
  toString a := s!"Arrow"

def ArrowLine (α : Arrow) : Prim := NewLine α.p₁ α.p₂ α.style

instance : MarkInterface Arrow where
  θ α :=
    let c₁ : Mark := ArrowLine α
    let c₂ :  Mark := α.tip
    let m : FreeMonad.𝕋 Mark := envelopePositionMarks c₁ (α.p₂ - α.p₁) c₂
    FreeMonad.flat m

instance : Coe Arrow Mark where
  coe m := Mark.mk m

def Arrow₁ : Arrow := {p₁ := ![0,0], p₂ := ![1,0], tip := triangle, style := bigborder.comp borderToblue}
#eval Arrow₁.p₂ - Arrow₁.p₁
#html draw Arrow₁
-- 