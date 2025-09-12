import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad

-- Assumindo que o código do DataFrame está disponível
-- (incluindo todas as definições de CellValue, DataFrame, etc.)

-- Exemplo de DataFrame com dados numéricos
def sampleDataFrame : DataFrame :=
  let df := DataFrame.empty
  let df := df.addColumn "values" [
    CellValue.float 1.0,
    CellValue.float 2.3,
    CellValue.float 0.7,
    CellValue.float 3.1,
    CellValue.float 0.3
  ]
  let df := df.addColumn "labels" [
    CellValue.string "A",
    CellValue.string "B",
    CellValue.string "C",
    CellValue.string "D",
    CellValue.string "E"
  ]
  df

-- Helper function: Extrair valores Float de uma coluna do DataFrame
def extractFloatColumn (df : DataFrame) (colName : String) : List Float :=
  match df.getColumn colName with
  | none => []
  | some col => col.filterMap (fun cv =>
    match cv with
    | CellValue.float f => some f
    | CellValue.int i => some (DataFrame.intToFloat i)
    | _ => none
  )

-- Helper function: Extrair valores String de uma coluna do DataFrame
def extractStringColumn (df : DataFrame) (colName : String) : List String :=
  match df.getColumn colName with
  | none => []
  | some col => col.filterMap (fun cv =>
    match cv with
    | CellValue.string s => some s
    | _ => none
  )

-- Coalgebra adaptada para DataFrame
def coalgbarDF (df : DataFrame) (valueCol : String) : List (List Float) :=
  let values := extractFloatColumn df valueCol
  values.map (fun x => [x])

-- Mantemos as mesmas funções do exemplo original
def barPolygon (h : Float) (w : Float := 0.8) : Prim :=
  let pts : Array Vec2 :=
    #[![0, 0], ![w, 0], ![w, h], ![0, h]]
  NewPolygon pts {fillColor := Color.mk (3*h/2) (2*h/3) (h/5) }

def barExpr : List Float → 𝕋 Mark
  | [h] => 𝕋.pure (barPolygon h)
  | _   => 𝕋.pure (NewCircle 0 ![0,0])

def algBar (τ : List (𝕋 Mark)) : 𝕋 Mark :=
  match τ with
  | x :: xs => List.foldl (· →[0.5] ·) x xs
  | [] => 𝕋.pure (NewCircle 0 ![0,0])

-- Nova estrutura GraphicExpression para DataFrame
structure DataFrameGraphicExpression (α : Type) where
  expr : List Float → 𝕋 Mark
  coalg : α → String → List (List Float)
  alg : List (𝕋 Mark) → 𝕋 Mark

-- Função de avaliação para DataFrame
def DataFrameGraphicExpression.eval (ge : DataFrameGraphicExpression DataFrame)
  (df : DataFrame) (valueCol : String) : 𝕋 Mark :=
  let decomposed := ge.coalg df valueCol
  let marks := decomposed.map ge.expr
  ge.alg marks

-- Definição do bar chart para DataFrame
def dataFrameBars : DataFrameGraphicExpression DataFrame := {
  expr := barExpr
  coalg := coalgbarDF
  alg := algBar
}

-- Exemplo de uso
#check dataFrameBars.eval sampleDataFrame "values"

-- Para renderizar:
-- #html draw (dataFrameBars.eval sampleDataFrame "values")

-- Versão mais avançada que inclui labels
def barPolygonWithLabel (h : Float) (label : String) (w : Float := 0.8) : Prim :=
  let pts : Array Vec2 :=
    #[![0, 0], ![w, 0], ![w, h], ![0, h]]
  -- Por simplicidade, mantemos apenas o polígono
  -- Em uma implementação completa, adicionaríamos o texto do label
  NewPolygon pts {fillColor := Color.mk (3*h/2) (2*h/3) (h/5) }

-- Coalgebra que extrai pares (valor, label)
def coalgbarDFWithLabels (df : DataFrame) (valueCol : String) (labelCol : String) : List (List (Float × String)) :=
  let values := extractFloatColumn df valueCol
  let labels := extractStringColumn df labelCol
  let pairs := values.zip labels
  pairs.map (fun pair => [pair])

-- Expressão que trabalha com pares (valor, label)
def barExprWithLabel : List (Float × String) → 𝕋 Mark
  | [(h, label)] => 𝕋.pure (barPolygonWithLabel h label)
  | _            => 𝕋.pure (NewCircle 0 ![0,0])

-- Estrutura mais avançada para trabalhar com múltiplas colunas
structure DataFrameGraphicExpressionPairs (α : Type) where
  expr : List (Float × String) → 𝕋 Mark
  coalg : α → String → String → List (List (Float × String))
  alg : List (𝕋 Mark) → 𝕋 Mark

def DataFrameGraphicExpressionPairs.eval (ge : DataFrameGraphicExpressionPairs DataFrame)
  (df : DataFrame) (valueCol : String) (labelCol : String) : 𝕋 Mark :=
  let decomposed := ge.coalg df valueCol labelCol
  let marks := decomposed.map ge.expr
  ge.alg marks

-- Bar chart com labels
def dataFrameBarsWithLabels : DataFrameGraphicExpressionPairs DataFrame := {
  expr := barExprWithLabel
  coalg := coalgbarDFWithLabels
  alg := algBar
}

-- Exemplo de uso com labels
#check dataFrameBarsWithLabels.eval sampleDataFrame "values" "labels"

-- Para renderizar:
#html draw (dataFrameBarsWithLabels.eval sampleDataFrame "values" "labels")
