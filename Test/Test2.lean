import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad

-- Assumindo que o código do DataFrame está disponível
-- (incluindo todas as definições de CellValue, DataFrame, etc.)

/-
## BarPlot: Rotina Completa para Visualização
Esta seção implementa uma interface completa para criar bar charts a partir de dados CSV
-/

namespace BarPlot

-- ============================================================================
-- FUNÇÕES AUXILIARES PARA FLOAT
-- ============================================================================

-- Implementação de Float.max (não existe no Lean 4 padrão)
private def floatMax (a b : Float) : Float :=
  if a >= b then a else b

-- Implementação de Float.min (não existe no Lean 4 padrão)
private def floatMin (a b : Float) : Float :=
  if a <= b then a else b

-- Comparação de igualdade para Float
private def floatEq (a b : Float) : Bool :=
  -- Usamos uma tolerância pequena para comparação de floats
  let diff := if a >= b then a - b else b - a
  diff < 0.0001

-- ============================================================================
-- CONFIGURAÇÕES DO BARPLOT
-- ============================================================================

-- Estrutura para configurar o BarPlot
structure Config where
  barWidth : Float := 0.8
  spacing : Float := 0.5
  colorIntensity : Float := 1.0
  maxBarHeight : Float := 3.0
  showLabels : Bool := true
deriving Repr

-- Configuração padrão
def defaultConfig : Config := {}

-- ============================================================================
-- FUNÇÕES DE RENDERIZAÇÃO
-- ============================================================================

-- Função para normalizar valores (escalar para altura máxima)
private def normalizeValues (values : List Float) (maxHeight : Float) : List Float :=
  if values.isEmpty then []
  else
    let maxVal := values.foldl floatMax 0.0
    if floatEq maxVal 0.0 then values.map (fun _ => 0.0)
    else values.map (fun v => (v / maxVal) * maxHeight)

-- Criar um polígono de barra com configuração personalizada
private def createBar (height : Float) (config : Config) : Prim :=
  let w := config.barWidth
  let pts : Array Vec2 := #[![0, 0], ![w, 0], ![w, height], ![0, height]]
  let intensity := config.colorIntensity
  NewPolygon pts {
    fillColor := Color.mk
      (floatMin 1.0 (3 * height * intensity / 2))
      (floatMin 1.0 (2 * height * intensity / 3))
      (floatMin 1.0 (height * intensity / 5))
  }

-- Expressão para criar uma barra individual
private def barExpression (config : Config) : List Float → 𝕋 Mark
  | [h] => 𝕋.pure (createBar h config)
  | _   => 𝕋.pure (NewCircle 0 ![0,0])  -- Fallback para dados inválidos

-- Algebra para combinar barras com espaçamento configurável
private def combineBar (config : Config) (marks : List (𝕋 Mark)) : 𝕋 Mark :=
  match marks with
  | x :: xs => List.foldl (· →[config.spacing] ·) x xs
  | []      => 𝕋.pure (NewCircle 0 ![0,0])

-- ============================================================================
-- ESTRUTURA PRINCIPAL DO BARPLOT
-- ============================================================================

-- Estrutura principal para BarPlot
structure Plot where
  dataFrame : DataFrame
  config : Config := defaultConfig

-- ============================================================================
-- FUNÇÕES PRINCIPAIS DA API
-- ============================================================================

-- 1. Carregar CSV e criar BarPlot
def fromCSV (csvContent : String) (config : Config := defaultConfig) : Plot :=
  let df := DataFrame.fromCSV csvContent
  { dataFrame := df, config := config }

-- 2. Função para plotar com seleção de colunas
def plot (barPlot : Plot) (valueColumn : String) : 𝕋 Mark :=
  let df := barPlot.dataFrame
  let config := barPlot.config

  -- Extrair valores numéricos
  let rawValues := match df.getColumn valueColumn with
    | none => []
    | some col => col.filterMap (fun cv =>
        match cv with
        | CellValue.float f => some f
        | CellValue.int i => some (DataFrame.intToFloat i)
        | _ => none
      )

  -- Normalizar valores
  let values := normalizeValues rawValues config.maxBarHeight

  -- Criar coalgebra: cada valor vira uma lista unitária
  let decomposed := values.map (fun v => [v])

  -- Aplicar expressão de barra para cada valor
  let marks := decomposed.map (barExpression config)

  -- Combinar todas as barras
  combineBar config marks

-- ============================================================================
-- FUNÇÕES DE CONVENIÊNCIA
-- ============================================================================

-- Função que faz tudo em uma linha: CSV → BarPlot → Render
def quickPlot (csvContent : String) (valueColumn : String) (config : Config := defaultConfig) : 𝕋 Mark :=
  let barPlot := fromCSV csvContent config
  plot barPlot valueColumn

-- Função para inspecionar as colunas disponíveis
def availableColumns (barPlot : Plot) : List String :=
  barPlot.dataFrame.columnNames

-- Função para mostrar informações do BarPlot
def info (barPlot : Plot) : String :=
  let df := barPlot.dataFrame
  let cols := df.columnNames
  let nrows := df.nrows
  s!"BarPlot with {cols.length} columns and {nrows} rows\nColumns: {String.intercalate ", " cols}"

-- ============================================================================
-- CONFIGURAÇÕES PRÉ-DEFINIDAS
-- ============================================================================

-- Configuração para barras largas
def wideBarConfig : Config := {
  barWidth := 1.2,
  spacing := 0.3,
  colorIntensity := 0.8,
  maxBarHeight := 4.0,
  showLabels := true
}

-- Configuração para barras compactas
def compactBarConfig : Config := {
  barWidth := 0.5,
  spacing := 0.2,
  colorIntensity := 1.2,
  maxBarHeight := 2.5,
  showLabels := false
}

-- Configuração para barras coloridas
def colorfulBarConfig : Config := {
  barWidth := 0.8,
  spacing := 0.4,
  colorIntensity := 1.5,
  maxBarHeight := 3.5,
  showLabels := true
}

-- ============================================================================
-- EXEMPLOS DE USO
-- ============================================================================

-- Exemplo de dados CSV
def sampleCSV : String :=
"Product,Sales,Revenue
A,1.5,150.0
B,2.3,230.0
C,0.7,70.0
D,3.1,310.0
E,1.8,180.0"

-- Exemplo básico de uso
def example1 : 𝕋 Mark :=
  quickPlot sampleCSV "Sales"

-- Exemplo com configuração personalizada
def example2 : 𝕋 Mark :=
  let barPlot := fromCSV sampleCSV wideBarConfig
  plot barPlot "Revenue"

-- Exemplo de workflow completo
def example3 : 𝕋 Mark :=
  -- 1. Carregar CSV
  let barPlot := fromCSV sampleCSV compactBarConfig
  -- 2. Ver colunas disponíveis (para debug)
  -- let _ := availableColumns barPlot  -- ["Product", "Sales", "Revenue"]
  -- 3. Plotar coluna específica
  plot barPlot "Sales"

-- Para renderizar os exemplos:
-- #html draw example1
-- #html draw example2
-- #html draw example3

-- ============================================================================
-- FUNÇÕES AUXILIARES PARA DEBUG
-- ============================================================================

-- Função para verificar se uma coluna existe
def hasColumn (barPlot : Plot) (columnName : String) : Bool :=
  (availableColumns barPlot).contains columnName

-- Função para obter preview dos dados
def preview (barPlot : Plot) (nrows : Nat := 5) : String :=
  let df := barPlot.dataFrame
  let totalRows := df.nrows
  let rowsToShow := Nat.min nrows totalRows

  let header := String.intercalate " | " df.columnNames
  let separator := String.mk (List.replicate (header.length + 10) '-')

  let rows := (List.range rowsToShow).map fun i =>
    let rowData := df.getRow i
    let rowValues := df.columnNames.map fun colName =>
      match rowData.find? (fun (name, _) => name = colName) with
      | some (_, val) => toString val
      | none => "N/A"
    String.intercalate " | " rowValues

  header ++ "\n" ++ separator ++ "\n" ++ String.intercalate "\n" rows

end BarPlot

-- ============================================================================
-- INTERFACE GLOBAL SIMPLIFICADA
-- ============================================================================

-- Função global para uso rápido
def createBarPlot (csvContent : String) : BarPlot.Plot :=
  BarPlot.fromCSV csvContent

-- Função global para plotagem rápida
def plotBar (csvContent : String) (column : String) : 𝕋 Mark :=
  BarPlot.quickPlot csvContent column

-- Exemplo de uso da interface global:
#html draw (plotBar BarPlot.sampleCSV "Sales")
