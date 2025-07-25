import Vizagrams

open GeometricPrimitive
open VizBackend
open GraphicalPrimitive
open ProofWidgets Svg
open GraphicalMark
open FreeMonad
open Envelope
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
  showLegend : Bool := true
  showTitle : Bool := true
  showGrid : Bool := false
  labelSize : Float := 0.5
  titleSize : Float := 0.8
  legendSize : Float := 0.4
deriving Repr

-- Configuração padrão
def defaultConfig : Config := {}

-- Configuração minimalista (que estava faltando)
def minimalConfig : Config := {
  barWidth := 0.6,
  spacing := 0.3,
  colorIntensity := 0.8,
  maxBarHeight := 2.0,
  showLabels := false,
  showLegend := false,
  showTitle := false,
  showGrid := false
}

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

-- Função para obter cor da barra baseada no índice
private def getBarColor (index : Nat) (height : Float) (config : Config) : Color :=
  let intensity := config.colorIntensity
  let baseColors : Array (Float × Float × Float) := #[
    (0.2, 0.6, 0.9),  -- Azul
    (0.9, 0.4, 0.2),  -- Vermelho
    (0.2, 0.8, 0.2),  -- Verde
    (0.9, 0.7, 0.1),  -- Amarelo
    (0.6, 0.2, 0.8),  -- Roxo
    (0.9, 0.5, 0.7),  -- Rosa
    (0.3, 0.9, 0.9),  -- Ciano
    (0.9, 0.6, 0.2)   -- Laranja
  ]
  let colorIndex := index % baseColors.size
  match baseColors[colorIndex]? with
  | some (r, g, b) => Color.mk
      (floatMin 1.0 (r * intensity * (0.7 + 0.3 * height / 3.0)))
      (floatMin 1.0 (g * intensity * (0.7 + 0.3 * height / 3.0)))
      (floatMin 1.0 (b * intensity * (0.7 + 0.3 * height / 3.0)))
  | none => Color.mk 0.5 0.5 0.5

-- Criar um polígono de barra com configuração personalizada
private def createBar (height : Float) (index : Nat) (config : Config) : Mark :=
  let w := config.barWidth
  let pts : Array Vec2 := #[![0, 0], ![w, 0], ![w, height], ![0, height]]
  let color := getBarColor index height config
  ⟨NewPolygon pts { fillColor := color }⟩

-- Criar label para uma barra
private def createBarLabel (value : Float) (label : String) (config : Config) : 𝕋 Mark :=
  if config.showLabels then
    let labelText := ⟨NewText label ![config.barWidth / 2, -0.3] config.labelSize
      { fillColor := Color.mk 0 0 0 }⟩
    let valueText := ⟨NewText (toString value) ![config.barWidth / 2, value + 0.1] config.labelSize
      { fillColor := Color.mk 0.3 0.3 0.3 }⟩
    𝕋.pure labelText →[0] 𝕋.pure valueText
  else
    𝕋.pure ⟨NewCircle 0 ![0,0]⟩ -- Invisible placeholder

-- ============================================================================
-- FUNÇÕES PARA LEGENDAS E TÍTULOS
-- ============================================================================

-- Criar item de legenda
private def createLegendItem (index : Nat) (label : String) (value : Float) (config : Config) : 𝕋 Mark :=
  let color := getBarColor index 1.0 config
  let square := ⟨NewPolygon #[![0, 0], ![0.3, 0], ![0.3, 0.3], ![0, 0.3]] { fillColor := color }⟩
  let text := ⟨NewText s!"{label}: {value}" ![0.4, 0.15] config.legendSize { fillColor := Color.mk 0 0 0 }⟩
  𝕋.pure square →[0] 𝕋.pure text

-- Criar legenda completa
private def createLegend (labels : List String) (values : List Float) (config : Config) : 𝕋 Mark :=
  if config.showLegend && !labels.isEmpty then
    let items := labels.zip values
    let legendItems := items.zipIdx.map fun ((label, value), idx) =>
      createLegendItem idx label value config
    match legendItems with
    | [] => 𝕋.pure ⟨NewCircle 0 ![0,0]⟩
    | x :: xs =>
        -- Posicionar itens verticalmente
        let spacing := 0.5
        List.foldl (fun acc item => acc ↓[spacing] item) x xs
  else
    𝕋.pure ⟨NewCircle 0 ![0,0]⟩

-- Criar título do gráfico
private def createTitle (title : String) (config : Config) : 𝕋 Mark :=
  if config.showTitle && !title.isEmpty then
    𝕋.pure ⟨NewText title ![0, 0] config.titleSize { fillColor := Color.mk 0.1 0.1 0.1 }⟩
  else
    𝕋.pure ⟨NewCircle 0 ![0,0]⟩

-- Criar grade de fundo (opcional)
private def createGrid (width : Float) (height : Float) (config : Config) : 𝕋 Mark :=
  if config.showGrid then
    let horizontalLines := (List.range 6).map fun i =>
      let y := (i.toFloat * height) / 5.0
      𝕋.pure ⟨NewLine ![0, y] ![width, y] { strokeColor := Color.mk 0.9 0.9 0.9 }⟩
    match horizontalLines with
    | [] => 𝕋.pure ⟨NewCircle 0 ![0,0]⟩
    | x :: xs => List.foldl (· →[0] ·) x xs
  else
    𝕋.pure ⟨NewCircle 0 ![0,0]⟩

-- ============================================================================
-- ESTRUTURAS PARA BARRA COM METADADOS
-- ============================================================================

-- Estrutura que contém uma barra com seus metadados
structure BarData where
  value : Float
  label : String
  index : Nat

-- Expressão para criar uma barra com label
private def barExpressionWithData (config : Config) : List BarData → 𝕋 Mark
  | [barData] =>
      let bar := 𝕋.pure (createBar barData.value barData.index config)
      let label := createBarLabel barData.value barData.label config
      bar ↓[0] label
  | _ => 𝕋.pure ⟨NewCircle 0 ![0,0]⟩

-- Algebra para combinar barras com espaçamento configurável
private def combineBar (config : Config) (marks : List (𝕋 Mark)) : 𝕋 Mark :=
  match marks with
  | x :: xs => List.foldl (· →[config.spacing] ·) x xs
  | []      => 𝕋.pure ⟨NewCircle 0 ![0,0]⟩

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
def plot (barPlot : Plot) (valueColumn : String) (labelColumn : Option String := none) (title : String := "") : 𝕋 Mark :=
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

  -- Extrair labels (se especificado)
  let labels := match labelColumn with
    | none => rawValues.mapIdx (fun idx _ => s!"Item {idx + 1}")
    | some colName => match df.getColumn colName with
        | none => rawValues.mapIdx (fun idx _ => s!"Item {idx + 1}")
        | some col => col.map (fun cv =>
            match cv with
            | CellValue.string s => s
            | CellValue.int i => toString i
            | CellValue.float f => toString f
            | CellValue.bool b => toString b
            | CellValue.none => "N/A"
          )

  -- Normalizar valores
  let normalizedValues := normalizeValues rawValues config.maxBarHeight

  -- Criar estruturas BarData
  let barDataList := normalizedValues.zipIdx.zip labels |>.map fun ((value, index), label) =>
    { value := value, label := label, index := index : BarData }

  -- Criar coalgebra: cada BarData vira uma lista unitária
  let decomposed := barDataList.map (fun bd => [bd])

  -- Aplicar expressão de barra para cada valor
  let barMarks := decomposed.map (barExpressionWithData config)

  -- Combinar todas as barras
  let mainChart := combineBar config barMarks

  -- Criar componentes adicionais
  let titleMark := createTitle title config
  let legendMark := createLegend labels rawValues config
  let totalWidth := (normalizedValues.length.toFloat * (config.barWidth + config.spacing))
  let gridMark := createGrid totalWidth config.maxBarHeight config

  -- Compor o gráfico final
  let chartWithGrid := gridMark →[0] mainChart
  let chartWithTitle := if config.showTitle && !title.isEmpty
                       then titleMark ↓[0.5] chartWithGrid
                       else chartWithGrid
  let finalChart := if config.showLegend
                    then chartWithTitle →[1.0] legendMark
                    else chartWithTitle

  finalChart

-- ============================================================================
-- FUNÇÕES DE CONVENIÊNCIA
-- ============================================================================

-- Função que faz tudo em uma linha: CSV → BarPlot → Render
def quickPlot (csvContent : String) (valueColumn : String) (labelColumn : Option String := none)
              (title : String := "") (config : Config := defaultConfig) : 𝕋 Mark :=
  let barPlot := fromCSV csvContent config
  plot barPlot valueColumn labelColumn title

-- Versão ainda mais simples para uso rápido
def simplePlot (csvContent : String) (valueColumn : String) : 𝕋 Mark :=
  quickPlot csvContent valueColumn none s!"Bar Chart - {valueColumn}"

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
  simplePlot sampleCSV "Sales"

-- Exemplo com configuração personalizada e legenda
def example2 : 𝕋 Mark :=
  let barPlot := fromCSV sampleCSV colorfulBarConfig
  plot barPlot "Revenue" (some "Product") "Revenue by Product"

-- Exemplo de workflow completo com todas as funcionalidades
def example3 : 𝕋 Mark :=
  quickPlot sampleCSV "Sales" (some "Product") "Sales Performance" wideBarConfig

-- Exemplo minimalista
def example4 : 𝕋 Mark :=
  let barPlot := fromCSV sampleCSV minimalConfig
  plot barPlot "Sales"

-- Para renderizar os exemplos:
-- #html draw example1
-- #html draw example2
-- #html draw example3
-- #html draw example4

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
  BarPlot.simplePlot csvContent column

-- Função global para plotagem completa com legendas
def plotBarWithLegend (csvContent : String) (valueColumn : String) (labelColumn : String) (title : String) : 𝕋 Mark :=
  BarPlot.quickPlot csvContent valueColumn (some labelColumn) title

-- Exemplo de uso da interface global:
#html draw (plotBar BarPlot.sampleCSV "Sales") ( BoundingBox.toFrame (boundingBox𝕋 (plotBar BarPlot.sampleCSV "Sales")))
#html draw (plotBarWithLegend BarPlot.sampleCSV "Revenue" "Product" "Revenue Analysis")
