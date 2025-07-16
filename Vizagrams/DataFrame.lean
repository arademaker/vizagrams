/-
# DataFrame Implementation in Lean

A formal implementation of DataFrame data structure for data visualization tools.
The DataFrame is designed to be a foundation for plotting functions of type:
  f : DataFrame → 𝕋 Mark

## Overview
This implementation provides:
- Type-safe cell values (Int, Float, String, Bool, None)
- DataFrame operations (filter, select, map, aggregate)
- CSV parsing capabilities
- Formatted display functionality
-/

/-
## CellValue Definition
Represents the types that can be stored in DataFrame cells.
This reduces the generality to basic types but can be extended by adding
new types that implement Repr, BEq, and ToString.
-/
inductive CellValue where
  | int : Int → CellValue
  | float : Float → CellValue
  | string : String → CellValue
  | bool : Bool → CellValue
  | none : CellValue
deriving Repr, BEq

-- ToString instance for CellValue
instance : ToString CellValue where
  toString
    | CellValue.int i => toString i
    | CellValue.float f => toString f
    | CellValue.string s => s
    | CellValue.bool b => toString b
    | CellValue.none => "None"

/-
## DataFrame Structure
A DataFrame is represented as a collection of named columns and row count.
- Column: List of CellValue
- DataFrame: Structure with columns (name-value pairs) and row count
-/
def Column := List CellValue

structure DataFrame where
  columns : List (String × Column)
  nrows : Nat

-- Repr instance for DataFrame
instance : Repr DataFrame where
  reprPrec df _ :=
    "DataFrame with " ++ toString df.columns.length ++ " columns and " ++ toString df.nrows ++ " rows"

/-
## DataFrame Operations
-/
namespace DataFrame

-- ============================================================================
-- BASIC OPERATIONS
-- ============================================================================

-- Create empty DataFrame
def empty : DataFrame := {
  columns := [],
  nrows := 0
}

-- Helper function to get element from list by index
def getAt : List α → Nat → Option α
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: xs, n + 1 => getAt xs n

-- Find column by name
def findColumn (columns : List (String × Column)) (name : String) : Option Column :=
  match columns.find? (fun (colName, _) => colName = name) with
  | none => none
  | some (_, col) => some col

-- Update or add column
def updateColumn (columns : List (String × Column)) (name : String) (newCol : Column) : List (String × Column) :=
  match columns.findIdx? (fun (colName, _) => colName = name) with
  | none => columns ++ [(name, newCol)]  -- Add new column
  | some idx => columns.set idx (name, newCol)  -- Update existing column

-- ============================================================================
-- CORE DATAFRAME OPERATIONS
-- ============================================================================

-- Add column to DataFrame
def addColumn (df : DataFrame) (name : String) (col : Column) : DataFrame :=
  if col.length = df.nrows ∨ df.nrows = 0 then
    {
      columns := updateColumn df.columns name col,
      nrows := if df.nrows = 0 then col.length else df.nrows
    }
  else
    df -- Return original DataFrame if size doesn't match

-- Get column by name
def getColumn (df : DataFrame) (name : String) : Option Column :=
  findColumn df.columns name

-- Get number of columns
def ncols (df : DataFrame) : Nat := df.columns.length

-- Get column names
def columnNames (df : DataFrame) : List String :=
  df.columns.map (fun (name, _) => name)

-- Get specific cell (row, column)
def getCell (df : DataFrame) (row : Nat) (colName : String) : Option CellValue :=
  match df.getColumn colName with
  | none => none
  | some col => getAt col row

-- Get complete row
def getRow (df : DataFrame) (row : Nat) : List (String × CellValue) :=
  df.columns.filterMap fun (colName, col) =>
    match getAt col row with
    | none => none
    | some val => some (colName, val)

-- ============================================================================
-- ADVANCED OPERATIONS
-- ============================================================================

-- Filter rows based on predicate
def filter (df : DataFrame) (predicate : List (String × CellValue) → Bool) : DataFrame :=
  let validRows := (List.range df.nrows).filter fun i =>
    predicate (df.getRow i)

  let newColumns := df.columns.map fun (colName, col) =>
    let filteredCol := validRows.filterMap fun i => getAt col i
    (colName, filteredCol)

  {
    columns := newColumns,
    nrows := validRows.length
  }

-- Select specific columns
def select (df : DataFrame) (colNames : List String) : DataFrame :=
  let selectedCols := df.columns.filter fun (name, _) => colNames.contains name
  let orderedCols := colNames.filterMap fun name =>
    selectedCols.find? fun (colName, _) => colName = name

  {
    columns := orderedCols,
    nrows := df.nrows
  }

-- Map function over column
def mapColumn (df : DataFrame) (colName : String) (f : CellValue → CellValue) : DataFrame :=
  match df.getColumn colName with
  | none => df
  | some col =>
    let newCol := col.map f
    df.addColumn colName newCol

-- Apply aggregation function to column
def aggregate (df : DataFrame) (colName : String) (f : List CellValue → CellValue) : Option CellValue :=
  match df.getColumn colName with
  | none => none
  | some col => some (f col)

-- ============================================================================
-- AGGREGATION FUNCTIONS
-- ============================================================================

-- Helper function to convert Int to Float
def intToFloat (i : Int) : Float :=
  Float.ofInt i

-- Sum aggregation
def sum (values : List CellValue) : CellValue :=
  values.foldl (fun acc val =>
    match acc, val with
    | CellValue.int a, CellValue.int b => CellValue.int (a + b)
    | CellValue.float a, CellValue.float b => CellValue.float (a + b)
    | CellValue.int a, CellValue.float b => CellValue.float (intToFloat a + b)
    | CellValue.float a, CellValue.int b => CellValue.float (a + intToFloat b)
    | _, _ => acc
  ) (CellValue.int 0)

-- Count aggregation
def count (values : List CellValue) : CellValue :=
  CellValue.int values.length

-- ============================================================================
-- DISPLAY FUNCTIONS
-- ============================================================================

-- Helper function to pad string to fixed width
def padString (s : String) (width : Nat) : String :=
  let len := s.length
  if len >= width then s
  else s ++ String.mk (List.replicate (width - len) ' ')

-- Display DataFrame in formatted way
def display (df : DataFrame) : String :=
  let colNames := df.columnNames
  let colWidth := 12

  -- Header
  let header := colNames.foldl (fun acc name => acc ++ padString name colWidth) ""

  -- Rows
  let rows := (List.range df.nrows).map fun i =>
    let rowData := df.getRow i
    colNames.foldl (fun acc colName =>
      let cellValue := rowData.find? (fun (name, _) => name = colName)
      match cellValue with
      | none => acc ++ padString "None" colWidth
      | some (_, val) => acc ++ padString (toString val) colWidth
    ) ""

  -- Separator line
  let separator := String.mk (List.replicate (colWidth * colNames.length) '-')

  " " ++ header ++ " " ++ separator ++ " " ++ String.intercalate " " rows

-- ToString instance for DataFrame
instance : ToString DataFrame where
  toString := display

-- ============================================================================
-- CSV PARSING
-- ============================================================================

section CSV

-- Helper function to split string by delimiter
private def splitString (s : String) (delim : Char) : List String :=
  let chars := s.toList
  let rec splitRec (acc : List String) (current : List Char) (remaining : List Char) : List String :=
    match remaining with
    | [] =>
      if current.isEmpty then acc.reverse
      else (String.mk current.reverse :: acc).reverse
    | c :: rest =>
      if c = delim then
        splitRec (String.mk current.reverse :: acc) [] rest
      else
        splitRec acc (c :: current) rest
  splitRec [] [] chars

-- Helper function to trim whitespace
private def trimString (s : String) : String :=
  let chars := s.toList
  let rec trimLeft : List Char → List Char
    | [] => []
    | ' ' :: rest => trimLeft rest
    | '\t' :: rest => trimLeft rest
    | '\n' :: rest => trimLeft rest
    | '\r' :: rest => trimLeft rest
    | list => list

  let rec trimRight : List Char → List Char
    | [] => []
    | list =>
      let reversed := list.reverse
      let trimmed := trimLeft reversed
      trimmed.reverse

  String.mk (trimRight (trimLeft chars))

-- Try to parse value as Float (simplified implementation)
private def tryParseFloat (s : String) : Option Float :=
  if s.any (· == '.') then
    let parts := splitString s '.'
    match parts with
    | [intPart, fracPart] =>
      match intPart.toInt?, fracPart.toInt? with
      | some i, some f =>
        let fracDigits := fracPart.length
        let fracValue := intToFloat f / intToFloat (10 ^ fracDigits)
        some (intToFloat i + fracValue)
      | _, _ => none
    | _ => none
  else
    none

-- Try to parse value as Bool
private def tryParseBool (s : String) : Option Bool :=
  let trimmed := trimString s
  let lower := trimmed.toLower
  if lower == "true" || lower == "1" then some true
  else if lower == "false" || lower == "0" then some false
  else none

-- Infer and convert string value to CellValue
private def parseCell (s : String) : CellValue :=
  let trimmed := trimString s

  -- Empty case
  if trimmed.isEmpty then CellValue.none
  -- Try Bool first
  else match tryParseBool trimmed with
    | some b => CellValue.bool b
    -- Try Int
    | none => match trimmed.toInt? with
      | some i => CellValue.int i
      -- Try Float
      | none => match tryParseFloat trimmed with
        | some f => CellValue.float f
        -- Default to String
        | none => CellValue.string trimmed

-- Parse CSV line
private def parseCsvLine (line : String) : List String :=
  splitString line ','

-- Main function to parse CSV
def fromCSV (csvContent : String) : DataFrame :=
  let lines := splitString csvContent '\n'
  let nonEmptyLines := lines.filter (fun line => !(trimString line).isEmpty)

  match nonEmptyLines with
  | [] => DataFrame.empty
  | headerLine :: dataLines =>
    let headers := (parseCsvLine headerLine).map trimString
    let rows := dataLines.map parseCsvLine

    -- Check if all lines have same number of columns
    let expectedCols := headers.length
    let validRows := rows.filter (fun row => row.length == expectedCols)

    if validRows.isEmpty then
      -- Return empty DataFrame with headers
      let emptyColumns := headers.map (fun name => (name, ([] : Column)))
      { columns := emptyColumns, nrows := 0 }
    else
      -- Build columns
      let nrows := validRows.length
      let columns := headers.zipIdx.map (fun (name, idx) =>
        let columnValues := validRows.map (fun row =>
          match row[idx]? with
          | some cellStr => parseCell cellStr
          | none => CellValue.none
        )
        (name, columnValues)
      )

      { columns := columns, nrows := nrows }

end CSV

end DataFrame

-- ============================================================================
-- EXAMPLES AND TESTS
-- ============================================================================

-- Basic example
def exemplo : DataFrame :=
  let df := DataFrame.empty
  let df := df.addColumn "nome" [CellValue.string "João", CellValue.string "Maria", CellValue.string "Pedro"]
  let df := df.addColumn "idade" [CellValue.int 25, CellValue.int 30, CellValue.int 28]
  let df := df.addColumn "salario" [CellValue.float 5000.0, CellValue.float 6000.0, CellValue.float 5500.0]
  df

-- Basic tests
#eval toString exemplo
#eval exemplo.ncols
#eval exemplo.nrows
#eval exemplo.columnNames
#eval exemplo.getCell 0 "nome"
#eval exemplo.aggregate "idade" DataFrame.sum

-- Filtering example (people with age > 26)
def exemploFiltrado : DataFrame :=
  exemplo.filter fun row =>
    match row.find? (fun (name, _) => name = "idade") with
    | none => false
    | some (_, CellValue.int age) => age > 26
    | _ => false

#eval toString exemploFiltrado

-- Column selection example
def exemploSelecionado : DataFrame :=
  exemplo.select ["nome", "salario"]

#eval toString exemploSelecionado

-- Mapping example (increase salary by 10%)
def exemploMapeado : DataFrame :=
  exemplo.mapColumn "salario" fun val =>
    match val with
    | CellValue.float x => CellValue.float (x * 1.1)
    | CellValue.int x => CellValue.float (DataFrame.intToFloat x * 1.1)
    | _ => val

#eval toString exemploMapeado

-- ============================================================================
-- CSV EXAMPLES
-- ============================================================================

-- CSV example 1
def csvExample1 : String :=
  "nome,idade,salario,ativo\nJoão,25,5000.5,true\nMaria,30,6000.0,false\nPedro,28,5500.75,true"

def dfFromCsv1 : DataFrame := DataFrame.fromCSV csvExample1

-- CSV example 2
def csvExample2 : String :=
  "produto,preco,quantidade,disponivel\nNotebook,2500,10,true\nMouse,45,100,true\nTeclado,150,0,false"

def dfFromCsv2 : DataFrame := DataFrame.fromCSV csvExample2

-- CSV example with missing data
def csvExample3 : String :=
  "nome,idade,cidade\nAna,25,São Paulo\nCarlos,,Rio de Janeiro\nLuiza,35,"

def dfFromCsv3 : DataFrame := DataFrame.fromCSV csvExample3
#eval toString dfFromCsv3

-- CSV example 4
def csvExample4 : String :=
"nome,idade,salario,ativo
Lucas,32,4720.25,true
Ana,27,5890.0,false
Rafael,40,7200.75,true
Beatriz,22,4100.0,true
Carlos,35,6500.5,false"

def dfFromCsv4 : DataFrame := DataFrame.fromCSV csvExample4
#eval toString dfFromCsv4

/-
# DataFrames - Theoretical Background

## What is a DataFrame?
A DataFrame is a tabular data structure containing named columns and associated data.
We seek to represent something like:
```lean
structure DataFrame where
  columns : List String
  lines : List (List α)  -- (α ∈ Type) ?
```

## Category Theory and DataFrames
We can try to formalize DataFrame as a Functor D:
  𝓓 : 𝒞 ⟶ FinSet
Where 𝒞 is a discrete category where objects are column names,
and distinct objects have no morphisms between them.

The image of an object 𝒸 ∈ 𝒞 representing a column is a finite set
of column data 𝓓(𝒸) ∈ FinSet.
-/
