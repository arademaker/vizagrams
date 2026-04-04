/-!
# DataFrame Implementation in Lean

A formal implementation of the DataFrame data structure for data visualization tools.
The DataFrame is designed to be a foundation for plotting functions of type:
```
f : DataFrame → 𝕋 Mark
```

## Overview
This implementation provides:
- Type-safe cell values (`Int`, `Float`, `String`, `Bool`, `None`)
- Core DataFrame operations: `filter`, `select`, `map_column`, `aggregate`
- CSV parsing capabilities
- Formatted display functionality
-/

/--
Represents the types that can be stored in DataFrame cells.
Covers basic scalar types; can be extended by adding new constructors
that implement `Repr`, `BEq`, and `ToString`.
-/
inductive CellValue where
  | int    : Int    → CellValue
  | float  : Float  → CellValue
  | string : String → CellValue
  | bool   : Bool   → CellValue
  | none   : CellValue
deriving Repr, BEq

instance : ToString CellValue where
  toString
    | CellValue.int i    => toString i
    | CellValue.float f  => toString f
    | CellValue.string s => s
    | CellValue.bool b   => toString b
    | CellValue.none     => "None"

/-- A named column: a list of `CellValue` entries, one per row. -/
def Column := List CellValue

/--
A DataFrame: a collection of named columns and an explicit row count.

**Fields:**
- `columns`: List of `(columnName, column)` pairs
- `nrows`: Number of rows (must match the length of every column)
-/
structure DataFrame where
  columns : List (String × Column)
  nrows   : Nat

instance : Repr DataFrame where
  reprPrec df _ :=
    "DataFrame with " ++ toString df.columns.length ++ " columns and " ++ toString df.nrows ++ " rows"

namespace DataFrame

-- ============================================================================
-- BASIC OPERATIONS
-- ============================================================================

/-- An empty DataFrame with no columns and no rows. -/
def empty : DataFrame := { columns := [], nrows := 0 }

/-- Retrieve the element at index `n` from a list, or `none` if out of bounds. -/
def get_at : List α → Nat → Option α
  | [],      _     => none
  | x :: _,  0     => some x
  | _ :: xs, n + 1 => get_at xs n

/-- Look up a column by name in a list of `(name, column)` pairs. -/
def find_column (columns : List (String × Column)) (name : String) : Option Column :=
  match columns.find? (fun (colName, _) => colName = name) with
  | none            => none
  | some (_, col)   => some col

/--
Update an existing column by name, or append a new column if the name is not found.
-/
def update_column (columns : List (String × Column)) (name : String) (newCol : Column) : List (String × Column) :=
  match columns.findIdx? (fun (colName, _) => colName = name) with
  | none     => columns ++ [(name, newCol)]
  | some idx => columns.set idx (name, newCol)

-- ============================================================================
-- CORE DATAFRAME OPERATIONS
-- ============================================================================

/--
Add or replace a column in the DataFrame.

If the DataFrame is empty (`nrows = 0`), the row count is set to the column length.
The column is ignored if its length does not match `df.nrows`.
-/
def add_column (df : DataFrame) (name : String) (col : Column) : DataFrame :=
  if col.length = df.nrows ∨ df.nrows = 0 then
    { columns := update_column df.columns name col,
      nrows   := if df.nrows = 0 then col.length else df.nrows }
  else
    df

/-- Retrieve a column by name, or `none` if not found. -/
def get_column (df : DataFrame) (name : String) : Option Column :=
  find_column df.columns name

/-- Number of columns in the DataFrame. -/
def ncols (df : DataFrame) : Nat := df.columns.length

/-- List of column names in order. -/
def column_names (df : DataFrame) : List String :=
  df.columns.map (fun (name, _) => name)

/-- Retrieve the value at a specific `(row, columnName)` cell, or `none` if out of range. -/
def get_cell (df : DataFrame) (row : Nat) (colName : String) : Option CellValue :=
  match df.get_column colName with
  | none     => none
  | some col => get_at col row

/-- Retrieve all `(columnName, value)` pairs for a given row index. -/
def get_row (df : DataFrame) (row : Nat) : List (String × CellValue) :=
  df.columns.filterMap fun (colName, col) =>
    match get_at col row with
    | none     => none
    | some val => some (colName, val)

-- ============================================================================
-- ADVANCED OPERATIONS
-- ============================================================================

/-- Keep only the rows for which `predicate` returns `true`. -/
def filter (df : DataFrame) (predicate : List (String × CellValue) → Bool) : DataFrame :=
  let validRows := (List.range df.nrows).filter fun i =>
    predicate (df.get_row i)
  let newColumns := df.columns.map fun (colName, col) =>
    let filteredCol := validRows.filterMap fun i => get_at col i
    (colName, filteredCol)
  { columns := newColumns, nrows := validRows.length }

/-- Return a new DataFrame containing only the listed columns, in the given order. -/
def select (df : DataFrame) (colNames : List String) : DataFrame :=
  let selectedCols := df.columns.filter fun (name, _) => colNames.contains name
  let orderedCols  := colNames.filterMap fun name =>
    selectedCols.find? fun (colName, _) => colName = name
  { columns := orderedCols, nrows := df.nrows }

/-- Apply `f` to every cell in the named column, returning an updated DataFrame. -/
def map_column (df : DataFrame) (colName : String) (f : CellValue → CellValue) : DataFrame :=
  match df.get_column colName with
  | none     => df
  | some col =>
    let newCol := col.map f
    df.add_column colName newCol

/-- Reduce the named column to a single `CellValue` using `f`, or `none` if not found. -/
def aggregate (df : DataFrame) (colName : String) (f : List CellValue → CellValue) : Option CellValue :=
  match df.get_column colName with
  | none     => none
  | some col => some (f col)

-- ============================================================================
-- AGGREGATION FUNCTIONS
-- ============================================================================

/-- Convert an `Int` to a `Float`. -/
def int_to_float (i : Int) : Float := Float.ofInt i

/--
Sum all numeric values in a column.
Mixed `Int`/`Float` values are coerced to `Float`; non-numeric values are skipped.
-/
def sum (values : List CellValue) : CellValue :=
  values.foldl (fun acc val =>
    match acc, val with
    | CellValue.int a,   CellValue.int b   => CellValue.int   (a + b)
    | CellValue.float a, CellValue.float b => CellValue.float (a + b)
    | CellValue.int a,   CellValue.float b => CellValue.float (int_to_float a + b)
    | CellValue.float a, CellValue.int b   => CellValue.float (a + int_to_float b)
    | _, _                                 => acc
  ) (CellValue.int 0)

/-- Count the number of values in a column. -/
def count (values : List CellValue) : CellValue := CellValue.int values.length

-- ============================================================================
-- DISPLAY FUNCTIONS
-- ============================================================================

/-- Pad `s` with trailing spaces to at least `width` characters. -/
def pad_string (s : String) (width : Nat) : String :=
  let len := s.length
  if len >= width then s
  else s ++ String.mk (List.replicate (width - len) ' ')

/-- Render the DataFrame as a formatted fixed-width string table. -/
def display (df : DataFrame) : String :=
  let colNames := df.column_names
  let colWidth := 12
  let header    := colNames.foldl (fun acc name => acc ++ pad_string name colWidth) ""
  let rows      := (List.range df.nrows).map fun i =>
    let rowData := df.get_row i
    colNames.foldl (fun acc colName =>
      let cellValue := rowData.find? (fun (name, _) => name = colName)
      match cellValue with
      | none          => acc ++ pad_string "None" colWidth
      | some (_, val) => acc ++ pad_string (toString val) colWidth
    ) ""
  let separator := String.mk (List.replicate (colWidth * colNames.length) '-')
  " " ++ header ++ " " ++ separator ++ " " ++ String.intercalate " " rows

instance : ToString DataFrame where
  toString := display

-- ============================================================================
-- CSV PARSING
-- ============================================================================

section CSV

/-- Split `s` into substrings separated by `delim`. -/
private def split_string (s : String) (delim : Char) : List String :=
  let chars := s.toList
  let rec splitRec (acc : List String) (current : List Char) (remaining : List Char) : List String :=
    match remaining with
    | [] =>
      if current.isEmpty then acc.reverse
      else (String.mk current.reverse :: acc).reverse
    | c :: rest =>
      if c = delim then splitRec (String.mk current.reverse :: acc) [] rest
      else splitRec acc (c :: current) rest
  splitRec [] [] chars

/-- Remove leading and trailing whitespace characters from `s`. -/
private def trim_string (s : String) : String :=
  let chars := s.toList
  let rec trimLeft : List Char → List Char
    | []             => []
    | ' '  :: rest   => trimLeft rest
    | '\t' :: rest   => trimLeft rest
    | '\n' :: rest   => trimLeft rest
    | '\r' :: rest   => trimLeft rest
    | list           => list
  let rec trimRight : List Char → List Char
    | []   => []
    | list => (trimLeft list.reverse).reverse
  String.mk (trimRight (trimLeft chars))

/-- Attempt to parse `s` as a `Float`; returns `none` if parsing fails. -/
private def try_parse_float (s : String) : Option Float :=
  if s.any (· == '.') then
    let parts := split_string s '.'
    match parts with
    | [intPart, fracPart] =>
      match intPart.toInt?, fracPart.toInt? with
      | some i, some f =>
        let fracDigits := fracPart.length
        let fracValue  := int_to_float f / int_to_float (10 ^ fracDigits)
        some (int_to_float i + fracValue)
      | _, _ => none
    | _ => none
  else none

/-- Attempt to parse `s` as a `Bool`; recognises `"true"`, `"false"`, `"1"`, `"0"`. -/
private def try_parse_bool (s : String) : Option Bool :=
  let lower := (trim_string s).toLower
  if lower == "true"  || lower == "1" then some true
  else if lower == "false" || lower == "0" then some false
  else none

/--
Infer the type of a CSV cell string and convert it to a `CellValue`.
Tries `Bool`, then `Int`, then `Float`, defaulting to `String`.
-/
private def parse_cell (s : String) : CellValue :=
  let trimmed := trim_string s
  if trimmed.isEmpty then CellValue.none
  else match try_parse_bool trimmed with
    | some b => CellValue.bool b
    | none   => match trimmed.toInt? with
      | some i => CellValue.int i
      | none   => match try_parse_float trimmed with
        | some f => CellValue.float f
        | none   => CellValue.string trimmed

/-- Split a single CSV line into a list of raw cell strings. -/
private def parse_csv_line (line : String) : List String :=
  split_string line ','

/--
Parse a CSV string into a DataFrame.

The first line is treated as the header row (column names).
Subsequent lines are data rows. Rows with a different number of fields than the
header are silently dropped. Cell values are inferred via `parse_cell`.
-/
def from_csv (csvContent : String) : DataFrame :=
  let lines         := split_string csvContent '\n'
  let nonEmptyLines := lines.filter (fun line => !(trim_string line).isEmpty)
  match nonEmptyLines with
  | []                          => DataFrame.empty
  | headerLine :: dataLines =>
    let headers      := (parse_csv_line headerLine).map trim_string
    let rows         := dataLines.map parse_csv_line
    let expectedCols := headers.length
    let validRows    := rows.filter (fun row => row.length == expectedCols)
    if validRows.isEmpty then
      { columns := headers.map (fun name => (name, ([] : Column))), nrows := 0 }
    else
      let nrows   := validRows.length
      let columns := headers.zipIdx.map (fun (name, idx) =>
        let columnValues := validRows.map (fun row =>
          match row[idx]? with
          | some cellStr => parse_cell cellStr
          | none         => CellValue.none)
        (name, columnValues))
      { columns := columns, nrows := nrows }

end CSV

end DataFrame

-- ============================================================================
-- EXAMPLES AND TESTS
-- ============================================================================

-- Basic example
def df_example: DataFrame :=
  let df := DataFrame.empty
  let df := df.add_column "name" [CellValue.string "João", CellValue.string "Maria", CellValue.string "Pedro"]
  let df := df.add_column "age" [CellValue.int 25, CellValue.int 30, CellValue.int 28]
  let df := df.add_column "salary" [CellValue.float 5000.0, CellValue.float 6000.0, CellValue.float 5500.0]
  df

-- Basic tests
#eval toString df_example
#eval df_example.ncols
#eval df_example.nrows
#eval df_example.column_names
#eval df_example.get_cell 0 "name"
#eval df_example.aggregate "age" DataFrame.sum

-- Filtering example (people with age > 26)
def example_filtered : DataFrame :=
  df_example.filter fun row =>
    match row.find? (fun (name, _) => name = "age") with
    | none => false
    | some (_, CellValue.int age) => age > 26
    | _ => false

#eval toString example_filtered

-- Column selection example
def example_selected : DataFrame :=
  df_example.select ["name", "salary"]

#eval toString example_selected

-- Mapping example (increase salary by 10%)
def example_mapped : DataFrame :=
  df_example.map_column "salary" fun val =>
    match val with
    | CellValue.float x => CellValue.float (x * 1.1)
    | CellValue.int x => CellValue.float (DataFrame.int_to_float x * 1.1)
    | _ => val

#eval toString example_mapped

-- ============================================================================
-- CSV EXAMPLES
-- ============================================================================

-- CSV example 1
def csvExample1 : String :=
  "name,age,salary,active\nJoão,25,5000.5,true\nMaria,30,6000.0,false\nPedro,28,5500.75,true"

def dfFromCsv1 : DataFrame := DataFrame.from_csv csvExample1

-- CSV example 2
def csvExample2 : String :=
  "product,price,quantity,available\nNotebook,2500,10,true\nMouse,45,100,true\nKeyboard,150,0,false"

def dfFromCsv2 : DataFrame := DataFrame.from_csv csvExample2

-- CSV example with missing data
def csvExample3 : String :=
  "name,age,city\nAna,25,São Paulo\nCarlos,,Rio de Janeiro\nLuiza,35,"

def dfFromCsv3 : DataFrame := DataFrame.from_csv csvExample3
#eval toString dfFromCsv3

-- CSV example 4
def csvExample4 : String :=
"name,age,salary,active
Lucas,32,4720.25,true
Ana,27,5890.0,false
Rafael,40,7200.75,true
Beatriz,22,4100.0,true
Carlos,35,6500.5,false"

def dfFromCsv4 : DataFrame := DataFrame.from_csv csvExample4
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
