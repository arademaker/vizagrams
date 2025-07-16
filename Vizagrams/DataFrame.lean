/- # DataFrame
Aqui definimos o novo tipo de dado DataFrame
A ideia é que tenhamos uma estrutura formal para o uso das ferramentas de visualização de dados, onde consideramos
que uma função de plotagem é tal que
  f : DataFrame → 𝕋 Mark
-/

/- Definimos CellValue para representar o "Type" em uma célula do Dataframe
Assim "reduzimos a generalidade do DataFrame" de forma que não podemos ter campos com objetos de tipos como tupla,
List α e tipos criados pelo usuário, entretanto, caso necessário pode se extender o escopo do `inductive` apenas
adicionando um novo type que possua um Repr e BEq e seu respectivo ToString -/
inductive CellValue where
  | int : Int → CellValue
  | float : Float → CellValue
  | string : String → CellValue
  | bool : Bool → CellValue
  | none : CellValue
deriving Repr, BEq

-- Instância para converter CellValue em String
instance : ToString CellValue where
  toString
    | CellValue.int i => toString i
    | CellValue.float f => toString f
    | CellValue.string s => s
    | CellValue.bool b => toString b
    | CellValue.none => "None"

/- Um DataFrame será visto como um conjunto de colunas e a quantidade de linhas que possui
portanto definimos Column como uma lista de CellValue
E definimos DataFrame como uma structure com dois campos
`columns`: Uma lista de Nome e Valores
`nrows`: Quantidade de linhas -/

def Column := List CellValue

structure DataFrame where
  columns : List (String × Column)
  nrows : Nat

-- Instância manual para Repr
instance : Repr DataFrame where
  reprPrec df _ :=
    "DataFrame with " ++ toString df.columns.length ++ " columns and " ++ toString df.nrows ++ " rows"

-- Namespace para operações em DataFrame
namespace DataFrame

-- Empty DataFrame
def empty : DataFrame := {
  columns := [],
  nrows := 0
}

#eval empty

-- Função auxiliar para obter elemento de lista por índice
def getAt : List α → Nat → Option α
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: xs, n + 1 => getAt xs n

-- Função auxiliar para encontrar uma coluna por nome
def findColumn (columns : List (String × Column)) (name : String) : Option Column :=
  match columns.find? (fun (colName, _) => colName = name) with
  | none => none
  | some (_, col) => some col

-- Função auxiliar para atualizar ou adicionar uma coluna
def updateColumn (columns : List (String × Column)) (name : String) (newCol : Column) : List (String × Column) :=
  match columns.findIdx? (fun (colName, _) => colName = name) with
  | none => columns ++ [(name, newCol)]  -- Adiciona nova coluna
  | some idx => columns.set idx (name, newCol)  -- Atualiza coluna existente

-- Adicionar uma coluna ao DataFrame
def addColumn (df : DataFrame) (name : String) (col : Column) : DataFrame :=
  if col.length = df.nrows ∨ df.nrows = 0 then
    {
      columns := updateColumn df.columns name col,
      nrows := if df.nrows = 0 then col.length else df.nrows
    }
  else
    df -- Retorna o DataFrame original se o tamanho não bater

-- Obter uma coluna por nome
def getColumn (df : DataFrame) (name : String) : Option Column :=
  findColumn df.columns name

-- Obter o número de colunas
def ncols (df : DataFrame) : Nat := df.columns.length

-- Obter os nomes das colunas
def columnNames (df : DataFrame) : List String :=
  df.columns.map (fun (name, _) => name)

-- Obter uma célula específica (linha, coluna)
def getCell (df : DataFrame) (row : Nat) (colName : String) : Option CellValue :=
  match df.getColumn colName with
  | none => none
  | some col => getAt col row

-- Obter uma linha completa
def getRow (df : DataFrame) (row : Nat) : List (String × CellValue) :=
  df.columns.filterMap fun (colName, col) =>
    match getAt col row with
    | none => none
    | some val => some (colName, val)

-- Filtrar linhas baseado em uma condição
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

-- Selecionar colunas específicas
def select (df : DataFrame) (colNames : List String) : DataFrame :=
  let selectedCols := df.columns.filter fun (name, _) => colNames.contains name
  let orderedCols := colNames.filterMap fun name =>
    selectedCols.find? fun (colName, _) => colName = name

  {
    columns := orderedCols,
    nrows := df.nrows
  }

-- Mapear uma função sobre uma coluna
def mapColumn (df : DataFrame) (colName : String) (f : CellValue → CellValue) : DataFrame :=
  match df.getColumn colName with
  | none => df
  | some col =>
    let newCol := col.map f
    df.addColumn colName newCol

-- Aplicar uma função de agregação em uma coluna
def aggregate (df : DataFrame) (colName : String) (f : List CellValue → CellValue) : Option CellValue :=
  match df.getColumn colName with
  | none => none
  | some col => some (f col)

-- Função auxiliar para converter Int para Float
def intToFloat (i : Int) : Float :=
  Float.ofInt i

-- Funções de agregação comuns
def sum (values : List CellValue) : CellValue :=
  values.foldl (fun acc val =>
    match acc, val with
    | CellValue.int a, CellValue.int b => CellValue.int (a + b)
    | CellValue.float a, CellValue.float b => CellValue.float (a + b)
    | CellValue.int a, CellValue.float b => CellValue.float (intToFloat a + b)
    | CellValue.float a, CellValue.int b => CellValue.float (a + intToFloat b)
    | _, _ => acc
  ) (CellValue.int 0)

def count (values : List CellValue) : CellValue :=
  CellValue.int values.length

-- Função auxiliar para criar string com largura fixa
def padString (s : String) (width : Nat) : String :=
  let len := s.length
  if len >= width then s
  else s ++ String.mk (List.replicate (width - len) ' ')

-- Exibir o DataFrame de forma formatada
def display (df : DataFrame) : String :=
  let colNames := df.columnNames
  let colWidth := 12

  -- Cabeçalho
  let header := colNames.foldl (fun acc name => acc ++ padString name colWidth) ""

  -- Linhas
  let rows := (List.range df.nrows).map fun i =>
    let rowData := df.getRow i
    colNames.foldl (fun acc colName =>
      let cellValue := rowData.find? (fun (name, _) => name = colName)
      match cellValue with
      | none => acc ++ padString "None" colWidth
      | some (_, val) => acc ++ padString (toString val) colWidth
    ) ""

  -- Linha separadora
  let separator := String.mk (List.replicate (colWidth * colNames.length) '-')

  " " ++ header ++ " " ++ separator ++ " " ++ String.intercalate " " rows

-- Instância ToString para DataFrame
instance : ToString DataFrame where
  toString := display

-- Adicione esta seção dentro do namespace DataFrame, antes do "end DataFrame"

namespace DataFrame

-- Seção de CSV parsing
section CSV

-- Função auxiliar para dividir string por delimitador
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

-- Função auxiliar para remover espaços em branco no início e fim
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

-- Função para converter Int para Float
private def intToFloat (i : Int) : Float :=
  Float.ofInt i

-- Função para tentar parsear um valor como Float (implementação simples)
private def tryParseFloat (s : String) : Option Float :=
  -- Implementação simplificada - verifica se contém ponto
  if s.any (· == '.') then
    -- Tenta converter manualmente
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

-- Função para tentar parsear um valor como Bool
private def tryParseBool (s : String) : Option Bool :=
  let trimmed := trimString s
  let lower := trimmed.toLower
  if lower == "true" || lower == "1" then some true
  else if lower == "false" || lower == "0" then some false
  else none

-- Função para inferir e converter um valor string para CellValue
private def parseCell (s : String) : CellValue :=
  let trimmed := trimString s

  -- Caso vazio
  if trimmed.isEmpty then CellValue.none

  -- Tentar Bool primeiro
  else match tryParseBool trimmed with
    | some b => CellValue.bool b

    -- Tentar Int
    | none => match trimmed.toInt? with
      | some i => CellValue.int i

      -- Tentar Float
      | none => match tryParseFloat trimmed with
        | some f => CellValue.float f

        -- Default para String
        | none => CellValue.string trimmed

-- Função para parsear uma linha CSV
private def parseCsvLine (line : String) : List String :=
  splitString line ','

-- Função principal para parsear CSV
def fromCSV (csvContent : String) : DataFrame :=
  let lines := splitString csvContent '\n'
  let nonEmptyLines := lines.filter (fun line => !(trimString line).isEmpty)

  match nonEmptyLines with
  | [] => DataFrame.empty
  | headerLine :: dataLines =>
    let headers := (parseCsvLine headerLine).map trimString
    let rows := dataLines.map parseCsvLine

    -- Verificar se todas as linhas têm o mesmo número de colunas
    let expectedCols := headers.length
    let validRows := rows.filter (fun row => row.length == expectedCols)

    if validRows.isEmpty then
      -- Retornar DataFrame vazio com headers
      let emptyColumns := headers.map (fun name => (name, ([] : Column)))
      { columns := emptyColumns, nrows := 0 }
    else
      -- Construir colunas
      let nrows := validRows.length
      let columns := headers.enum.map (fun (idx, name) =>
        let columnValues := validRows.map (fun row =>
          match row.get? idx with
          | some cellStr => parseCell cellStr
          | none => CellValue.none
        )
        (name, columnValues)
      )

      { columns := columns, nrows := nrows }

end CSV

end DataFrame

-- Exemplo de uso
def exemplo : DataFrame :=
  let df := DataFrame.empty
  let df := df.addColumn "nome" [CellValue.string "João", CellValue.string "Maria", CellValue.string "Pedro"]
  let df := df.addColumn "idade" [CellValue.int 25, CellValue.int 30, CellValue.int 28]
  let df := df.addColumn "salario" [CellValue.float 5000.0, CellValue.float 6000.0, CellValue.float 5500.0]
  df

-- Teste básico
#eval toString exemplo
#eval exemplo.ncols
#eval exemplo.nrows
#eval exemplo.columnNames
#eval exemplo.getCell 0 "nome"
#eval exemplo.aggregate "idade" DataFrame.sum

-- Exemplo de filtragem (pessoas com idade > 26)
def exemploFiltrado : DataFrame :=
  exemplo.filter fun row =>
    match row.find? (fun (name, _) => name = "idade") with
    | none => false
    | some (_, CellValue.int age) => age > 26
    | _ => false

#eval toString exemploFiltrado

-- Exemplo de seleção de colunas
def exemploSelecionado : DataFrame :=
  exemplo.select ["nome", "salario"]

#eval toString exemploSelecionado

-- Exemplo de mapeamento (aumentar salário em 10%)
def exemploMapeado : DataFrame :=
  exemplo.mapColumn "salario" fun val =>
    match val with
    | CellValue.float x => CellValue.float (x * 1.1)
    | CellValue.int x => CellValue.float (DataFrame.intToFloat x * 1.1)
    | _ => val

#eval toString exemploMapeado

-- Exemplos de uso (fora do namespace)
def csvExample : String :=
  "nome,idade,salario,ativo\nJoão,25,5000.5,true\nMaria,30,6000.0,false\nPedro,28,5500.75,true"

def dfFromCsv : DataFrame := DataFrame.fromCSV csvExample

-- Exemplo com dados mais simples
def csvExample2 : String :=
  "produto,preco,quantidade,disponivel\nNotebook,2500,10,true\nMouse,45,100,true\nTeclado,150,0,false"

def dfFromCsv2 : DataFrame := DataFrame.fromCSV csvExample2

-- Exemplo com dados missing
def csvExample3 : String :=
  "nome,idade,cidade\nAna,25,São Paulo\nCarlos,,Rio de Janeiro\nLuiza,35,"

def dfFromCsv3 : DataFrame := DataFrame.fromCSV csvExample3
#eval toString dfFromCsv3

def csvExample4 : String :=
"
nome,idade,salario,ativo
Lucas,32,4720.25,true
Ana,27,5890.0,false
Rafael,40,7200.75,true
Beatriz,22,4100.0,true
Carlos,35,6500.5,false
"

def dfFromCsv4 : DataFrame := DataFrame.fromCSV csvExample4
#eval toString dfFromCsv4
/-
# DataFrames
Um passo importante é a criação dos objetos que devem guardar nossos dados, sendo esses comumente chamados como
`DataFrame`.
## O que é um DataFrame ?
Um DataFrame é uma estrutura de dados tabular contendo colunas nomeadas e dados associados
Buscamos representar algo como:
```lean
structure DataFrame where
  columns : List String
  lines : List ( List α )  -- (α ∈ Type) ?
```
Aqui nos deparamos com o primeiro problema, com essa estrutura como representar esse tipo de dado abaixo ?

Coluna 1 (Int) | Coluna 2 ( Bool)
             1 |            True
             5 |           False
          ...  |             ...

Uma solução seria (α : String) entretanto acredito não ser uma solução formal

# Category Theory and DataFrames
Podemos tentar formalizar Dataframe como um Functor D
  𝓓 : 𝒞 ⟶ FinSet
Onde 𝒞 é uma categoria discreta onde os objetos são os nomes das colunas,
e objetos distintos não tem morfismos entre si.

A imagem de um objeto 𝒸 ∈ 𝒞 que representa a coluna é um conjunto finito de dados da coluna 𝓓(𝒸) ∈ FinSet
-/
