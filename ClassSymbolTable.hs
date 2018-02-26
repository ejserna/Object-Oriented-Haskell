module ClassSymbolTable where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

import SymbolTable
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

-- La class symbol table es un map que dado un class id, te da su tabla de simbolos
type ClassSymbolTable = Map.Map ClassIdentifier SymbolTable

emptyClassSymbolTable :: ClassSymbolTable
emptyClassSymbolTable = Map.empty

insert :: Identifier -> SymbolTable -> ClassSymbolTable -> ClassSymbolTable
insert idn symTable classTable = if Map.member idn classTable
    then classTable 
    else Map.insert idn symTable classTable