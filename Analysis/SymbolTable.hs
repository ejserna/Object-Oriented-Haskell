module SymbolTable where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

-- import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as Map

import Data.List (intercalate)

type Scope = Int

globalScope :: Scope
globalScope = 0

defScope :: Scope
defScope = -1

-- La symbol table es un map que dado un identificador, te da el scope de éste junto con su información de símbolos
type SymbolTable = Map.HashMap Identifier Symbol

data Symbol = SymbolVar
                { dataType   :: Type
                , scope      :: Scope
                , isPublic :: Maybe Bool
                }
            | SymbolFunction
                { params :: [(Type,Identifier)]
                , returnType :: Maybe Type
                , body       :: Block
                , scope      :: Scope
                , shouldReturn :: Bool -- Si la funcion regresa Nothing, no tiene por que tener un return regresar
                , isPublic :: Maybe Bool
                , symbolTable :: SymbolTable
                }

instance Show Symbol where
    show sym = case sym of
        SymbolVar dt scope isPublic  -> "SYMVAR  " ++ intercalate ", " [ppShow dt, show scope, show isPublic]
        SymbolFunction params ret body scope shouldRet isPublic symbolTable -> "SYMFUNC  " ++intercalate ", " [ppShow params, show ret, ppShow body, show scope, show shouldRet, show isPublic, ppShow symbolTable]

emptySymbolTable :: SymbolTable
emptySymbolTable = Map.empty 

-- insert :: Identifier -> Symbol -> SymbolTable -> SymbolTable
-- insert idn sym table = if Map.member idn table
--     then table 
--     else Map.insert idn (Map.singleton scp sym) table
--     where
--         inner scpTab = if Map.member scp scpTab
--             then error "SymbolTable.insert: inserting Symbol already in SymbolTable"
--             else Map.insert scp sym scpTab
--         scp = scope sym

