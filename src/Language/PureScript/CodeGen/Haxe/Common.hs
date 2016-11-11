-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.Haxe.Common where

import Prelude.Compat

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Crash
import Language.PureScript.Names

moduleNameToHaxe :: ModuleName -> String
moduleNameToHaxe (ModuleName pns) =
  let name =  runProperName <$> pns
  in intercalate "." $ replaceBuiltinName <$> ((((<$>) toLower) <$> (init name)) ++ [(last name) ++ "Class"])

moduleNameToHaxePackage :: ModuleName -> String
moduleNameToHaxePackage (ModuleName pns) =
  let name =  runProperName <$> pns
  in intercalate "." $ replaceBuiltinName <$> (((<$>) toLower) <$> (init name))

moduleNameToHaxeClass :: ModuleName -> String
moduleNameToHaxeClass (ModuleName pns) =
  let name =  last $ runProperName <$> pns
  in replaceBuiltinName (name ++ "Class")

moduleNameToFilePath :: ModuleName -> String
moduleNameToFilePath (ModuleName pns) =
  let name =  runProperName <$> pns
  in intercalate "/" $ replaceBuiltinName <$> (((<$>) toLower) <$> (init name))

replaceBuiltinName :: String -> String
replaceBuiltinName name = identToJs $ Ident name

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name)
  | nameIsHaxeReserved name || nameIsHaxeBuiltIn name = name ++ "__"
  | otherwise = concatMap identCharToString name
identToJs (GenIdent _ _) = internalError "GenIdent in identToJs"

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToJs (Ident s) || null s

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "__dot__"
identCharToString '$' = "__dollar__"
identCharToString '~' = "__tilde__"
identCharToString '=' = "__eq__"
identCharToString '<' = "__less__"
identCharToString '>' = "__greater__"
identCharToString '!' = "__bang__"
identCharToString '#' = "__hash__"
identCharToString '%' = "__percent__"
identCharToString '^' = "__up__"
identCharToString '&' = "__amp__"
identCharToString '|' = "__bar__"
identCharToString '*' = "__times__"
identCharToString '/' = "__div__"
identCharToString '+' = "__plus__"
identCharToString '-' = "__minus__"
identCharToString ':' = "__colon__"
identCharToString '\\' = "__bslash__"
identCharToString '?' = "__qmark__"
identCharToString '@' = "__at__"
identCharToString '\'' = "__prime__"
identCharToString c = '$' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in Haxe.
--
nameIsHaxeReserved :: String -> Bool
nameIsHaxeReserved name =
  name `elem` jsAnyReserved

-- |
-- Checks whether a name matches a built-in value in Haxe.
--
nameIsHaxeBuiltIn :: String -> Bool
nameIsHaxeBuiltIn name =
  name `elem`
    [ "arguments"
    , "Array"
    , "ArrayBuffer"
    , "DataView"
    , "Date"
    , "decodeURI"
    , "decodeURIComponent"
    , "encodeURI"
    , "encodeURIComponent"
    , "Error"
    , "escape"
    , "eval"
    , "EvalError"
    , "Float32Array"
    , "Float64Array"
    , "Infinity"
    , "Int16Array"
    , "Int32Array"
    , "Int8Array"
    , "Intl"
    , "isFinite"
    , "isNaN"
    , "JSON"
    , "Map"
    , "Math"
    , "NaN"
    , "Number"
    , "Object"
    , "parseFloat"
    , "parseInt"
    , "Promise"
    , "Proxy"
    , "RangeError"
    , "ReferenceError"
    , "Reflect"
    , "RegExp"
    , "Set"
    , "SIMD"
    , "String"
    , "Symbol"
    , "SyntaxError"
    , "TypeError"
    , "Uint16Array"
    , "Uint32Array"
    , "Uint8Array"
    , "Uint8ClampedArray"
    , "unescape"
    , "URIError"
    , "WeakMap"
    , "WeakSet"
    ]

jsAnyReserved :: [String]
jsAnyReserved =
  concat
    [ jsKeywords
    , jsSometimesReserved
    , jsFutureReserved
    , jsFutureReservedStrict
    , jsOldReserved
    , jsLiterals
    ]

jsKeywords :: [String]
jsKeywords =
  [ "break"
  , "case"
  , "catch"
  , "class"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "export"
  , "extends"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  ]

jsSometimesReserved :: [String]
jsSometimesReserved =
  [ "await"
  , "let"
  , "static"
  , "yield"
  ]

jsFutureReserved :: [String]
jsFutureReserved =
  [ "enum" ]

jsFutureReservedStrict :: [String]
jsFutureReservedStrict =
  [ "implements"
  , "interface"
  , "package"
  , "private"
  , "protected"
  , "public"
  ]

jsOldReserved :: [String]
jsOldReserved =
  [ "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "double"
  , "final"
  , "float"
  , "goto"
  , "int"
  , "long"
  , "native"
  , "short"
  , "synchronized"
  , "throws"
  , "transient"
  , "volatile"
  ]

jsLiterals :: [String]
jsLiterals =
  [ "null"
  , "true"
  , "false"
  ]
