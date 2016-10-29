-- |
-- Pretty printer for the Javascript AST
--
module Language.PureScript.Pretty.Haxe
  ( prettyPrintHaxe
  , prettyPrintHaxeWithSourceMaps
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.Monad.State hiding (sequence)
import Control.PatternArrows
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)
import Data.Monoid

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.CodeGen.Haxe.AST
import Language.PureScript.CodeGen.Haxe.Common
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common

import Numeric

literals :: (Emit gen) => Pattern PrinterState Haxe gen
literals = mkPattern' match'
  where
  match' :: (Emit gen) => Haxe -> StateT PrinterState Maybe gen
  match' js = (addMapping' (getSourceSpan js) <>) <$> match js

  match :: (Emit gen) => Haxe -> StateT PrinterState Maybe gen
  match (HaxeNumericLiteral _ n) = return $ emit $ either show show n
  match (HaxeStringLiteral _ s) = return $ string s
  match (HaxeBooleanLiteral _ True) = return $ emit "true"
  match (HaxeBooleanLiteral _ False) = return $ emit "false"
  match (HaxeArrayLiteral _ xs) = mconcat <$> sequence
    [ return $ emit "[ "
    , intercalate (emit ", ") <$> forM xs prettyPrintHaxe'
    , return $ emit " ]"
    ]
  match (HaxeObjectLiteral _ []) = return $ emit "{}"
  match (HaxeObjectLiteral _ ps) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key <> emit ": ") <>) . prettyPrintHaxe' $ value
        indentString <- currentIndent
        return $ intercalate (emit ", \n") $ map (indentString <>) jss
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
    where
    objectPropertyToString :: (Emit gen) => String -> gen
    objectPropertyToString s | identNeedsEscaping s = emit $ show s
                             | otherwise = emit s
  match (HaxeBlock _ sts) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ prettyStatementsSC sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (HaxeVar _ ident) = return $ emit ident
  match (HaxeVariableIntroduction _ ident value) = mconcat <$> sequence
    [ return $ emit $ "var " ++ ident
    , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintHaxe') value
    ]
  match (HaxeAssignment _ target value) = mconcat <$> sequence
    [ prettyPrintHaxe' target
    , return $ emit " = "
    , prettyPrintHaxe' value
    ]
  match (HaxeAttribute _ target value) = mconcat <$> sequence
    [ return $ emit "public static "
    , prettyPrintHaxe' target
    , return $ emit " = "
    , prettyPrintHaxe' value
    , return $ emit ";"
    ]
  match (HaxeWhile _ cond sts) = mconcat <$> sequence
    [ return $ emit "while ("
    , prettyPrintHaxe' cond
    , return $ emit ") "
    , prettyPrintHaxe' sts
    ]
  match (HaxeForIn _ ident obj sts) = mconcat <$> sequence
    [ return $ emit $ "for (var " ++ ident ++ " in "
    , prettyPrintHaxe' obj
    , return $ emit ") "
    , prettyPrintHaxe' sts
    ]
  match (HaxeIfElse _ cond thens elses) = mconcat <$> sequence
    [ return $ emit "if ("
    , prettyPrintHaxe' cond
    , return $ emit ") "
    , prettyPrintHaxe' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintHaxe') elses
    ]
  match (HaxeReturn _ value) = mconcat <$> sequence
    [ return $ emit "return "
    , prettyPrintHaxe' value
    ]
  match (HaxeThrow _ value) = mconcat <$> sequence
    [ return $ emit "throw "
    , prettyPrintHaxe' value
    ]
  match (HaxeBreak _ lbl) = return $ emit $ "break " ++ lbl
  match (HaxeContinue _ lbl) = return $ emit $ "continue " ++ lbl
  match (HaxeComment _ com js) = fmap mconcat $ sequence $
    [ return $ emit "\n"
    , currentIndent
    , return $ emit "/**\n"
    ] ++
    map asLine (concatMap commentLines com) ++
    [ currentIndent
    , return $ emit " */\n"
    , currentIndent
    , prettyPrintHaxe' js
    ]
    where
    commentLines :: Comment -> [String]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = lines s

    asLine :: (Emit gen) => String -> StateT PrinterState Maybe gen
    asLine s = do
      i <- currentIndent
      return $ i <> emit " * " <> (emit . removeComments) s <> emit "\n"

    removeComments :: String -> String
    removeComments ('*' : '/' : s) = removeComments s
    removeComments (c : s) = c : removeComments s

    removeComments [] = []
  match (HaxeRaw _ js) = return $ emit js
  match (HaxeImport _ i) = return $ emit ("import " <> i <> ";")
  match (HaxePackage _ p) = return $ emit ("package " <> p <> ";")
  match (HaxeClass _ n sts) = mconcat <$> sequence
    [ return $ emit ("class " ++ n ++ " {\n")
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (HaxeMethod _ name args ret) = mconcat <$> sequence
    [ return $ emit ("public static function " ++ name ++ "(" ++ intercalate ", " args ++ ") {\n")
    , withIndent $ prettyStatementsSC [ret]
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (HaxeMember _ name ret) = return $ emit ("public var " ++ name ++ ";" )
  match _ = mzero

string :: (Emit gen) => String -> gen
string s = emit $ '"' : concatMap encodeChar s ++ "\""
  where
  encodeChar :: Char -> String
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  encodeChar c | fromEnum c > 0xFFFF = "\\u" ++ showHex highSurrogate ("\\u" ++ showHex lowSurrogate "")
    where
    (h, l) = divMod (fromEnum c - 0x10000) 0x400
    highSurrogate = h + 0xD800
    lowSurrogate = l + 0xDC00
  encodeChar c | fromEnum c > 0xFFF = "\\u" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c < 0x10 = "\\x0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0x7E || fromEnum c < 0x20 = "\\x" ++ showHex (fromEnum c) ""
  encodeChar c = [c]

conditional :: Pattern PrinterState Haxe ((Maybe SourceSpan, Haxe, Haxe), Haxe)
conditional = mkPattern match
  where
  match (HaxeConditional ss cond th el) = Just ((ss, th, el), cond)
  match _ = Nothing

accessor :: (Emit gen) => Pattern PrinterState Haxe (gen, Haxe)
accessor = mkPattern match
  where
  match (HaxeAccessor _ prop val) = Just (emit prop, val)
  match _ = Nothing

indexer :: (Emit gen) => Pattern PrinterState Haxe (gen, Haxe)
indexer = mkPattern' match
  where
  match (HaxeIndexer _ index val) = (,) <$> prettyPrintHaxe' index <*> pure val

  match _ = mzero

lam :: Pattern PrinterState Haxe ((Maybe String, [String], Maybe SourceSpan), Haxe)
lam = mkPattern match
  where
  match (HaxeFunction ss name args ret) = Just ((name, args, ss), ret)
  match _ = Nothing

app :: (Emit gen) => Pattern PrinterState Haxe (gen, Haxe)
app = mkPattern' match
  where
  match (HaxeApp _ val args) = do
    jss <- traverse prettyPrintHaxe' args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

typeOf :: Pattern PrinterState Haxe ((), Haxe)
typeOf = mkPattern match
  where
  match (HaxeTypeOf _ val) = Just ((), val)
  match _ = Nothing

instanceOf :: Pattern PrinterState Haxe (Haxe, Haxe)
instanceOf = mkPattern match
  where
  match (HaxeInstanceOf _ val ty) = Just (val, ty)
  match _ = Nothing

unary' :: (Emit gen) => UnaryOperator -> (Haxe -> String) -> Operator PrinterState Haxe gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState Haxe (gen, Haxe)
  match = mkPattern match'
    where
    match' (HaxeUnary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> String -> Operator PrinterState Haxe gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState Haxe gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (HaxeUnary _ Negate _) = True
  isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> String -> Operator PrinterState Haxe gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " ++ str ++ " ") <> v2)
  where
  match :: Pattern PrinterState Haxe (Haxe, Haxe)
  match = mkPattern match'
    where
    match' (HaxeBinary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: (Emit gen) => [Haxe] -> StateT PrinterState Maybe gen
prettyStatements = prettyStatements' ""

prettyStatementsSC :: (Emit gen) => [Haxe] -> StateT PrinterState Maybe gen
prettyStatementsSC = prettyStatements' ";"

prettyStatements' :: (Emit gen) => String -> [Haxe] -> StateT PrinterState Maybe gen
prettyStatements' lineEnd sts = do
  jss <- forM sts prettyPrintHaxe'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit lineEnd) . (indentString <>)) jss

-- |
-- Generate a pretty-printed string representing a Javascript expression
--
prettyPrintHaxe1 :: (Emit gen) => Haxe -> gen
prettyPrintHaxe1 = fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintHaxe'

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
prettyPrintHaxeWithSourceMaps :: [Haxe] -> (String, [SMap])
prettyPrintHaxeWithSourceMaps js =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  in (s, mp)

prettyPrintHaxe :: [Haxe] -> String
prettyPrintHaxe = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements
-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
prettyPrintHaxe' :: (Emit gen) => Haxe -> StateT PrinterState Maybe gen
prettyPrintHaxe' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState Haxe gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState Haxe gen
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val <> emit "." <> prop ]
                  , [ Wrap indexer $ \index val -> val <> emit "[" <> index <> emit "]" ]
                  , [ Wrap app $ \args val -> val <> emit "(" <> args <> emit ")" ]
                  , [ unary HaxeNew "new " ]
                  , [ Wrap lam $ \(name, args, ss) ret -> addMapping' ss <>
                      emit ("function "
                        ++ fromMaybe "" name
                        ++ "(" ++ intercalate ", " args ++ ") ")
                        <> ret ]
                  , [ Wrap typeOf $ \_ s -> emit "typeof " <> s ]
                  , [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR instanceOf $ \v1 v2 -> emit "Std.is(" <> v1 <> emit ", " <> v2 <> emit ")"]
                  , [ binary    EqualTo              "=="
                    , binary    NotEqualTo           "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  , [ Wrap conditional $ \(ss, th, el) cond -> cond <> addMapping' ss <> emit " ? " <> prettyPrintHaxe1 th <> addMapping' ss <> emit " : " <> prettyPrintHaxe1 el ]
                    ]
