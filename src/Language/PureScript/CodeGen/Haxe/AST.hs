-- |
-- Data types for the intermediate simplified-Javascript AST
--
module Language.PureScript.CodeGen.Haxe.AST where

import Prelude.Compat

import Control.Monad.Identity

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.Comments
import Language.PureScript.Traversals

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  -- |
  -- Constructor
  --
  | HaxeNew
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division
  --
  | Divide
  -- |
  -- Remainder
  --
  | Modulus
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo
  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- |
  -- Bitwise right shift with zero-fill
  --
  | ZeroFillShiftRight
  deriving (Show, Eq)

-- |
-- Data type for simplified Javascript expressions
--
data Haxe
  -- |
  -- A numeric literal
  --
  = HaxeNumericLiteral (Maybe SourceSpan) (Either Integer Double)
  -- |
  -- A string literal
  --
  | HaxeStringLiteral (Maybe SourceSpan) String
  -- |
  -- A boolean literal
  --
  | HaxeBooleanLiteral (Maybe SourceSpan) Bool
  -- |
  -- A unary operator application
  --
  | HaxeUnary (Maybe SourceSpan) UnaryOperator Haxe
  -- |
  -- A binary operator application
  --
  | HaxeBinary (Maybe SourceSpan) BinaryOperator Haxe Haxe
  -- |
  -- An array literal
  --
  | HaxeArrayLiteral (Maybe SourceSpan) [Haxe]
  -- |
  -- An array indexer expression
  --
  | HaxeIndexer (Maybe SourceSpan) Haxe Haxe
  -- |
  -- An object literal
  --
  | HaxeObjectLiteral (Maybe SourceSpan) [(String, Haxe)]
  -- |
  -- An object property accessor expression
  --
  | HaxeAccessor (Maybe SourceSpan) String Haxe
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | HaxeFunction (Maybe SourceSpan) (Maybe String) [String] Haxe
  -- |
  -- A class constructor introduction (arguments, body)
  --
  | HaxeConstructor (Maybe SourceSpan) [String] Haxe
  -- |
  -- |
  -- A method introduction (name, arguments, body)
  --
  | HaxeMethod (Maybe SourceSpan) String [String] Haxe
  -- |
  -- A class member introduction (name, optional body)
  --
  | HaxeMember (Maybe SourceSpan) String (Maybe Haxe)
  -- |
  -- Function application
  --
  | HaxeApp (Maybe SourceSpan) Haxe [Haxe]
  -- |
  -- Variable
  --
  | HaxeVar (Maybe SourceSpan) String
  -- |
  -- Conditional expression
  --
  | HaxeConditional (Maybe SourceSpan) Haxe Haxe Haxe
  -- |
  -- A block of expressions in braces
  --
  | HaxeBlock (Maybe SourceSpan) [Haxe]
  -- |
  -- A variable introduction and optional initialization
  --
  | HaxeVariableIntroduction (Maybe SourceSpan) String (Maybe Haxe)
  -- |
  -- A top level import
  --
  | HaxeImport (Maybe SourceSpan) String
  -- |
  -- A package declaration
  --
  | HaxePackage (Maybe SourceSpan) String
  -- |
  -- A class declaration (name, methods)
  --
  | HaxeClass (Maybe SourceSpan) String [Haxe]
  -- |
  -- A top level import
  --
  | HaxeAssignment (Maybe SourceSpan) Haxe Haxe
  -- |
  -- An attribute introduction (name, value). Value must be a constant
  --
  | HaxeAttribute (Maybe SourceSpan) Haxe Haxe
  -- |
  -- While loop
  --
  | HaxeWhile (Maybe SourceSpan) Haxe Haxe
  -- |
  -- ForIn loop
  --
  | HaxeForIn (Maybe SourceSpan) String Haxe Haxe
  -- |
  -- If-then-else statement
  --
  | HaxeIfElse (Maybe SourceSpan) Haxe Haxe (Maybe Haxe)
  -- |
  -- Return statement
  --
  | HaxeReturn (Maybe SourceSpan) Haxe
  -- |
  -- Throw statement
  --
  | HaxeThrow (Maybe SourceSpan) Haxe
  -- |
  -- Type-Of operator
  --
  | HaxeTypeOf (Maybe SourceSpan) Haxe
  -- |
  -- InstanceOf test
  --
  | HaxeInstanceOf (Maybe SourceSpan) Haxe Haxe
  -- |
  -- Break statement
  --
  | HaxeBreak (Maybe SourceSpan) String
  -- |
  -- Continue statement
  --
  | HaxeContinue (Maybe SourceSpan) String
  -- |
  -- Raw Javascript (generated when parsing fails for an inline foreign import declaration)
  --
  | HaxeRaw (Maybe SourceSpan) String
  -- |
  -- Commented Javascript
  --
  | HaxeComment (Maybe SourceSpan) [Comment] Haxe deriving (Show, Eq)

withSourceSpan :: SourceSpan -> Haxe -> Haxe
withSourceSpan withSpan = go
  where
  ss :: Maybe SourceSpan
  ss = Just withSpan

  go :: Haxe -> Haxe
  go (HaxeNumericLiteral _ n) = HaxeNumericLiteral ss n
  go (HaxeStringLiteral _ s) = HaxeStringLiteral ss s
  go (HaxeBooleanLiteral _ b) = HaxeBooleanLiteral ss b
  go (HaxeUnary _ op j) = HaxeUnary ss op j
  go (HaxeBinary _ op j1 j2) = HaxeBinary ss op j1 j2
  go (HaxeArrayLiteral _ js) = HaxeArrayLiteral ss js
  go (HaxeIndexer _ j1 j2) = HaxeIndexer ss j1 j2
  go (HaxeObjectLiteral _ js) = HaxeObjectLiteral ss js
  go (HaxeAccessor _ prop j) = HaxeAccessor ss prop j
  go (HaxeFunction _ name args j) = HaxeFunction ss name args j
  go (HaxeConstructor _ args j) = HaxeConstructor ss args j
  go (HaxeMethod _ name args j) = HaxeMethod ss name args j
  go (HaxeMember _ name j) = HaxeMember ss name j
  go (HaxeApp _ j js) = HaxeApp ss j js
  go (HaxeVar _ s) = HaxeVar ss s
  go (HaxeConditional _ j1 j2 j3) = HaxeConditional ss j1 j2 j3
  go (HaxeBlock _ js) = HaxeBlock ss js
  go (HaxeVariableIntroduction _ name j) = HaxeVariableIntroduction ss name j
  go (HaxeAssignment _ j1 j2) = HaxeAssignment ss j1 j2
  go (HaxeAttribute _ j1 j2) = HaxeAttribute ss j1 j2
  go (HaxeWhile _ j1 j2) = HaxeWhile ss j1 j2
  go (HaxeForIn _ name j1 j2) = HaxeForIn ss name j1 j2
  go (HaxeIfElse _ j1 j2 j3) = HaxeIfElse ss j1 j2 j3
  go (HaxeReturn _ js) = HaxeReturn ss js
  go (HaxeThrow _ js) = HaxeThrow ss js
  go (HaxeTypeOf _ js) = HaxeTypeOf ss js
  go (HaxeInstanceOf _ j1 j2) = HaxeInstanceOf ss j1 j2
  go (HaxeBreak _ s) = HaxeBreak ss s
  go (HaxeContinue _ s) = HaxeContinue ss s
  go (HaxeRaw _ s) = HaxeRaw ss s
  go (HaxeComment _ com j) = HaxeComment ss com j
  go (HaxeImport _ i) = HaxeImport ss i
  go (HaxePackage _ p) = HaxePackage ss p
  go (HaxeClass _ c1 c2) = HaxeClass ss c1 c2

getSourceSpan :: Haxe -> Maybe SourceSpan
getSourceSpan = go
  where
  go :: Haxe -> Maybe SourceSpan
  go (HaxeNumericLiteral ss _) = ss
  go (HaxeStringLiteral ss _) = ss
  go (HaxeBooleanLiteral ss _) = ss
  go (HaxeUnary ss _ _) = ss
  go (HaxeBinary ss _ _ _) = ss
  go (HaxeArrayLiteral ss _) = ss
  go (HaxeIndexer ss _ _) = ss
  go (HaxeObjectLiteral ss _) = ss
  go (HaxeAccessor ss _ _) = ss
  go (HaxeFunction ss _ _ _) = ss
  go (HaxeConstructor ss _ _) = ss
  go (HaxeMethod ss _ _ _) = ss
  go (HaxeMember ss _ _) = ss
  go (HaxeApp ss _ _) = ss
  go (HaxeVar ss _) = ss
  go (HaxeConditional ss _ _ _) = ss
  go (HaxeBlock ss _) = ss
  go (HaxeVariableIntroduction ss _ _) = ss
  go (HaxeAssignment ss _ _) = ss
  go (HaxeAttribute ss _ _) = ss
  go (HaxeWhile ss _ _) = ss
  go (HaxeForIn ss _ _ _) = ss
  go (HaxeIfElse ss _ _ _) = ss
  go (HaxeReturn ss _) = ss
  go (HaxeThrow ss _) = ss
  go (HaxeTypeOf ss _) = ss
  go (HaxeInstanceOf ss _ _) = ss
  go (HaxeBreak ss _) = ss
  go (HaxeContinue ss _) = ss
  go (HaxeRaw ss _) = ss
  go (HaxeComment ss _ _) = ss
  go (HaxeImport ss _) = ss
  go (HaxePackage ss _) = ss
  go (HaxeClass ss _ _) = ss

--
-- Traversals
--

everywhereOnHaxe :: (Haxe -> Haxe) -> Haxe -> Haxe
everywhereOnHaxe f = go
  where
  go :: Haxe -> Haxe
  go (HaxeUnary ss op j) = f (HaxeUnary ss op (go j))
  go (HaxeBinary ss op j1 j2) = f (HaxeBinary ss op (go j1) (go j2))
  go (HaxeArrayLiteral ss js) = f (HaxeArrayLiteral ss (map go js))
  go (HaxeIndexer ss j1 j2) = f (HaxeIndexer ss (go j1) (go j2))
  go (HaxeObjectLiteral ss js) = f (HaxeObjectLiteral ss (map (fmap go) js))
  go (HaxeAccessor ss prop j) = f (HaxeAccessor ss prop (go j))
  go (HaxeFunction ss name args j) = f (HaxeFunction ss name args (go j))
  go (HaxeApp ss j js) = f (HaxeApp ss (go j) (map go js))
  go (HaxeConditional ss j1 j2 j3) = f (HaxeConditional ss (go j1) (go j2) (go j3))
  go (HaxeBlock ss js) = f (HaxeBlock ss (map go js))
  go (HaxeVariableIntroduction ss name j) = f (HaxeVariableIntroduction ss name (fmap go j))
  go (HaxeAssignment ss j1 j2) = f (HaxeAssignment ss (go j1) (go j2))
  go (HaxeAttribute ss j1 j2) = f (HaxeAttribute ss (go j1) (go j2))
  go (HaxeWhile ss j1 j2) = f (HaxeWhile ss (go j1) (go j2))
  go (HaxeForIn ss name j1 j2) = f (HaxeForIn ss name (go j1) (go j2))
  go (HaxeIfElse ss j1 j2 j3) = f (HaxeIfElse ss (go j1) (go j2) (fmap go j3))
  go (HaxeReturn ss js) = f (HaxeReturn ss (go js))
  go (HaxeThrow ss js) = f (HaxeThrow ss (go js))
  go (HaxeTypeOf ss js) = f (HaxeTypeOf ss (go js))
  go (HaxeInstanceOf ss j1 j2) = f (HaxeInstanceOf ss (go j1) (go j2))
  go (HaxeComment ss com j) = f (HaxeComment ss com (go j))
  go other = f other

everywhereOnHaxeTopDown :: (Haxe -> Haxe) -> Haxe -> Haxe
everywhereOnHaxeTopDown f = runIdentity . everywhereOnHaxeTopDownM (Identity . f)

everywhereOnHaxeTopDownM :: (Monad m) => (Haxe -> m Haxe) -> Haxe -> m Haxe
everywhereOnHaxeTopDownM f = f >=> go
  where
  f' = f >=> go
  go (HaxeUnary ss op j) = HaxeUnary ss op <$> f' j
  go (HaxeBinary ss op j1 j2) = HaxeBinary ss op <$> f' j1 <*> f' j2
  go (HaxeArrayLiteral ss js) = HaxeArrayLiteral ss <$> traverse f' js
  go (HaxeIndexer ss j1 j2) = HaxeIndexer ss <$> f' j1 <*> f' j2
  go (HaxeObjectLiteral ss js) = HaxeObjectLiteral ss <$> traverse (sndM f') js
  go (HaxeAccessor ss prop j) = HaxeAccessor ss prop <$> f' j
  go (HaxeFunction ss name args j) = HaxeFunction ss name args <$> f' j
  go (HaxeApp ss j js) = HaxeApp ss <$> f' j <*> traverse f' js
  go (HaxeConditional ss j1 j2 j3) = HaxeConditional ss <$> f' j1 <*> f' j2 <*> f' j3
  go (HaxeBlock ss js) = HaxeBlock ss <$> traverse f' js
  go (HaxeVariableIntroduction ss name j) = HaxeVariableIntroduction ss name <$> traverse f' j
  go (HaxeAssignment ss j1 j2) = HaxeAssignment ss <$> f' j1 <*> f' j2
  go (HaxeAttribute ss j1 j2) = HaxeAttribute ss <$> f' j1 <*> f' j2
  go (HaxeWhile ss j1 j2) = HaxeWhile ss <$> f' j1 <*> f' j2
  go (HaxeForIn ss name j1 j2) = HaxeForIn ss name <$> f' j1 <*> f' j2
  go (HaxeIfElse ss j1 j2 j3) = HaxeIfElse ss <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (HaxeReturn ss j) = HaxeReturn ss <$> f' j
  go (HaxeThrow ss j) = HaxeThrow ss <$> f' j
  go (HaxeTypeOf ss j) = HaxeTypeOf ss <$> f' j
  go (HaxeInstanceOf ss j1 j2) = HaxeInstanceOf ss <$> f' j1 <*> f' j2
  go (HaxeComment ss com j) = HaxeComment ss com <$> f' j
  go (HaxeConstructor ss args j) = HaxeConstructor ss args <$> f' j
  go (HaxeMethod ss name args j) = HaxeMethod ss name args <$> f' j
  go (HaxeMember ss name j) = HaxeMember ss name <$> traverse f' j
  go other = f other

everythingOnHaxe :: (r -> r -> r) -> (Haxe -> r) -> Haxe -> r
everythingOnHaxe (<>) f = go
  where
  go j@(HaxeUnary _ _ j1) = f j <> go j1
  go j@(HaxeBinary _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeArrayLiteral _ js) = foldl (<>) (f j) (map go js)
  go j@(HaxeIndexer _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeObjectLiteral _ js) = foldl (<>) (f j) (map (go . snd) js)
  go j@(HaxeAccessor _ _ j1) = f j <> go j1
  go j@(HaxeFunction _ _ _ j1) = f j <> go j1
  go j@(HaxeApp _ j1 js) = foldl (<>) (f j <> go j1) (map go js)
  go j@(HaxeConditional _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(HaxeBlock _ js) = foldl (<>) (f j) (map go js)
  go j@(HaxeVariableIntroduction _ _ (Just j1)) = f j <> go j1
  go j@(HaxeAssignment _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeAttribute _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeWhile _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeForIn _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeIfElse _ j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(HaxeIfElse _ j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(HaxeReturn _ j1) = f j <> go j1
  go j@(HaxeThrow _ j1) = f j <> go j1
  go j@(HaxeTypeOf _ j1) = f j <> go j1
  go j@(HaxeInstanceOf _ j1 j2) = f j <> go j1 <> go j2
  go j@(HaxeComment _ _ j1) = f j <> go j1
  go j@(HaxeConstructor _ _ j1) = f j <> go j1
  go j@(HaxeMethod _ _ _ j1) = f j <> go j1
  go j@(HaxeMember _ _ (Just j1)) = f j <> go j1
  go other = f other
