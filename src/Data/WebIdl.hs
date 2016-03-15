-- |

module Data.WebIdl where

import Data.WebIdl.Types

import Text.Trifecta
import Text.Parser.Expression as X
import Text.Parser.LookAhead as X
import Text.Parser.Token as X
import Text.Parser.Char as X hiding (text)
import Text.Parser.Combinators as X

import Control.Applicative

-- | 1
definitions :: Parser DefinitionList
definitions = some $ ExtendedDefinition <$> extendedAttribute <*> definition

-- | 2
definition :: Parser Definition
definition = undefined

-- | 3
callbackOrInterface :: Parser Definition
callbackOrInterface = symbol "callback" *> callbackRestOrInterface
  <|> interface

-- | 4
callbackRestOrInterface :: Parser Definition
callbackRestOrInterface = callbackRest <|> interface

-- | 5
interface :: Parser Definition
interface = symbol "interface" *>
  Interface <$> identifier <*> inheritance <*> interfaceMembers <* semi

-- | 6
partial :: Parser Definition
partial = symbol "partial" *> partialDefinition

-- | 7
partialDefinition :: Parser Definition
partialDefinition = partialInterface <|> partialDictionary

-- | 8
partialInterface :: Parser Definition
partialInterface = symbol "interface" *>
  PartialInterface <$> identifier <*> braces interfaceMembers <* semi

-- | 9
interfaceMembers :: Parser [InterfaceMember]
interfaceMembers = some $
  replace <$> extendedAttributeList <*> interfaceMember where
  replace attrs (InterfaceMember _ m) = InterfaceMember attrs m
  -- replace is a bit kludgy, but it avoids an explosion of types,
  -- while keeping these two parsers close to the spec

-- | 10
interfaceMember :: Parser InterfaceMember
interfaceMember = InterfaceConst <$> const <|> attributeOrOperation

-- | 11
dictionary :: Parser Definition
dictionary = symbol "dictionary" *>
  Dictionary <$> identifier <*> inheritance <*> braces dictionaryMembers <* semi

-- | 12
dictionaryMembers :: Parser [ DictionaryMember ]
dictionaryMembers = some $
  replace <$> extendedAttributeList <*> dictionaryMember where
  replace attrs (DictionaryMember _ ty i def) = DictionaryMember attrs ty i def

-- | 13
dictionaryMember :: Parser DictionaryMember
dictionaryMember = DictionaryMember [] <$>
  parseType <*> identifier <*> parseDefault <* semi

-- | 14
partialDictionary :: Parser Definition
partialDictionary = symbol "dictionary" *>
  PartialDictionary <$> identifier <*> braces dictionaryMembers <* semi

-- | 15
parseDefault :: Parser (Maybe Default)
parseDefault = optional $ symbol "=" *> defaultValue

-- | 16
defaultValue :: Parser Default
defaultValue = constValue <|> parseString

-- | 17
exception :: Parser Definition
exception = symbol "exception" *>
  Exception <$> identifier <*> inheritance <*> braces exceptionMembers <* semi

-- | 18
exceptionMembers :: Parser [ExceptionMember]
exceptionMembers = some $
  replace <$> extendedAttributeList <*> exceptionMember where
  replace as (ExceptionMember _ m) = ExceptionMember as m

-- | 19
inheritance :: Parser Identifier
inheritance = symbol ":" *> identifier

-- | 20
enum :: Parser Definition
enum = symbol "enum" *> Enum <$> identifier <*> braces enumValueList  <* semi

-- | 21 && 22
enumValueList :: [EnumValue]
enumValueList = sepBy1 string comma

-- | 23
callbackRest :: Parser Definition
callbackRest =
  Callback <$> identifier <*> returnType <*> parens argumentList <* semi

-- | 24
typedef :: Parser Definition
typedef = symbol "typedef" *>
  Typedef <$> extendedAttributeList <*> parseType <*> identifier <* semi

-- | 25
implementsStatement :: Parser Definition
implementsStatement = ImplementsStatement <$> identifier <*>
  ( symbol "implements" *> identifier )

-- | 26
const :: Parser Definition
const :: symbol "const" *>
         Const <$> constType <*> identifier <*> ( symbol "=" *> constValue <* semi )

-- | 27
constValue :: Parser Const
constValue = ConstBool <$> booleanLiteral
  <|> ConstFloat  <$> floatLiteral
  <|> ConstInt <$> integer
  <|> symbol "null" *> Null

-- | 28
booleanLiteral :: Parser Bool
booleanLiteral = symbol "true" *> True <|> symbol "false" *> False

-- | 29
floatLiteral :: Parser Double
floatLiteral = double <|> ( symbol "-" *> symbol "Infinity" $> (-1)/0 )
  <|> symbol "Infinity" $> 1/0 <|> symbol "NaN" $> 0/0

-- | 30
attributeOrOperation :: Parser InterfaceMember
attributeOrOperation = symbol "stringifier" *> stringifierAttributeOrOperation
  <|> attribute <|> operation

-- | 31
stringifierAttributeOrOperation :: Parser InterfaceMember
stringifierAttributeOrOperation :: attribute <|> operationRest <|> semi

-- | 32
attribute :: Parser InterfaceMember
attribute :: Attribute <$> inherit <*> readOnly <*> parseType <*> identifier <* semi

-- | 33
inherit :: Parser Bool
inherit = option False $ symbol "inherit" *> True

-- | 34 :: Parser Bool
readOnly :: Parser Bool
readOnly = option False $ symbol "readonly" *> True

-- | 35
operation :: Parser InterfaceMember
operation = replace <$> qualifiers <*> operationRest where
  replace qs (Operation _ ty i args) = Operation qs ty i args

-- | 36
qualifiers :: Parser Qualifiers
qualifiers = symbol "static" *> Static <|> Special <$> specials

-- | 37
specials :: Parser [Special]
specials = some special

-- | 38
special :: Parser Special
special = symbol "getter" *> Getter
  <|> symbol "setter" *> Setter
  <|> symbol "creator" *> Creator
  <|> symbol "deleter" *> Deleter
  <|> symbol "legacycaller" *> LegacyCaller

-- | 39 & 40
operationRest :: Parser InterfaceMember
operationRest = Operation [] <$> returnType <*> (optional identifier) <*>
  parens argumentList

-- | 41 & 42
argumentList :: Parser [Argument]
argumentList = sepBy1 argument comma

-- | 43
argument :: Parser Argument
argument = replace <$> extendedAttributeList <*> optionalOrRequiredArgument where
  replace attrs (
  Argument <$> extendedAttributeList <*> optionalOrRequiredArgument

-- | 44
optionalOrRequiredArgument :: Parser Argument
optionalOrRequiredArgument = (symbol "optional" *>
  Optional <$> parseType <*> argumentName <*> parseDefault)
  <|> Required <$> parseType <*> ellipsis <*> argumentName

-- | 45
argumentName :: Parser Identifier
argumentName = argumentNameKeyword <|> identifier

-- | 46
ellipsis :: Parser Bool
ellipsis = option False $ symbol "..." *> True

-- | 47 & 48
exceptionMember :: Parser ExceptionMember
exceptionMember = ExceptionConst <$> const <|>
  ExceptionField <$> parseType <*> identifier <* semi

-- | 49 & 50
extendedAttributeList :: Parser [ExtendedAttribute]
extendedAttributeList = brackets $ sepBy1 extendedAttribute comma

-- | 51
extendedAttribute :: Parser ExtendedAttribute
extendedAttribute = choice
  [ ExtendedAttribute <$>  parens extendedAttributeInner <*> extendedAttributeRest
  , ExtendedAttribute <$>  brackets extendedAttributeInner <*> extendedAttributeRest
  , ExtendedAttribute <$>  braces extendedAttributeInner <*> extendedAttributeRest
  , other extendedAttributeRest
  ]

-- | 54
other =

-- | 55
argumentNameKeyword :: Parser Identifier
argumentNameKeyword = Identifier <$> choice names where
  names = map symbol
    [ "attribute", "callback", "const", "creator", "deleter",
      "dictionary", "enum", "exception", "getter", "implements", "inherit",
      "interface", "legacycaller", "partial", "setter", "static",
      "stringifier", "typedef", "unrestricted"
    ]
