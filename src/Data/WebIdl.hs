-- |

module Data.WebIdl where

import Text.Parser.Expression as X
import Text.Parser.LookAhead as X
import Text.Parser.Token as X
import Text.Parser.Char as X hiding (text)
import Text.Parser.Combinators as X

data Definition
    = DefCallback Callback
    | Interface String String [InterfaceMember]
    | PartialInterface String [ExtendedAttribute] [InterfaceMember]
    | PartialDictionary
    | Dictionary Identifier Inheritance [ DictionaryMember ]
    | DefException
    | DefEnum
    | DefTypedef
    | DefImplementsStatement

type DefinitionList = [ ExtendedDefinition ]

data ExtendedDefinition = ExtendedDefinition ExtendedAttribute Definition

data ExtendedAttribute

data Qualifiers = Static | Special [Special]

data Special = Getter | Setter | Creator | Deleter | LegacyCaller

data InterfaceMember = Const Type String WIP
  | Attribute WIP
  | Operation Type String WIP

data WIP

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
callbackRestOrInterface = callbackRest <|> interface

-- | 5
interface = symbol "interface" *>
  Interface <$> identifier <*> inheritance <*> interfaceMembers <* semi

-- | 6
partial = symbol "partial" *> partialDefinition

-- | 7
partialDefinition = partialInterface <|> partialDictionary

-- | 8
partialInterface = symbol "interface" *>
  PartialInterface <$> identifier ( symbol "{" *> interfaceMembers )
  <* symbol "}" <* semi

-- | 9
interfaceMembers = symbol "{" *>
  InterfaceMembers <$> extendedAttributeList <*> some interfaceMember
  <* symbol "}"

-- | 10
interfaceMember = const <|> attributeOrOperation

-- | 11
dictionary :: Parser Definition
dictionary = symbol "dictionary" *>
  Dictionary <$> identifier <*> inheritance <*> ( symbol "{" *> dictionaryMembers <* symbol "}" )

-- | 13
dictionaryMembers :: Parser [ DictionaryMember ]
dictionaryMembers = some dictionaryMembers


-- | 49
extendedAttributeList :: Parser [ExtendedAttribute]
extendedAttributeList = undefined

-- | 36
qualifiers :: Parser Qualifiers
qualifiers = symbol "static" *> Static <|> specials

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

-- | 46
ellipsis :: Parser ()
ellipsis = symbol "..."

-- | 55
argumentNameKeyword = choice $ map symbol
  [ "attribute", "callback", "const", "creator", "deleter",
    "dictionary", "enum", "exception", "getter", "implements", "inherit",
    "interface", "legacycaller", "partial", "setter", "static",
    "stringifier", "typedef", "unrestricted"
  ]
