-- |

module Data.WebIdl.Types where

data Definition
    = Callback
    | Interface String String [InterfaceMember]
    | PartialInterface String [ExtendedAttribute] [InterfaceMember]
    | PartialDictionary
    | Dictionary Identifier Inheritance [ DictionaryMember ]
    | Exception
    | Enum
    | Typedef
    | ImplementsStatement

type DefinitionList = [ ExtendedDefinition ]

data ExtendedDefinition = ExtendedDefinition [ExtendedAttribute] Definition

data ExtendedAttribute = ExtendedAttribute WIP WIP

data Qualifiers = Static | Special [Special]

data Special = Getter | Setter | Creator | Deleter | LegacyCaller

data InterfaceMember = InterfaceConst Const
  | Attribute Bool Bool Type Identifier
  | Operation Qualifiers Type (Maybe Identifier)

data Const = ConstBool Boolean | ConstFloat Float | ConstInt Int | Null

data Argument = Optional Type Identifier Default
  | Required Type Bool Identifier

data DictionaryMember
data Identifier = Identifier String
data EnumValue = EnumValue String
data Inheritance
data Type
data WIP
