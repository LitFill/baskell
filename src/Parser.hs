module Parser where

import Data.Text (Text)

-- | the ast of neo haskell
data Module = Module
    { modulename    :: Text
        -- ^ from @module 'Name'@
    , moduleexports :: [Text]
        -- ^ from @export : '...'@ space separated names
    , imports       :: [(Text, Maybe Text, [Text])]
        -- ^ from @import : \n'...'@ newline separated, args: @module name@, optional @rename@, list of imported @decls@
    , openimports   :: [(Text, [Text])]
        -- ^ from @open : \n'...'@ newline separated, args: @module name@, list of imported @decls@
    , moduledecls   :: [Declaration]
        -- ^ list of this module declarations
    } deriving (Eq, Show)

data Declaration
    = NewtypeDecl Text [Text] Text (Maybe Text) Text [Text]
        -- ^ args: newtype @name@, list of @type var@, @constructor@, optional field @name@, constructor @field@, list of @deriving@
    | TypeAliasDecl Text [Text] Text [Text]
        -- ^ args: type alias @name@, list of @type var@, real @type@ name, list of @type arg@
    | DataDecl DataDeclaration
        -- ^ delegates to @DataDeclaration@
    | ClassDecl [Text] Text [Text] [ClassMember]
        -- ^ args: list of @constraint@, the class @name@, list of class @arg@, list of class @member@
    deriving (Eq, Show)

data DataConstr
    = CoproductConstr Text [Text]
    | ProductConstr   Text [(Text, Text)]
    deriving (Eq, Show)

data DataDeclaration
    = CoproductData Text [Text] [DataConstr] [Text]
        -- ^ args: type @name@, list of @type var@, list of delegated data @constructor@, list of @deriving@
    | ProductData Text [Text] Text [(Text, Text)] [Text]
        -- ^ args: type @name@, list of @type var@, @constructor@, list of @(field, type)@, list of @deriving@
    deriving (Eq, Show)

data ClassMember = ClassMember
    { cmname :: Text
    , cmtype :: Text
    , cmdefaulttype :: Maybe Text
    , cmbinding :: Maybe Binding
    } deriving (Eq, Show)
