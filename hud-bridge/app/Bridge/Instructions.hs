module Bridge.Instructions where

import Prelude

import Language.PureScript.Bridge

import Data.Function (on)
import Data.List (intercalate, nub)
import Data.Monoid

data Instructions (a :: Language) =
  Instructions
    { appendInstructions :: Maybe [AppendInstruction a]
    , exportInstructions :: Maybe [ExportInstruction a]
    , importInstructions :: Maybe [ImportInstruction a]
    }

instance Monoid (Instructions 'PureScript) where
    -- `Instructions 'PureScript` has both, an identity element
    -- (the empty or no instructions element, which uses Just []) and an
    -- absorbing or zero element (the manual instructions element, which uses
    -- Nothing)
    mempty  = Instructions { appendInstructions = Just []
                           , exportInstructions = Just []
                           , importInstructions = Just []
                           }
    mappend = combineInstructions

-- | Manual append, export, and import instructions.

manualInstructions :: Instructions (a :: Language)
manualInstructions =
  Instructions
    { appendInstructions = Nothing
    , exportInstructions = Nothing
    , importInstructions = Nothing
    }

-- | No append, export, and import instructions.

emptyInstructions :: Instructions (a :: Language)
emptyInstructions =
  Instructions
    { appendInstructions = Just []
    , exportInstructions = Just []
    , importInstructions = Just []
    }

-- | Append instructions.

data family AppendInstruction (a :: Language)

data instance AppendInstruction 'PureScript
  = InstanceDecodeJson String [String]
  | InstanceEncodeJson String [String]
  | InstanceOrd String [String]
  | InstanceEq String [String]

-- | Export instructions.

data family ExportInstruction (a :: Language)

data instance ExportInstruction 'PureScript
  = OtherExport String

-- | Import instructions.

data family ImportInstruction (a :: Language)

data instance ImportInstruction 'PureScript
  = ImportArgonautAeson
  | ImportArgonautDecodeJson
  | ImportArgonautEncodeJson
  | OtherImport String
  deriving (Eq)

-- | Append 'Eq' instances.

eqInstructions :: String -> [String] -> Instructions 'PureScript
eqInstructions psTypeName args =
  emptyInstructions
    { appendInstructions =
        Just
          [ InstanceEq psTypeName args
          ]
    , importInstructions = Just []
    }

-- | Append 'Ord' instances.

ordInstructions :: String -> [String] -> Instructions 'PureScript
ordInstructions psTypeName args =
  emptyInstructions
    { appendInstructions =
        Just
          [ InstanceOrd psTypeName args]
    , importInstructions = Just []
    }


-- | JSON instructions, that is, import and append 'DecodeJson' and
-- 'EncodeJson' instances.

jsonInstructions :: String -> Instructions 'PureScript
jsonInstructions psTypeName = jsonWithParametersInstructions psTypeName []

-- | Variant of JSON instructions that support parametrized types

jsonWithParametersInstructions :: String -> [String] -> Instructions 'PureScript
jsonWithParametersInstructions psTypeName parameters =
  emptyInstructions
    { appendInstructions =
        Just
          [ InstanceDecodeJson psTypeName parameters
          , InstanceEncodeJson psTypeName parameters
          ]
    , importInstructions =
        Just
          [ ImportArgonautDecodeJson
          , ImportArgonautEncodeJson
          , ImportArgonautAeson
          ]
    }

-- | Combine instructions for a module.

combineInstructions
  :: Instructions 'PureScript
  -> Instructions 'PureScript
  -> Instructions 'PureScript
combineInstructions ins1 ins2 =
  Instructions
    { appendInstructions =
        on combineAppendInstructions appendInstructions ins1 ins2
    , exportInstructions =
        on combineExportInstructions exportInstructions ins1 ins2
    , importInstructions =
        on combineImportInstructions importInstructions ins1 ins2
    }

-- | Combine append instructions for a module.

combineAppendInstructions
  :: Maybe [AppendInstruction 'PureScript]
  -> Maybe [AppendInstruction 'PureScript]
  -> Maybe [AppendInstruction 'PureScript]
combineAppendInstructions (Just ins1) (Just ins2) = Just (ins1 <> ins2)
combineAppendInstructions _ _ = Nothing

-- | Combine export instructions for a module.

combineExportInstructions
  :: Maybe [ExportInstruction 'PureScript]
  -> Maybe [ExportInstruction 'PureScript]
  -> Maybe [ExportInstruction 'PureScript]
combineExportInstructions (Just ins1) (Just ins2) = Just (ins1 <> ins2)
combineExportInstructions _ _ = Nothing

-- | Combine import instructions for a module.
--
-- Note: removes duplicate imports.

combineImportInstructions
  :: Maybe [ImportInstruction 'PureScript]
  -> Maybe [ImportInstruction 'PureScript]
  -> Maybe [ImportInstruction 'PureScript]
combineImportInstructions (Just ins1) (Just ins2) = Just (nub (ins1 <> ins2))
combineImportInstructions _ _ = Nothing

-- | Combine instructions for a module allowing us to avoid repeating the type
-- name
-- Example
-- 1.
--
--     combine [jsonInstructions, ordInstructions] "SchoolClassification"

combine :: [String -> Instructions 'PureScript] -> String -> Instructions 'PureScript
combine fis psTypeName  = mconcat $ (\f -> f psTypeName) <$> fis

-- | Generate exports given instructions.

genExports :: Instructions 'PureScript -> String
genExports = maybe mempty genExports' . exportInstructions
  where
    genExports' [] = mempty
    genExports' instructions =
      "(\n" <> genExports'' instructions <> "\n)"
    genExports'' =
      intercalate ",\n" . fmap genExport

-- | Generate an export given instructions.

genExport :: ExportInstruction 'PureScript -> String
genExport exportInstruction =
  case exportInstruction of
    OtherExport otherExport ->
      otherExport

-- | Generate imports given instructions.

genImports :: Instructions 'PureScript -> String
genImports =
  maybe mempty (intercalate "\n" . fmap genImport) . importInstructions

-- | Generate an import given instructions.

genImport :: ImportInstruction 'PureScript -> String
genImport importInstruction =
  case importInstruction of
    ImportArgonautAeson ->
      "import Data.Argonaut.Generic.Aeson as Aeson"
    ImportArgonautDecodeJson ->
      "import Data.Argonaut.Decode (class DecodeJson, decodeJson)"
    ImportArgonautEncodeJson ->
      "import Data.Argonaut.Encode (class EncodeJson, encodeJson)"
    OtherImport otherImport ->
      "import " <> otherImport

-- | Generate appends given instructions.

genAppends :: Instructions 'PureScript -> String
genAppends =
  maybe mempty (intercalate "\n" . fmap genAppend) . appendInstructions

-- | Generate an append given instructions.

genAppend :: AppendInstruction 'PureScript -> String
genAppend appendInstruction =
  case appendInstruction of
    InstanceDecodeJson name vars ->
      mconcat
        [ "instance decode"
        , name
        , " :: "
        , mkConstraints "Generic" vars  -- This is "Generic" because of limitations on Argonaut/Aeson
                                        -- Ideally it should be EncodeJson/DecodeJson
        ,"DecodeJson "
        , mkType name vars
        , " where\n    decodeJson = Aeson.decodeJson"
        ]
    InstanceEncodeJson name vars ->
      mconcat
        [ "instance encode"
        , name
        , " :: "
        , mkConstraints "Generic" vars
        , "EncodeJson "
        , mkType name vars
        , " where\n    encodeJson = Aeson.encodeJson"
        ]
    InstanceEq name vars ->
      mconcat
        [ "derive instance eq"
        , name
        , " :: "
        , mkConstraints "Eq" vars
        , "Eq "
        , mkType name vars
        ]
    InstanceOrd name vars ->
      mconcat
        [ "derive instance ord"
        , name
        , " :: "
        , mkConstraints "Ord" vars
        , "Ord "
        , mkType name vars
        ]
  where
    mkConstraints :: String -> [String] -> String
    mkConstraints _ [] = ""
    mkConstraints constraintName vars = "(" ++ (intercalate ", " $ map (\x -> constraintName ++ " " ++ x) vars) ++ ") => "
    mkType :: String -> [String] -> String
    mkType t [] = t
    mkType t vars = "(" ++ (unwords $ t : vars) ++ ")"
