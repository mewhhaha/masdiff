module Font
  ( FontKind (..),
    FontSource (..),
    FontCase (..),
    fontFilePath,
    fontInputArgs,
    fontInputLabel,
    interHarnessFontCases,
    interOracleFontCases,
    interHarnessGlyphs,
  )
where

import Data.List (intercalate)

data FontKind = Static | Variable
  deriving (Eq, Show)

data FontSource
  = StaticFont FilePath
  | VariableFont FilePath [(String, String)]
  deriving (Eq, Show)

data FontCase = FontCase
  { fontCaseId :: String,
    fontCaseKind :: FontKind,
    fontCaseSource :: FontSource
  }
  deriving (Eq, Show)

fontFilePath :: FontCase -> FilePath
fontFilePath FontCase {fontCaseSource = StaticFont path} = path
fontFilePath FontCase {fontCaseSource = VariableFont path _} = path

fontInputArgs :: FontCase -> [String]
fontInputArgs FontCase {fontCaseSource = StaticFont path} = ["-font", path]
fontInputArgs FontCase {fontCaseSource = VariableFont path axes} =
  ["-varfont", path <> "?" <> encodeAxes axes]

fontInputLabel :: FontCase -> String
fontInputLabel FontCase {fontCaseSource = StaticFont path} = "font:" <> path
fontInputLabel FontCase {fontCaseSource = VariableFont path axes} =
  "varfont:" <> path <> "?" <> encodeAxes axes

encodeAxes :: [(String, String)] -> String
encodeAxes = intercalate "&" . fmap (\(name, value) -> name <> "=" <> value)

interHarnessGlyphs :: [Char]
interHarnessGlyphs =
  [ 'A',
    'B',
    'C',
    'a',
    'e',
    'g',
    'j',
    'm',
    'W',
    'Q',
    '0',
    '1',
    '&',
    '@',
    '%',
    '?',
    '$',
    '/'
  ]

interHarnessFontCases :: [FontCase]
interHarnessFontCases =
  [ variableCase "inter-var-roman-w100-o14" romanVar [("wght", "100"), ("opsz", "14")],
    variableCase "inter-var-roman-w400-o14" romanVar [("wght", "400"), ("opsz", "14")],
    variableCase "inter-var-roman-w700-o14" romanVar [("wght", "700"), ("opsz", "14")],
    variableCase "inter-var-roman-w900-o14" romanVar [("wght", "900"), ("opsz", "14")],
    variableCase "inter-var-roman-w400-o32" romanVar [("wght", "400"), ("opsz", "32")],
    variableCase "inter-var-roman-w900-o32" romanVar [("wght", "900"), ("opsz", "32")],
    variableCase "inter-var-italic-w100-o14" italicVar [("wght", "100"), ("opsz", "14")],
    variableCase "inter-var-italic-w400-o14" italicVar [("wght", "400"), ("opsz", "14")],
    variableCase "inter-var-italic-w700-o14" italicVar [("wght", "700"), ("opsz", "14")],
    variableCase "inter-var-italic-w900-o32" italicVar [("wght", "900"), ("opsz", "32")],
    staticCase "inter-18-thin" "assets/Inter/static/Inter_18pt-Thin.ttf",
    staticCase "inter-18-regular" "assets/Inter/static/Inter_18pt-Regular.ttf",
    staticCase "inter-18-bold" "assets/Inter/static/Inter_18pt-Bold.ttf",
    staticCase "inter-18-black" "assets/Inter/static/Inter_18pt-Black.ttf",
    staticCase "inter-18-italic" "assets/Inter/static/Inter_18pt-Italic.ttf",
    staticCase "inter-18-bolditalic" "assets/Inter/static/Inter_18pt-BoldItalic.ttf",
    staticCase "inter-24-light" "assets/Inter/static/Inter_24pt-Light.ttf",
    staticCase "inter-24-regular" "assets/Inter/static/Inter_24pt-Regular.ttf",
    staticCase "inter-24-semibold" "assets/Inter/static/Inter_24pt-SemiBold.ttf",
    staticCase "inter-24-bolditalic" "assets/Inter/static/Inter_24pt-BoldItalic.ttf",
    staticCase "inter-24-blackitalic" "assets/Inter/static/Inter_24pt-BlackItalic.ttf",
    staticCase "inter-28-thinitalic" "assets/Inter/static/Inter_28pt-ThinItalic.ttf",
    staticCase "inter-28-medium" "assets/Inter/static/Inter_28pt-Medium.ttf",
    staticCase "inter-28-blackitalic" "assets/Inter/static/Inter_28pt-BlackItalic.ttf"
  ]
  where
    romanVar = "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
    italicVar = "assets/Inter/Inter-Italic-VariableFont_opsz,wght.ttf"

interOracleFontCases :: [FontCase]
interOracleFontCases = filter oracleCompatible interHarnessFontCases
  where
    oracleCompatible fontCase =
      case fontCaseSource fontCase of
        StaticFont _ -> True
        VariableFont _ axes ->
          lookup "wght" axes == Just "400"
            && lookup "opsz" axes == Just "14"

staticCase :: String -> FilePath -> FontCase
staticCase ident path =
  FontCase
    { fontCaseId = ident,
      fontCaseKind = Static,
      fontCaseSource = StaticFont path
    }

variableCase :: String -> FilePath -> [(String, String)] -> FontCase
variableCase ident path axes =
  FontCase
    { fontCaseId = ident,
      fontCaseKind = Variable,
      fontCaseSource = VariableFont path axes
    }
