{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff-promise"
    , "affjax"
    , "argonaut"
    , "arrays"
    , "console"
    , "debug"
    , "effect"
    , "foreign"
    , "halogen"
    , "halogen-hooks"
    , "halogen-hooks-extra"
    , "halogen-select"
    , "parsing"
    , "psci-support"
    , "string-parsers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
