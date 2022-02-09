{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "free"
  , "language-cst-parser"
  , "language-cst-parser-lenses"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "partial"
  , "prelude"
  , "psci-support"
  , "safe-coerce"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
