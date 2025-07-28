module Wire.EmailSubsystem.TemplateSpec (spec) where

import Imports
import Test.Hspec
import Text.Mustache
import Text.Mustache.Render (SubstitutionError (VariableNotFound))

data Hello = Hello
  { name :: Text,
    foo :: Text
  }
  deriving (Show, Generic)

instance ToMustache Hello where
  toMustache (Hello {name, foo}) =
    object
      [ "name" ~> name,
        "foo" ~> foo
      ]

spec :: Spec
spec = do
  describe "mustache exploratory test" $ do
    it "renders mustache templates" $ do
      -- note that 'boo' is not defined in 'Hello' and 'foo' is not used in the template
      -- this should result in a VariableNotFound error for 'foo'
      -- and the template should render with 'name' only
      let templateText = "Hello, ${name}!${boo}"
          templateTextRaw = "Hello, ${{name}}!${boo}"
          -- using a custom delimiter
          eTplWithEscaping = compileTemplate "example" ("{{= ${ } =}}" <> templateText)
          eTplWithoutEscaping = compileTemplate "example" ("{{= ${ } =}}" <> templateTextRaw)
      case (eTplWithEscaping, eTplWithoutEscaping) of
        (Left err, _) -> expectationFailure $ "Unexpected errors: " <> show err
        (_, Left err) -> expectationFailure $ "Unexpected errors: " <> show err
        (Right tplWithEscaping, Right tplWithoutEscaping) -> do
          let (errs1, renderedEscaped) = checkedSubstitute tplWithEscaping (Hello {name = "<b>World</b>", foo = "<i>Foo</i>"})
              (errs2, renderedUnescaped) = checkedSubstitute tplWithoutEscaping (Hello {name = "<b>World</b>", foo = "<i>Foo</i>"})
           in do
                renderedEscaped `shouldBe` "Hello, &lt;b&gt;World&lt;/b&gt;!"
                renderedUnescaped `shouldBe` "Hello, <b>World</b>!"
                case errs1 of
                  [VariableNotFound ["boo"]] -> pure ()
                  _ -> expectationFailure $ "Unexpected errors: " <> show errs2
                case errs2 of
                  [VariableNotFound ["boo"]] -> pure ()
                  _ -> expectationFailure $ "Unexpected errors: " <> show errs1
