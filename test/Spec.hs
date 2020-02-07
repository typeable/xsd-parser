{-# LANGUAGE OverloadedStrings #-}

module Spec
( spec
)
where

import Data.Text (Text)
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML

import qualified Xsd

import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    context "when targetNamespace is given" $ do
      it "parses the namespace" $ do
        let
          doc = mkDoc $
            mkElem "schema"
              (Map.fromList[("targetNamespace", "something")])
                []
          Right xsd = Xsd.parse Xsd.defaultConfig doc
        Xsd.targetNamespace xsd `shouldBe` Just (Xsd.Namespace "something")

    context "when targetNamespace is not given" $ do
      it "returns Nothing" $ do
        let
          doc = mkDoc $
            mkElem "schema"
              (Map.fromList [])
                []
          Right xsd = Xsd.parse Xsd.defaultConfig doc
        Xsd.targetNamespace xsd `shouldBe` Nothing

    context "when root element is not schema" $ do
      context "when root element has different name" $ do
        it "fails" $ do
          let
            doc = mkDoc $
              mkElem "notschema"
                (Map.fromList [])
                  []
            Left err = Xsd.parse Xsd.defaultConfig doc
          Xsd.errorLocation err `shouldBe` "notschema"

      context "when root element has different namespace" $ do
        it "fails" $ do
          let
            doc = mkDoc $
              Element "{othernamespace}schema"
                (Map.fromList [])
                  []
            Left err = Xsd.parse Xsd.defaultConfig doc
          Xsd.errorLocation err `shouldBe` "schema"

        context "when ignoring schema namespace" $ do
          it "succeeds" $ do
            let
              doc = mkDoc $
                Element "{othernamespace}schema"
                  (Map.fromList [])
                    []
              res = Xsd.parse conf doc
              conf = Xsd.defaultConfig
                { Xsd.configIgnoreSchemaNamespace = True
                }
            res `shouldSatisfy` isRight

    context "when simple type is given" $ do
      context "when it's atomic with restriction" $ do
        it "parses it" $ do
          let
            doc = mkDoc $
              mkElem "schema"
                (Map.fromList [("xmlns:xs","something")])
                [NodeElement $ mkElem "simpleType"
                  (Map.fromList [("name", "str")])
                  [NodeElement $ mkElem "restriction"
                    (Map.fromList [("base", "xs:string")])
                    []]]
            res = Xsd.parse Xsd.defaultConfig doc
            Right [Xsd.ChildType tpName (Xsd.TypeSimple t)] =
              fmap Xsd.children res
            Xsd.AtomicType restriction _ = t
          res `shouldSatisfy` isRight
          tpName `shouldBe` (Xsd.QName Nothing "str")
          Xsd.restrictionBase restriction `shouldBe`
            Xsd.Ref (Xsd.QName (Just (Xsd.Namespace "something")) "string")

        context "when restriction contains enumerations" $ do
          it "parses them" $ do
            let
              doc = mkDoc $
                mkElem "schema"
                  (Map.fromList [("xmlns:xs","something")])
                  [NodeElement $ mkElem "simpleType"
                    (Map.fromList [("name", "str")])
                    [NodeElement $ mkElem "restriction"
                      (Map.fromList [("base", "xs:string")])
                      [ NodeElement $ mkElem "enumeration"
                          (Map.fromList [("value", "value1")]) []
                      , NodeElement $ mkElem "enumeration"
                          (Map.fromList [("value", "value2")]) []
                      ]]]
              res = Xsd.parse Xsd.defaultConfig doc
              Right [Xsd.ChildType tpName (Xsd.TypeSimple t)] =
                fmap Xsd.children res
              Xsd.AtomicType restriction _ = t
            res `shouldSatisfy` isRight
            tpName `shouldBe` (Xsd.QName Nothing "str")
            Xsd.restrictionConstraints restriction `shouldBe`
              [ Xsd.Enumeration "value1"
              , Xsd.Enumeration "value2"
              ]

        context "when restrication base is inlined" $ do
          it "parses it" $ do
            let
              doc = mkDoc $
                mkElem "schema"
                  (Map.fromList [("xmlns:xs","something")])
                  [NodeElement $ mkElem "simpleType"
                    (Map.fromList [("name", "str")])
                    [NodeElement $ mkElem "restriction" Map.empty
                      [NodeElement $ mkElem "simpleType" Map.empty
                        [NodeElement $ mkElem "restriction"
                          (Map.fromList [("base", "xs:string")])
                          []]]]]
              res = Xsd.parse Xsd.defaultConfig doc
              Right [Xsd.ChildType tpName (Xsd.TypeSimple t)] =
                fmap Xsd.children res
              Xsd.AtomicType restriction' _ = t
              Xsd.Inline (Xsd.AtomicType restriction _) =
                Xsd.restrictionBase restriction'
            res `shouldSatisfy` isRight
            tpName `shouldBe` (Xsd.QName Nothing "str")
            Xsd.restrictionBase restriction `shouldBe`
              Xsd.Ref (Xsd.QName (Just (Xsd.Namespace "something")) "string")

        context "when base refers to an unbound prefix" $ do
          it "fails" $ do
            let
              doc = mkDoc $
                mkElem "schema"
                  (Map.fromList [])
                  [NodeElement $ mkElem "simpleType"
                    (Map.fromList [("name", "str")])
                    [NodeElement $ mkElem "restriction"
                      (Map.fromList [("base", "xs:string")])
                      []]]
              res = Xsd.parse Xsd.defaultConfig doc
              Left err = res
            res `shouldSatisfy` isLeft
            Xsd.errorLocation err `shouldBe`
              "schema.simpleType[str].restriction"

      context "when target namespace is provided" $ do
        it "parses it" $ do
          let
            doc = mkDoc $
              mkElem "schema"
                (Map.fromList
                  [ ("xmlns:xs","something")
                  , ("targetNamespace", "me")
                  ])
                [NodeElement $ mkElem "simpleType"
                  (Map.fromList [("name", "str")])
                  [NodeElement $ mkElem "restriction"
                    (Map.fromList [("base", "xs:string")])
                    []]]
            res = Xsd.parse Xsd.defaultConfig doc
            Right [Xsd.ChildType tpName _] = fmap Xsd.children res
          res `shouldSatisfy` isRight
          tpName `shouldBe` (Xsd.QName (Just (Xsd.Namespace "me")) "str")

    context "when complex type is given" $ do
      context "when it has attributes" $ do
        context "when attribute type is a reference" $ do
          it "succeeds" $ do
            let
              doc = mkDoc $
                mkElem "schema"
                  (Map.fromList [("xmlns:xs","something")])
                  [NodeElement $ mkElem "complexType"
                    (Map.fromList [("name", "person")])
                    [NodeElement $ mkElem "attribute"
                      (Map.fromList
                        [ ("type", "xs:string")
                        , ("name", "firstName")
                        ])
                      []]]
              res = Xsd.parse Xsd.defaultConfig doc
              Right [Xsd.ChildType tpName (Xsd.TypeComplex t)] =
                fmap Xsd.children res
              Xsd.ContentPlain plain = Xsd.complexContent t
              [attr] = Xsd.plainContentAttributes plain
            res `shouldSatisfy` isRight
            tpName `shouldBe` Xsd.QName Nothing "person"
            Xsd.attrName attr `shouldBe` Xsd.QName Nothing "firstName"
            Xsd.attrType attr `shouldBe`
              Xsd.Ref (Xsd.QName (Just (Xsd.Namespace "something")) "string")

        context "when attribute type is inlined" $ do
          it "succeeds" $ do
            let
              doc = mkDoc $
                mkElem "schema"
                  (Map.fromList [("xmlns:xs","something")])
                  [NodeElement $ mkElem "complexType"
                    (Map.fromList [("name", "person")])
                    [NodeElement $ mkElem "attribute"
                      (Map.fromList [("name", "firstName")])
                      [NodeElement $ mkElem "simpleType"
                        (Map.empty)
                        [NodeElement $ mkElem "restriction"
                          (Map.fromList [("base", "xs:string")])
                          []]]]]
              res = Xsd.parse Xsd.defaultConfig doc
              Right [Xsd.ChildType _ (Xsd.TypeComplex t)] =
                fmap Xsd.children res
              Xsd.ContentPlain plain = Xsd.complexContent t
              [attr] = Xsd.plainContentAttributes plain
              Xsd.Inline (Xsd.AtomicType restriction _) = Xsd.attrType attr
            res `shouldSatisfy` isRight
            Xsd.restrictionBase restriction `shouldBe`
              Xsd.Ref (Xsd.QName (Just (Xsd.Namespace "something")) "string")

      context "when it contains a sequence" $ do
        it "succeeds" $ do
          let
            doc = mkDoc $
              mkElem "schema"
                (Map.fromList [("xmlns:xs","something")])
                [NodeElement $ mkElem "complexType"
                  (Map.fromList [("name", "person")])
                  [NodeElement $ mkElem "sequence" Map.empty
                    [ NodeElement $ mkElem "element"
                      (Map.fromList
                        [ ("name", "firstName")
                        , ("type", "xs:string")
                        ])
                      []
                    , NodeElement $ mkElem "element"
                      (Map.fromList
                        [ ("name", "lastName")
                        , ("type", "xs:string")
                        ])
                      []
                    ]]]
            res = Xsd.parse Xsd.defaultConfig doc
            Right [Xsd.ChildType _ (Xsd.TypeComplex t)] =
              fmap Xsd.children res
            Xsd.ContentPlain plain = Xsd.complexContent t
            Just (Xsd.Sequence elems) = Xsd.plainContentModel plain
          res `shouldSatisfy` isRight
          map Xsd.elementName elems `shouldBe`
            map (Xsd.QName Nothing) ["firstName", "lastName"]

    context "when element is given" $ do
      context "when type is inlined" $ do
        it "succeeds" $ do
          let
            doc = mkDoc $
              mkElem "schema"
                (Map.fromList [("xmlns:xs","something")])
                [NodeElement $ mkElem "element"
                  (Map.fromList [("name", "person")])
                  [NodeElement $ mkElem "simpleType" Map.empty
                    [NodeElement $ mkElem "restriction"
                      (Map.fromList [("base", "xs:string")])
                      []]
                  ]
                ]
            res = Xsd.parse Xsd.defaultConfig doc
            Right [Xsd.ChildElement e] = fmap Xsd.children res
          res `shouldSatisfy` isRight
          Xsd.elementName e `shouldBe`
            Xsd.QName Nothing "person"

mkDoc :: Element -> Document
mkDoc e = Document (Prologue [] Nothing []) e []

mkElem :: Text -> Map Name Text -> [Node] -> Element
mkElem n = Element (Name n (Just Xsd.schemaNamespace) Nothing)
