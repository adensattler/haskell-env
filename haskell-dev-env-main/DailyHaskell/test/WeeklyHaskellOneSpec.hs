module WeeklyHaskellOneSpec where

import Test.Hspec
import WeeklyHaskellOne

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "removeChar" $ do
        context "removeChar 'a' 'banana'" $ do
            it "produces 'bnn'" $
                removeChar 'a' "banana" `shouldBe` "bnn"

        context "removeChar 'c' 'banana'" $ do
            it "produces 'banana'" $
                removeChar 'c' "banana" `shouldBe` "banana"
        
        context "removeChar 'c' ''" $ do
            it "produces ''" $
                removeChar 'c' "" `shouldBe` ""

    describe "removeWhitespace" $ do
        context "removeWhitespace 'Hello World\\n\\t\\r'" $ do
            it "produces 'HelloWorld'" $
                removeWhitespace "Hello World\n\t\r" `shouldBe` "HelloWorld"

        context "removeWhitespace ''" $ do
            it "produces ''" $
                removeWhitespace "" `shouldBe` ""

    describe "removePunctuation" $ do
        context "removePunctuation 'h,e.l!l?o:o;o'" $ do
            it "produces 'hello'" $
                removePunctuation "h,e.l!l?o:o;o" `shouldBe` "hellooo"
        
        context "removePunctuation ''" $ do
            it "produces ''" $
                removePunctuation "" `shouldBe` ""

    describe "charsToAscii" $ do
        context "charsToAscii 'abc'" $ do
            it "produces [97, 98, 99]" $
                charsToAscii "abc" `shouldBe` [97, 98, 99]

        context "charsToAscii []" $ do
            it "produces []" $
                charsToAscii "" `shouldBe` []

    describe "asciiToChars" $ do
        context "asciiToChars [97, 98, 99]" $ do
            it "produces 'abc'" $
                asciiToChars [97, 98, 99] `shouldBe` "abc"

        context "asciiToChars []" $ do
            it "produces []" $
                asciiToChars [] `shouldBe` []

    describe "shiftInts" $ do
        context "shiftInts 1 [2, 4, 6, 127]" $ do
            it "produces [3, 5, 7, 0]" $
                shiftInts 1 [2, 4, 6, 127] `shouldBe` [3, 5, 7, 0]
        
        context "shiftInts 1 [2, 4, 6, 127]" $ do
            it "produces [3, 5, 7, 0]" $
                shiftInts 5 [] `shouldBe` []

    describe "shiftMessage" $ do
        context "shiftMessage 1 'hello'" $ do
            it "produces 'ifmmp'" $
                shiftMessage 1 "hello" `shouldBe` "ifmmp"
        
        context "shiftMessage 1 ''" $ do
            it "produces 'ifmmp'" $
                shiftMessage 1 "" `shouldBe` ""