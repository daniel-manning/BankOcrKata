import Test.Hspec
import Test.QuickCheck
import Lib

one = ["   ",
       "  |",
       "  |"]
two = [" _ ",
       " _|",
       "|_ "]
three = [" _ ",
         " _|",
         " _|"]
four = ["   ",
        "|_|",
        "  |"]
five = [" _ ",
        "|_ ",
        " _|"]
six =  [" _ ",
        "|_ ",
        "|_|"]
seven = [" _ ",
         "  |",
         "  |"]
eight = [" _ ",
         "|_|",
         "|_|"]
nine = [" _ ",
        "|_|",
        " _|"]
zero = [" _ ",
        "| |",
        "|_|"]



main :: IO ()
main = hspec $ do
  describe "digit recognition" $ do
    it "should return the correct digit - 1" $ do
      createDigit one `shouldBe` Just 1

    it "should return the correct digit - 2" $ do
      createDigit two `shouldBe` Just 2

    it "should return the correct digit - 3" $ do
      createDigit three `shouldBe` Just 3

    it "should return the correct digit - 4" $ do
      createDigit four `shouldBe` Just 4

    it "should return the correct digit - 5" $ do
      createDigit five `shouldBe` Just 5

    it "should return the correct digit - 6" $ do
      createDigit six `shouldBe` Just 6

    it "should return the correct digit - 7" $ do
      createDigit seven `shouldBe` Just 7

    it "should return the correct digit - 8" $ do
      createDigit eight `shouldBe` Just 8

    it "should return the correct digit - 9" $ do
      createDigit nine `shouldBe` Just 9

    it "should return the correct digit - 0" $ do
      createDigit zero `shouldBe` Just 0

  describe "splitter" $ do
    it "should split up characters" $ do
      g  [[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]],[[13,14,15],[16,17,18]]] `shouldBe` [[[1,2,3],[7,8,9],[13,14,15]],[[4,5,6],[10,11,12],[16,17,18]]]

  describe "account recognition" $ do
    it "should be able to read line" $ do
     readAccount [" _  _  _  _  _  _  _  _  _ ",
                  "| || || || || || || || || |",
                  "|_||_||_||_||_||_||_||_||_|"] `shouldBe` "000000000"

    it "should be able to read line" $ do
     readAccount  ["                           ",
                   "  |  |  |  |  |  |  |  |  |",
                   "  |  |  |  |  |  |  |  |  |"] `shouldBe` "111111111"
    it "should be able to read line" $ do
     readAccount  [" _  _  _  _  _  _  _  _  _ ",
                   " _| _| _| _| _| _| _| _| _|",
                   "|_ |_ |_ |_ |_ |_ |_ |_ |_ "] `shouldBe` "222222222"

    it "should be able to read line" $ do
      readAccount  [" _  _  _  _  _  _  _  _  _ ",
                    " _| _| _| _| _| _| _| _| _|",
                    " _| _| _| _| _| _| _| _| _|"] `shouldBe` "333333333"

    it "should be able to read line" $ do
     readAccount  ["                           ",
                   "|_||_||_||_||_||_||_||_||_|",
                   "  |  |  |  |  |  |  |  |  |"] `shouldBe` "444444444"

    it "should be able to read line" $ do
     readAccount  [" _  _  _  _  _  _  _  _  _ ",
                   "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
                   " _| _| _| _| _| _| _| _| _|"] `shouldBe` "555555555"

    it "should be able to read line" $ do
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
                       "|_||_||_||_||_||_||_||_||_|"] `shouldBe` "666666666"

    it "should be able to read line" $ do
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "  |  |  |  |  |  |  |  |  |",
                       "  |  |  |  |  |  |  |  |  |"] `shouldBe` "777777777"

    it "should be able to read line" $ do
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "|_||_||_||_||_||_||_||_||_|",
                       "|_||_||_||_||_||_||_||_||_|"] `shouldBe` "888888888"

    it "should be able to read line" $ do
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "|_||_||_||_||_||_||_||_||_|",
                       " _| _| _| _| _| _| _| _| _|"] `shouldBe` "999999999"

    it "should be able to read line" $ do
         readAccount  ["    _  _     _  _  _  _  _ ",
                       "  | _| _||_||_ |_   ||_||_|",
                       "  ||_  _|  | _||_|  ||_| _|"] `shouldBe` "123456789"

    it "should be able to read line" $ do
         readAccount  [" _  _  _  _  _  _  _  _    ",
                       "| || || || || || || ||_   |",
                       "|_||_||_||_||_||_||_| _|  |"]  `shouldBe` "000000051"

  describe "checksum calculation" $ do
    it "should calculate the correct checksum" $ do
      checksum [3,4,5,8,8,2,8,6,5] `shouldBe` 0

    it "should be a valid checksum" $ do
      isValid [3,4,5,8,8,2,8,6,5] `shouldBe` True

    it "should validate account strings" $ do
      validateString "345882865" `shouldBe` True