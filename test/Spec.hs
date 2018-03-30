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
    it "should return the correct digit - 1" $
      createDigit one `shouldBe` Just 1

    it "should return the correct digit - 2" $
      createDigit two `shouldBe` Just 2

    it "should return the correct digit - 3" $
      createDigit three `shouldBe` Just 3

    it "should return the correct digit - 4" $
      createDigit four `shouldBe` Just 4

    it "should return the correct digit - 5" $
      createDigit five `shouldBe` Just 5

    it "should return the correct digit - 6" $
      createDigit six `shouldBe` Just 6

    it "should return the correct digit - 7" $
      createDigit seven `shouldBe` Just 7

    it "should return the correct digit - 8" $
      createDigit eight `shouldBe` Just 8

    it "should return the correct digit - 9" $
      createDigit nine `shouldBe` Just 9

    it "should return the correct digit - 0" $
      createDigit zero `shouldBe` Just 0

  describe "splitter" $ do
    it "should split up characters" $
      g  [[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]],[[13,14,15],[16,17,18]]] `shouldBe` [[[1,2,3],[7,8,9],[13,14,15]],[[4,5,6],[10,11,12],[16,17,18]]]

  describe "account recognition" $ do
    it "should be able to read line" $
     readAccount [" _  _  _  _  _  _  _  _  _ ",
                  "| || || || || || || || || |",
                  "|_||_||_||_||_||_||_||_||_|"] `shouldBe` "000000000"

    it "should be able to read line" $
     readAccount  ["                           ",
                   "  |  |  |  |  |  |  |  |  |",
                   "  |  |  |  |  |  |  |  |  |"] `shouldBe` "111111111"

    it "should be able to read line" $
     readAccount  [" _  _  _  _  _  _  _  _  _ ",
                   " _| _| _| _| _| _| _| _| _|",
                   "|_ |_ |_ |_ |_ |_ |_ |_ |_ "] `shouldBe` "222222222"

    it "should be able to read line" $
      readAccount  [" _  _  _  _  _  _  _  _  _ ",
                    " _| _| _| _| _| _| _| _| _|",
                    " _| _| _| _| _| _| _| _| _|"] `shouldBe` "333333333"

    it "should be able to read line" $
     readAccount  ["                           ",
                   "|_||_||_||_||_||_||_||_||_|",
                   "  |  |  |  |  |  |  |  |  |"] `shouldBe` "444444444"

    it "should be able to read line" $
     readAccount  [" _  _  _  _  _  _  _  _  _ ",
                   "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
                   " _| _| _| _| _| _| _| _| _|"] `shouldBe` "555555555"

    it "should be able to read line" $
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
                       "|_||_||_||_||_||_||_||_||_|"] `shouldBe` "666666666"

    it "should be able to read line" $
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "  |  |  |  |  |  |  |  |  |",
                       "  |  |  |  |  |  |  |  |  |"] `shouldBe` "777777777"

    it "should be able to read line" $
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "|_||_||_||_||_||_||_||_||_|",
                       "|_||_||_||_||_||_||_||_||_|"] `shouldBe` "888888888"

    it "should be able to read line" $
         readAccount  [" _  _  _  _  _  _  _  _  _ ",
                       "|_||_||_||_||_||_||_||_||_|",
                       " _| _| _| _| _| _| _| _| _|"] `shouldBe` "999999999"

    it "should be able to read line" $
         readAccount  ["    _  _     _  _  _  _  _ ",
                       "  | _| _||_||_ |_   ||_||_|",
                       "  ||_  _|  | _||_|  ||_| _|"] `shouldBe` "123456789"

    it "should be able to read line" $
         readAccount  [" _  _  _  _  _  _  _  _    ",
                       "| || || || || || || ||_   |",
                       "|_||_||_||_||_||_||_| _|  |"]  `shouldBe` "000000051"

    it "should be able to read line" $
         readAccount  ["    _  _  _  _  _  _     _ ",
                       "|_||_|| || ||_   |  |  | _ ",
                       "  | _||_||_||_|  |  |  | _|"]  `shouldBe` "49006771?"

  describe "checksum calculation" $ do
    it "should calculate the correct checksum" $
      checksum [3,4,5,8,8,2,8,6,5] `shouldBe` 0

    it "should calculate the correct checksum" $
      checksum [6,6,4,3,7,1,4,9,5] `shouldBe` 2

    it "should be a valid checksum" $
      isValid [3,4,5,8,8,2,8,6,5] `shouldBe` True

    it "should validate account strings" $
      validateString "345882865" `shouldBe` True

  describe "account description" $ do
    it "should handle an ill defined string" $
      describeAccount ["    _  _  _  _  _  _     _ ",
                       "|_||_|| || ||_   |  |  | _ ",
                       "  | _||_||_||_|  |  |  | _|"] `shouldBe` "49006771? ILL"

    it "should handle a valid account" $
      describeAccount [" _  _  _  _  _  _  _  _    ",
                       "| || || || || || || ||_   |",
                       "|_||_||_||_||_||_||_| _|  |"] `shouldBe` "000000051"

    it "should handle a multiple invalid charaters in account" $
      describeAccount ["    _  _     _  _  _  _  _ ",
                       "  | _| _||_| _ |_   ||_||_|",
                       "  ||_  _|  | _||_|  ||_| _ "] `shouldBe` "1234?678? ILL"

    it "should handle an account failing validation" $
      describeAccount [" _  _     _  _        _  _ ",
                       "|_ |_ |_| _|  |  ||_||_||_ ",
                       "|_||_|  | _|  |  |  | _| _|"] `shouldBe` "664371495 ERR"

  describe "everyCharacterWithHammingDistance" $ do
    it "should find the appropriate characters" $
       everyCharacterWithHammingDistance 1 [[0,1,0],
                                            [1,1,0],
                                            [0,1,1]]  `shouldBe` [6,9]
--
--  describe "readPossibleDigits" $ do
--    it "should find the appropriate characters" $ do
--       readPossibleDigits ["    _  _     _  _  _  _  _ ",
--                           "  | _| _||_| _ |_   ||_||_|",
--                           "  ||_  _|  | _||_|  ||_| _ "]  `shouldBe` [[1],[2],[3],[4],[3,5],[6],[7],[8],[9]]

  describe "readAndTestAccount" $ do
    it "should find the appropriate string" $
       readAndTestAccount ["                           ",
                           "  |  |  |  |  |  |  |  |  |",
                           "  |  |  |  |  |  |  |  |  |"]  `shouldBe` "711111111"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           "  |  |  |  |  |  |  |  |  |",
                           "  |  |  |  |  |  |  |  |  |"]  `shouldBe` "777777177"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           " _|| || || || || || || || |",
                           "|_ |_||_||_||_||_||_||_||_|"]  `shouldBe` "200800000"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           " _| _| _| _| _| _| _| _| _|",
                           " _| _| _| _| _| _| _| _| _|"]  `shouldBe` "333393333"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           "|_||_||_||_||_||_||_||_||_|",
                           "|_||_||_||_||_||_||_||_||_|"]  `shouldBe` "888888888 AMB [\"888886888\",\"888888880\",\"888888988\"]"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
                           " _| _| _| _| _| _| _| _| _|"]  `shouldBe` "555555555 AMB [\"555655555\",\"559555555\"]"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
                           "|_||_||_||_||_||_||_||_||_|"]  `shouldBe` "666666666 AMB [\"666566666\",\"686666666\"]"

    it "should find the appropriate string" $
       readAndTestAccount [" _  _  _  _  _  _  _  _  _ ",
                           "|_||_||_||_||_||_||_||_||_|",
                           " _| _| _| _| _| _| _| _| _|"]  `shouldBe` "999999999 AMB [\"899999999\",\"993999999\",\"999959999\"]"


    it "should find the appropriate string" $
       readAndTestAccount ["    _  _  _  _  _  _     _ ",
                           "|_||_|| || ||_   |  |  ||_ ",
                           "  | _||_||_||_|  |  |  | _|"]  `shouldBe` "490067715 AMB [\"490067115\",\"490067719\",\"490867715\"]"

    it "should find the appropriate string" $
           readAndTestAccount ["    _  _     _  _  _  _  _ ",
                           " _| _| _||_||_ |_   ||_||_|",
                           "  ||_  _|  | _||_|  ||_| _|"]  `shouldBe` "123456789"

    it "should find the appropriate string" $
       readAndTestAccount [" _     _  _  _  _  _  _    ",
                           "| || || || || || || ||_   |",
                           "|_||_||_||_||_||_||_| _|  |"]  `shouldBe` "000000051"

    it "should find the appropriate string" $
       readAndTestAccount ["    _  _  _  _  _  _     _ ",
                           "|_||_|| ||_||_   |  |  | _ ",
                           "  | _||_||_||_|  |  |  | _|"]  `shouldBe` "490867715"
