__initnumargs("gadgets/resistorgadget.gg/main.js",
              [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,2,1,0,0]);
var resistance = 1000;
var currentBandIndex = 0;
var numberOfColorBands = 4;
var bandNumberValues = [1,0,2,10,15];
var buttonStrings = ["0black",
                     "1brown",
                     "2red",
                     "3orange",
                     "4yellow",
                     "5green",
                     "6blue",
                     "7violet",
                     "8gray",
                     "9white",
                     "Empty",
                     "Tbrown",
                     "Tred",
                     "Tgold",
                     "Tsilver",
                     "Blank"];
var view_onOpen = __typedjs(function  ()
                            {
                              firstBand.downImage = "stock_images\\Button" + buttonStrings[bandNumberValues[0]] + "Down.PNG";
                              firstBand.image = "stock_images\\Button" + buttonStrings[bandNumberValues[0]] + "Normal.PNG";
                              firstBand.overImage = "stock_images\\Button" + buttonStrings[bandNumberValues[0]] + "Over.PNG";
                              secondBand.downImage = "stock_images\\Button" + buttonStrings[bandNumberValues[1]] + "Down.PNG";
                              secondBand.image = "stock_images\\Button" + buttonStrings[bandNumberValues[1]] + "Normal.PNG";
                              secondBand.overImage = "stock_images\\Button" + buttonStrings[bandNumberValues[1]] + "Over.PNG";
                              thirdBand.downImage = "stock_images\\Button" + buttonStrings[bandNumberValues[2]] + "Down.PNG";
                              thirdBand.image = "stock_images\\Button" + buttonStrings[bandNumberValues[2]] + "Normal.PNG";
                              thirdBand.overImage = "stock_images\\Button" + buttonStrings[bandNumberValues[2]] + "Over.PNG";
                              fourthBand.downImage = "stock_images\\Button" + buttonStrings[bandNumberValues[3]] + "Down.PNG";
                              fourthBand.image = "stock_images\\Button" + buttonStrings[bandNumberValues[3]] + "Normal.PNG";
                              fourthBand.overImage = "stock_images\\Button" + buttonStrings[bandNumberValues[3]] + "Over.PNG";
                              fifthBand.downImage = "stock_images\\Button" + buttonStrings[bandNumberValues[4]] + "Down.PNG";
                              fifthBand.image = "stock_images\\Button" + buttonStrings[bandNumberValues[4]] + "Normal.PNG";
                              fifthBand.overImage = "stock_images\\Button" + buttonStrings[bandNumberValues[4]] + "Over.PNG";
                              doCalculateResistance();
                              return;
                            },
                            0,
                            "view_onOpen",
                            "gadgets/resistorgadget.gg/main.js",
                            0);
var doBlackButton = __typedjs(function  ()
                              {
                                drawNewColorBand(0);
                                bandNumberValues[currentBandIndex] = 0;
                                doCalculateResistance();
                                return;
                              },
                              0,
                              "doBlackButton",
                              "gadgets/resistorgadget.gg/main.js",
                              1);
var doBrownButton = __typedjs(function  ()
                              {
                                drawNewColorBand(1);
                                bandNumberValues[currentBandIndex] = 1;
                                doCalculateResistance();
                                return;
                              },
                              0,
                              "doBrownButton",
                              "gadgets/resistorgadget.gg/main.js",
                              2);
var doRedButton = __typedjs(function  ()
                            {
                              drawNewColorBand(2);
                              bandNumberValues[currentBandIndex] = 2;
                              doCalculateResistance();
                              return;
                            },
                            0,
                            "doRedButton",
                            "gadgets/resistorgadget.gg/main.js",
                            3);
var doOrangeButton = __typedjs(function  ()
                               {
                                 drawNewColorBand(3);
                                 bandNumberValues[currentBandIndex] = 3;
                                 doCalculateResistance();
                                 return;
                               },
                               0,
                               "doOrangeButton",
                               "gadgets/resistorgadget.gg/main.js",
                               4);
var doYellowButton = __typedjs(function  ()
                               {
                                 drawNewColorBand(4);
                                 bandNumberValues[currentBandIndex] = 4;
                                 doCalculateResistance();
                                 return;
                               },
                               0,
                               "doYellowButton",
                               "gadgets/resistorgadget.gg/main.js",
                               5);
var doGreenButton = __typedjs(function  ()
                              {
                                drawNewColorBand(5);
                                bandNumberValues[currentBandIndex] = 5;
                                doCalculateResistance();
                                return;
                              },
                              0,
                              "doGreenButton",
                              "gadgets/resistorgadget.gg/main.js",
                              6);
var doBlueButton = __typedjs(function  ()
                             {
                               drawNewColorBand(6);
                               bandNumberValues[currentBandIndex] = 6;
                               doCalculateResistance();
                               return;
                             },
                             0,
                             "doBlueButton",
                             "gadgets/resistorgadget.gg/main.js",
                             7);
var doVioletButton = __typedjs(function  ()
                               {
                                 drawNewColorBand(7);
                                 bandNumberValues[currentBandIndex] = 7;
                                 doCalculateResistance();
                                 return;
                               },
                               0,
                               "doVioletButton",
                               "gadgets/resistorgadget.gg/main.js",
                               8);
var doGrayButton = __typedjs(function  ()
                             {
                               drawNewColorBand(8);
                               bandNumberValues[currentBandIndex] = 8;
                               doCalculateResistance();
                               return;
                             },
                             0,
                             "doGrayButton",
                             "gadgets/resistorgadget.gg/main.js",
                             9);
var doWhiteButton = __typedjs(function  ()
                              {
                                drawNewColorBand(9);
                                bandNumberValues[currentBandIndex] = 9;
                                doCalculateResistance();
                                return;
                              },
                              0,
                              "doWhiteButton",
                              "gadgets/resistorgadget.gg/main.js",
                              10);
var doEraseButton = __typedjs(function  ()
                              {
                                drawNewColorBand(10);
                                bandNumberValues[currentBandIndex] = 10;
                                doCalculateResistance();
                                return;
                              },
                              0,
                              "doEraseButton",
                              "gadgets/resistorgadget.gg/main.js",
                              11);
var doFirstToleranceButton = __typedjs(function  ()
                                       {
                                         if (numberOfColorBands == 4)
                                         {
                                           fourthBand.downImage = firstToleranceButton.image;
                                           fourthBand.image = firstToleranceButton.image;
                                           fourthBand.overImage = firstToleranceButton.image;
                                         }
                                         else {
                                                fifthBand.downImage = firstToleranceButton.image;
                                                fifthBand.image = firstToleranceButton.image;
                                                fifthBand.overImage = firstToleranceButton.image;
                                              };
                                         return;
                                       },
                                       0,
                                       "doFirstToleranceButton",
                                       "gadgets/resistorgadget.gg/main.js",
                                       12);
var doSecondToleranceButton = __typedjs(function  ()
                                        {
                                          if (numberOfColorBands == 4)
                                          {
                                            fourthBand.downImage = secondToleranceButton.image;
                                            fourthBand.image = secondToleranceButton.image;
                                            fourthBand.overImage = secondToleranceButton.image;
                                          }
                                          else {
                                                 fifthBand.downImage = secondToleranceButton.image;
                                                 fifthBand.image = secondToleranceButton.image;
                                                 fifthBand.overImage = secondToleranceButton.image;
                                               };
                                          return;
                                        },
                                        0,
                                        "doSecondToleranceButton",
                                        "gadgets/resistorgadget.gg/main.js",
                                        13);
var doNoneButton = __typedjs(function  ()
                             {
                               if (numberOfColorBands == 4)
                               {
                                 fourthBand.downImage = "stock_images\\ButtonBlankNormal.PNG";
                                 fourthBand.image = "stock_images\\ButtonBlankNormal.PNG";
                                 fourthBand.overImage = "stock_images\\ButtonBlankNormal.PNG";
                               }
                               else {
                                      fifthBand.downImage = "stock_images\\ButtonBlankNormal.PNG";
                                      fifthBand.image = "stock_images\\ButtonBlankNormal.PNG";
                                      fifthBand.overImage = "stock_images\\ButtonBlankNormal.PNG";
                                    };
                               return;
                             },
                             0,
                             "doNoneButton",
                             "gadgets/resistorgadget.gg/main.js",
                             14);
var drawNewColorBand = __typedjs(function  (color)
                                 {
                                   switch (currentBandIndex)
                                   {case
                                    0 :
                                      {
                                        firstBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
                                        firstBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
                                        firstBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
                                        break;
                                      };
                                    case
                                    1 :
                                      {
                                        secondBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
                                        secondBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
                                        secondBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
                                        break;
                                      };
                                    case
                                    2 :
                                      {
                                        thirdBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
                                        thirdBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
                                        thirdBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
                                        break;
                                      };
                                    case
                                    3 :
                                      {
                                        fourthBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
                                        fourthBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
                                        fourthBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
                                        break;
                                      };};
                                   return;
                                 },
                                 0,
                                 "drawNewColorBand",
                                 "gadgets/resistorgadget.gg/main.js",
                                 15);
var doFirstBand = __typedjs(function  ()
                            {
                              currentColorBandArrow.y = 79;
                              currentBandIndex = 0;
                              return;
                            },
                            0,
                            "doFirstBand",
                            "gadgets/resistorgadget.gg/main.js",
                            16);
var doSecondBand = __typedjs(function  ()
                             {
                               currentColorBandArrow.y = 105;
                               currentBandIndex = 1;
                               return;
                             },
                             0,
                             "doSecondBand",
                             "gadgets/resistorgadget.gg/main.js",
                             17);
var doThirdBand = __typedjs(function  ()
                            {
                              currentColorBandArrow.y = 131;
                              currentBandIndex = 2;
                              return;
                            },
                            0,
                            "doThirdBand",
                            "gadgets/resistorgadget.gg/main.js",
                            18);
var doFourthBand = __typedjs(function  ()
                             {
                               if (numberOfColorBands == 5)
                               {
                                 currentColorBandArrow.y = 157;
                                 currentBandIndex = 3;
                               };
                               return;
                             },
                             0,
                             "doFourthBand",
                             "gadgets/resistorgadget.gg/main.js",
                             19);
var doFifthBand = __typedjs(function  ()
                            {
                              return;
                            },
                            0,
                            "doFifthBand",
                            "gadgets/resistorgadget.gg/main.js",
                            20);
var doSwitchToFourBandResistor = __typedjs(function  ()
                                           {
                                             bandNumberValues[2] = 10;
                                             thirdBand.downImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             thirdBand.image = "stock_images\\ButtonEmptyNormal.PNG";
                                             thirdBand.overImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             bandNumberValues[3] = 10;
                                             fourthBand.downImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             fourthBand.image = "stock_images\\ButtonEmptyNormal.PNG";
                                             fourthBand.overImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             bandNumberValues[4] = 15;
                                             fifthBand.downImage = "stock_images\\ButtonBlankNormal.PNG";
                                             fifthBand.image = "stock_images\\ButtonBlankNormal.PNG";
                                             fifthBand.overImage = "stock_images\\ButtonBlankNormal.PNG";
                                             firstToleranceButton.downImage = "stock_images\\ButtonTbrownDown.PNG";
                                             firstToleranceButton.image = "stock_images\\ButtonTbrownNormal.PNG";
                                             firstToleranceButton.overImage = "stock_images\\ButtonTbrownOver.PNG";
                                             secondToleranceButton.downImage = "stock_images\\ButtonTredDown.PNG";
                                             secondToleranceButton.image = "stock_images\\ButtonTredNormal.PNG";
                                             secondToleranceButton.overImage = "stock_images\\ButtonTredOver.PNG";
                                             return;
                                           },
                                           0,
                                           "doSwitchToFourBandResistor",
                                           "gadgets/resistorgadget.gg/main.js",
                                           21);
var doSwitchToFiveBandResistor = __typedjs(function  ()
                                           {
                                             bandNumberValues[3] = 10;
                                             fourthBand.downImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             fourthBand.image = "stock_images\\ButtonEmptyNormal.PNG";
                                             fourthBand.overImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             bandNumberValues[4] = 10;
                                             fifthBand.downImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             fifthBand.image = "stock_images\\ButtonEmptyNormal.PNG";
                                             fifthBand.overImage = "stock_images\\ButtonEmptyNormal.PNG";
                                             firstToleranceButton.downImage = "stock_images\\ButtonTgoldDown.PNG";
                                             firstToleranceButton.image = "stock_images\\ButtonTgoldNormal.PNG";
                                             firstToleranceButton.overImage = "stock_images\\ButtonTgoldOver.PNG";
                                             secondToleranceButton.downImage = "stock_images\\ButtonTsilverDown.PNG";
                                             secondToleranceButton.image = "stock_images\\ButtonTsilverNormal.PNG";
                                             secondToleranceButton.overImage = "stock_images\\ButtonTsilverOver.PNG";
                                             return;
                                           },
                                           0,
                                           "doSwitchToFiveBandResistor",
                                           "gadgets/resistorgadget.gg/main.js",
                                           22);
var toggleNumberOfBandsButton = __typedjs(function  ()
                                          {
                                            if (numberOfColorBands == 4)
                                            {
                                              numberOfColorBands = 5;
                                              numberOfBandsButton.downImage = "stock_images\\Button5Down.PNG";
                                              numberOfBandsButton.image = "stock_images\\Button5Normal.PNG";
                                              numberOfBandsButton.overImage = "stock_images\\Button5Over.PNG";
                                              fourthBand.tooltip = "Resistor\'s fourth color band";
                                              fifthBand.tooltip = "Resistor\'s tolerance band";
                                              firstToleranceButton.tooltip = "5% Gold will become the fifth band (Tolerance)";
                                              secondToleranceButton.tooltip = "10% Silver will become the fifth band (Tolerance)";
                                              noneButton.tooltip = "Remove the fifth band (Tolerance)";
                                              ohms.tooltip = "Enter an Ohmic value between 0 and 999,000,000,000";
                                              doSwitchToFiveBandResistor();
                                            }
                                            else {
                                                   numberOfColorBands = 4;
                                                   numberOfBandsButton.downImage = "stock_images\\Button4Down.PNG";
                                                   numberOfBandsButton.image = "stock_images\\Button4Normal.PNG";
                                                   numberOfBandsButton.overImage = "stock_images\\Button4Over.PNG";
                                                   fourthBand.tooltip = "Resistor\'s tolerance band";
                                                   fifthBand.tooltip = "";
                                                   firstToleranceButton.tooltip = "1% Brown will become the fourth band (Tolerance)";
                                                   secondToleranceButton.tooltip = "1% Red will become the fourth band (Tolerance)";
                                                   noneButton.tooltip = "Remove the fourth band (Tolerance)";
                                                   ohms.tooltip = "Enter an Ohmic value between 0 and 99,000,000,000";
                                                   doSwitchToFourBandResistor();
                                                 };
                                            doFirstBand();
                                            doCalculateResistance();
                                            return;
                                          },
                                          0,
                                          "toggleNumberOfBandsButton",
                                          "gadgets/resistorgadget.gg/main.js",
                                          23);
var doCalculateResistance = __typedjs(function  ()
                                      {
                                        var currentBandValue;
                                        var power;
                                        resistance = 0;
                                        for (var bandIndex = 0; bandIndex < numberOfColorBands - 2; bandIndex++)
                                        {
                                          currentBandValue = bandNumberValues[bandIndex];
                                          if (currentBandValue > 9)
                                          {
                                            resistance = - 1;
                                          }
                                          else {
                                                 resistance *= 10;
                                                 resistance += Number(currentBandValue);
                                               };
                                        };
                                        power = bandNumberValues[numberOfColorBands - 2];
                                        if (power > 9)
                                        {
                                          resistance = - 1;
                                        }
                                        else {
                                               power = Math.pow(10,power);
                                               resistance *= power;
                                             };
                                        if (resistance < 0)
                                        {
                                          ohms.value = "Undefined";
                                        }
                                        else {
                                               ohms.value = addCommas(resistance.toString());
                                             };
                                        return;
                                      },
                                      0,
                                      "doCalculateResistance",
                                      "gadgets/resistorgadget.gg/main.js",
                                      24);
var addCommas = __typedjs(function  (inputString)
                          {
                            var commaAddedValue = "";
                            var lengthRemaining = inputString.length;
                            var index = lengthRemaining - 1;
                            var digitCount = 0;
                            while (lengthRemaining > 0)
                            {
                              if (digitCount == 3)
                              {
                                commaAddedValue = "," + commaAddedValue;
                                digitCount = 0;
                              };
                              commaAddedValue = inputString.charAt(index--) + commaAddedValue;
                              digitCount++;
                              lengthRemaining--;
                            };
                            return (commaAddedValue);
                          },
                          0,
                          "addCommas",
                          "gadgets/resistorgadget.gg/main.js",
                          25);
var containsNonDigit = __typedjs(function  (inputString)
                                 {
                                   var stringLength = inputString.length;
                                   for (var i = 0; i < stringLength; i++)
                                   {
                                     if ((inputString.charAt(i) < "0") || (inputString.charAt(i) > "9"))
                                     {
                                       return (true);
                                     };
                                   };
                                   return (false);
                                 },
                                 0,
                                 "containsNonDigit",
                                 "gadgets/resistorgadget.gg/main.js",
                                 26);
var containsLeadingZero = __typedjs(function  (inputString)
                                    {
                                      if ((inputString.charAt(0) == "0") && (inputString.length != 1))
                                      {
                                        return (true);
                                      };
                                      return (false);
                                    },
                                    0,
                                    "containsLeadingZero",
                                    "gadgets/resistorgadget.gg/main.js",
                                    27);
var containsErroneousNonZeroDigits = __typedjs(function  (inputString,
                                                          zerosStartAtPosition)
                                               {
                                                 var length = inputString.length;
                                                 if (length <= zerosStartAtPosition)
                                                 {
                                                   return (false);
                                                 };
                                                 for (var i = zerosStartAtPosition; i < length; i++)
                                                 {
                                                   if (inputString.charAt(i) != "0")
                                                   {
                                                     return (true);
                                                   };
                                                 };
                                                 return (false);
                                               },
                                               0,
                                               "containsErroneousNonZeroDigits",
                                               "gadgets/resistorgadget.gg/main.js",
                                               28);
var removeCommas = __typedjs(function  (inputString)
                             {
                               var noCommasValue = "";
                               var index = 0;
                               for (var i = 0; i < inputString.length; i++)
                               {
                                 if (inputString.charAt(i) != ",")
                                 {
                                   noCommasValue = noCommasValue + inputString.charAt(i);
                                 };
                               };
                               return (noCommasValue);
                             },
                             0,
                             "removeCommas",
                             "gadgets/resistorgadget.gg/main.js",
                             29);
var doOhmsCheck = __typedjs(function  ()
                            {
                              cleanedOhms = removeCommas(ohms.value);
                              if (numberOfColorBands == 4)
                              {
                                minimumOhmsValue = 0;
                                maximumOhmsValue = 215752192;
                              }
                              else {
                                     minimumOhmsValue = 0;
                                     maximumOhmsValue = -1727379968;
                                   };
                              if ((cleanedOhms < minimumOhmsValue) || (cleanedOhms > maximumOhmsValue) || (containsNonDigit(cleanedOhms)) || (containsLeadingZero(cleanedOhms)) || (containsErroneousNonZeroDigits(cleanedOhms,
                                                                                                                                                                                                                   numberOfColorBands - 2)))
                              {
                                ohms.color = "#FF0000";
                                ohms.strikeout = true;
                              }
                              else {
                                     ohms.color = "#000000";
                                     ohms.strikeout = false;
                                     resistance = cleanedOhms;
                                   };
                              doGenerateBandColors();
                              return;
                            },
                            0,
                            "doOhmsCheck",
                            "gadgets/resistorgadget.gg/main.js",
                            30);
var doGenerateBandColors = __typedjs(function  ()
                                     {
                                       digitString = resistance.toString();
                                       length = digitString.length;
                                       if (resistance < 0)
                                       {
                                         return;
                                       };
                                       if (numberOfColorBands == 4)
                                       {
                                         if (resistance < 10)
                                         {
                                           firstBand.downImage = "stock_images\\Button0blackDown.PNG";
                                           firstBand.image = "stock_images\\Button0blackNormal.PNG";
                                           firstBand.overImage = "stock_images\\Button0blackOver.PNG";
                                           bandNumberValues[0] = 0;
                                         }
                                         else {
                                                digit = digitString.charAt(0);
                                                firstBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                firstBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                firstBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                bandNumberValues[0] = digit;
                                              };
                                         if (resistance < 10)
                                         {
                                           if (length == 0)
                                           {
                                             digit = 0;
                                           }
                                           else {
                                                  digit = digitString.charAt(0);
                                                };
                                           secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                           secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                           secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                           bandNumberValues[1] = digit;
                                         }
                                         else {
                                                digit = digitString.charAt(1);
                                                secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                bandNumberValues[1] = digit;
                                              };
                                         if (resistance < 100)
                                         {
                                           thirdBand.downImage = "stock_images\\Button0blackDown.PNG";
                                           thirdBand.image = "stock_images\\Button0blackNormal.PNG";
                                           thirdBand.overImage = "stock_images\\Button0blackOver.PNG";
                                           bandNumberValues[2] = 0;
                                         }
                                         else {
                                                digit = (digitString.length - 2);
                                                thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                bandNumberValues[2] = digit;
                                              };
                                       }
                                       else {
                                              if (resistance < 100)
                                              {
                                                firstBand.downImage = "stock_images\\Button0blackDown.PNG";
                                                firstBand.image = "stock_images\\Button0blackNormal.PNG";
                                                firstBand.overImage = "stock_images\\Button0blackOver.PNG";
                                                bandNumberValues[0] = 0;
                                                if (resistance < 10)
                                                {
                                                  secondBand.downImage = "stock_images\\Button0blackDown.PNG";
                                                  secondBand.image = "stock_images\\Button0blackNormal.PNG";
                                                  secondBand.overImage = "stock_images\\Button0blackOver.PNG";
                                                  bandNumberValues[1] = 0;
                                                  digit = digitString.charAt(0);
                                                  thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                  thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                  thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                  bandNumberValues[2] = digit;
                                                }
                                                else {
                                                       digit = digitString.charAt(0);
                                                       secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                       secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                       secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                       bandNumberValues[1] = digit;
                                                       digit = digitString.charAt(1);
                                                       thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                       thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                       thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                       bandNumberValues[2] = digit;
                                                     };
                                              }
                                              else {
                                                     digit = digitString.charAt(0);
                                                     firstBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                     firstBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                     firstBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                     bandNumberValues[0] = digit;
                                                     digit = digitString.charAt(1);
                                                     secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                     secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                     secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                     bandNumberValues[1] = digit;
                                                     digit = digitString.charAt(2);
                                                     thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                     thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                     thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                     bandNumberValues[2] = digit;
                                                   };
                                              if (resistance < 1000)
                                              {
                                                fourthBand.downImage = "stock_images\\Button0blackDown.PNG";
                                                fourthBand.image = "stock_images\\Button0blackNormal.PNG";
                                                fourthBand.overImage = "stock_images\\Button0blackOver.PNG";
                                                bandNumberValues[3] = 0;
                                              }
                                              else {
                                                     digit = (digitString.length - 3);
                                                     fourthBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                                                     fourthBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                                                     fourthBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                                                     bandNumberValues[3] = digit;
                                                   };
                                            };
                                       return;
                                     },
                                     0,
                                     "doGenerateBandColors",
                                     "gadgets/resistorgadget.gg/main.js",
                                     31);
