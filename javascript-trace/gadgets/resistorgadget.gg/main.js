// Resistor Gadget.
var resistance = 1000;

var currentBandIndex = 0;

var numberOfColorBands = 4;

var bandNumberValues = [1, 0, 2, 10, 15]; // Brown, Black, Red, Empty, Blank.
var buttonStrings = ["0black", "1brown", "2red", "3orange", "4yellow", "5green", "6blue", "7violet", "8gray", "9white", "Empty", "Tbrown", "Tred", "Tgold", "Tsilver", "Blank"];

function view_onOpen() {
    // Initialize the resistor's color bands to match the bandNumberValues array above.
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

    // Initialize the resistor value.
    doCalculateResistance();

    return;
}

// Resistor   color   codes   and   Tolerances
// -------------------------------------------
//    Codes	Colors		    Tolerances
//    -----     ------              ----------
//	0	Black
//	1	Brown		 	 1%
//	2	Red		 	 2%
//	3	Orange
//	4	Yellow
//	5	Green
//	6	Blue
//	7	Violet
//	8	Gray
//	9	White
//		Gold		 	 5%
//		Silver		 	10%

function doBlackButton() {
    drawNewColorBand(0);
    bandNumberValues[currentBandIndex] = 0;
    doCalculateResistance();

    return;
}

function doBrownButton() {
    drawNewColorBand(1);
    bandNumberValues[currentBandIndex] = 1;
    doCalculateResistance();

    return;
}

function doRedButton() {
    drawNewColorBand(2);
    bandNumberValues[currentBandIndex] = 2;
    doCalculateResistance();

    return;
}

function doOrangeButton() {
    drawNewColorBand(3);
    bandNumberValues[currentBandIndex] = 3;
    doCalculateResistance();

    return;
}

function doYellowButton() {
    drawNewColorBand(4);
    bandNumberValues[currentBandIndex] = 4;
    doCalculateResistance();

    return;
}

function doGreenButton() {
    drawNewColorBand(5);
    bandNumberValues[currentBandIndex] = 5;
    doCalculateResistance();

    return;
}

function doBlueButton() {
    drawNewColorBand(6);
    bandNumberValues[currentBandIndex] = 6;
    doCalculateResistance();

    return;
}

function doVioletButton() {
    drawNewColorBand(7);
    bandNumberValues[currentBandIndex] = 7;
    doCalculateResistance();

    return;
}

function doGrayButton() {
    drawNewColorBand(8);
    bandNumberValues[currentBandIndex] = 8;
    doCalculateResistance();

    return;
}

function doWhiteButton() {
    drawNewColorBand(9);
    bandNumberValues[currentBandIndex] = 9;
    doCalculateResistance();

    return;
}

function doEraseButton() {
    drawNewColorBand(10);
    bandNumberValues[currentBandIndex] = 10;
    doCalculateResistance();

    return;
}


function doFirstToleranceButton() {
    if (numberOfColorBands == 4) {
        fourthBand.downImage = firstToleranceButton.image;
        fourthBand.image = firstToleranceButton.image;
        fourthBand.overImage = firstToleranceButton.image;
    } else {
        fifthBand.downImage = firstToleranceButton.image;
        fifthBand.image = firstToleranceButton.image;
        fifthBand.overImage = firstToleranceButton.image;
    }

    return;
}

function doSecondToleranceButton() {
    if (numberOfColorBands == 4) {
        fourthBand.downImage = secondToleranceButton.image;
        fourthBand.image = secondToleranceButton.image;
        fourthBand.overImage = secondToleranceButton.image;
    } else {
        fifthBand.downImage = secondToleranceButton.image;
        fifthBand.image = secondToleranceButton.image;
        fifthBand.overImage = secondToleranceButton.image;
    }

    return;
}

function doNoneButton() {
    if (numberOfColorBands == 4) {
        fourthBand.downImage = "stock_images\\ButtonBlankNormal.PNG";
        fourthBand.image = "stock_images\\ButtonBlankNormal.PNG";
        fourthBand.overImage = "stock_images\\ButtonBlankNormal.PNG";
    } else {
        fifthBand.downImage = "stock_images\\ButtonBlankNormal.PNG";
        fifthBand.image = "stock_images\\ButtonBlankNormal.PNG";
        fifthBand.overImage = "stock_images\\ButtonBlankNormal.PNG";
    }

    return;
}

function drawNewColorBand(color) {

    switch (currentBandIndex) {
    case 0:
        {
            firstBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
            firstBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
            firstBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
            break;
        }
    case 1:
        {
            secondBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
            secondBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
            secondBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
            break;
        }
    case 2:
        {
            thirdBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
            thirdBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
            thirdBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
            break;
        }
    case 3:
        {
            fourthBand.downImage = "stock_images\\Button" + buttonStrings[color] + "Down.PNG";
            fourthBand.image = "stock_images\\Button" + buttonStrings[color] + "Normal.PNG";
            fourthBand.overImage = "stock_images\\Button" + buttonStrings[color] + "Over.PNG";
            break;
        }
    }

    return;
}

function doFirstBand() {
    // Move the arrow pointer.
    currentColorBandArrow.y = 79;
    currentBandIndex = 0;

    return;
}

function doSecondBand() {
    // Move the arrow pointer.
    currentColorBandArrow.y = 105;
    currentBandIndex = 1;

    return;
}

function doThirdBand() {
    // Move the arrow pointer.
    currentColorBandArrow.y = 131;
    currentBandIndex = 2;

    return;
}

function doFourthBand() {
    // This function does not need to do anything for a four band resistor.
    if (numberOfColorBands == 5) {
        // Move the arrow pointer.
        currentColorBandArrow.y = 157;
        currentBandIndex = 3;
    }

    return;
}

function doFifthBand() {
    // This is an "empty" function.  The fifth color band should only be set by the Tolerance buttons.
    return;
}

function doSwitchToFourBandResistor() {
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
}

function doSwitchToFiveBandResistor() {
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
}

function toggleNumberOfBandsButton() {
    if (numberOfColorBands == 4) {
        numberOfColorBands = 5;
        numberOfBandsButton.downImage = "stock_images\\Button5Down.PNG";
        numberOfBandsButton.image = "stock_images\\Button5Normal.PNG";
        numberOfBandsButton.overImage = "stock_images\\Button5Over.PNG";
        fourthBand.tooltip = "Resistor's fourth color band";
        fifthBand.tooltip = "Resistor's tolerance band";
        firstToleranceButton.tooltip = "5% Gold will become the fifth band (Tolerance)";
        secondToleranceButton.tooltip = "10% Silver will become the fifth band (Tolerance)";
        noneButton.tooltip = "Remove the fifth band (Tolerance)";
        ohms.tooltip = "Enter an Ohmic value between 0 and 999,000,000,000";
        doSwitchToFiveBandResistor();
    } else { // numberOfColorBands == 5.
        numberOfColorBands = 4;
        numberOfBandsButton.downImage = "stock_images\\Button4Down.PNG";
        numberOfBandsButton.image = "stock_images\\Button4Normal.PNG";
        numberOfBandsButton.overImage = "stock_images\\Button4Over.PNG";
        fourthBand.tooltip = "Resistor's tolerance band";
        fifthBand.tooltip = "";
        firstToleranceButton.tooltip = "1% Brown will become the fourth band (Tolerance)";
        secondToleranceButton.tooltip = "1% Red will become the fourth band (Tolerance)";
        noneButton.tooltip = "Remove the fourth band (Tolerance)";
        ohms.tooltip = "Enter an Ohmic value between 0 and 99,000,000,000";
        doSwitchToFourBandResistor();
    }

    doFirstBand();

    doCalculateResistance();

    return;
}

function doCalculateResistance() {
    var currentBandValue;
    var power;

    resistance = 0;
    for (var bandIndex = 0; bandIndex < numberOfColorBands - 2; bandIndex++) {
        currentBandValue = bandNumberValues[bandIndex];
        if (currentBandValue > 9) {
            resistance = -1; // Undefined.
        } else {
            resistance *= 10;
            resistance += Number(currentBandValue);
        }
    }

    power = bandNumberValues[numberOfColorBands - 2];
    if (power > 9) {
        resistance = -1; // Undefined.
    } else {
        power = Math.pow(10, power);
        resistance *= power;
    }

    if (resistance < 0) {
        ohms.value = "Undefined";
    } else {
        ohms.value = addCommas(resistance.toString());
    }

    return;
}

function addCommas(inputString) {
    var commaAddedValue = "";
    var lengthRemaining = inputString.length;
    var index = lengthRemaining - 1;
    var digitCount = 0;

    while (lengthRemaining > 0) {
        if (digitCount == 3) {
            commaAddedValue = "," + commaAddedValue; // Stick a comma in.
            digitCount = 0;
        }
        commaAddedValue = inputString.charAt(index--) + commaAddedValue;
        digitCount++;
        lengthRemaining--;
    }

    return (commaAddedValue);
}

function containsNonDigit(inputString) {
    var stringLength = inputString.length;
    for (var i = 0; i < stringLength; i++) {
        if ((inputString.charAt(i) < "0") || (inputString.charAt(i) > "9")) {
            return (true);
        }
    }

    return (false);
}

function containsLeadingZero(inputString) {
    if ((inputString.charAt(0) == "0") && (inputString.length != 1)) {
        return (true);
    }

    return (false);
}

function containsErroneousNonZeroDigits(inputString, zerosStartAtPosition) {
    var length = inputString.length;

    if (length <= zerosStartAtPosition) {
        return (false);
    }

    for (var i = zerosStartAtPosition; i < length; i++) {
        if (inputString.charAt(i) != "0") {
            return (true);
        }
    }

    return (false);
}

function removeCommas(inputString) {
    var noCommasValue = "";
    var index = 0;

    for (var i = 0; i < inputString.length; i++) {
        if (inputString.charAt(i) != ",") {
            noCommasValue = noCommasValue + inputString.charAt(i);
        }
    }

    return (noCommasValue);
}

function doOhmsCheck() {
    cleanedOhms = removeCommas(ohms.value);

    if (numberOfColorBands == 4) {
        minimumOhmsValue = 0;
        maximumOhmsValue = 99000000000;
    } else {
        minimumOhmsValue = 0;
        maximumOhmsValue = 999000000000;
    }

    if ((cleanedOhms < minimumOhmsValue) || (cleanedOhms > maximumOhmsValue) || (containsNonDigit(cleanedOhms)) || (containsLeadingZero(cleanedOhms)) || (containsErroneousNonZeroDigits(cleanedOhms, numberOfColorBands - 2))) {
        ohms.color = "#FF0000"; // Red.
        ohms.strikeout = true;
    } else {
        ohms.color = "#000000"; // Black.
        ohms.strikeout = false;
        resistance = cleanedOhms;
    }

    doGenerateBandColors();

    return;
}

function doGenerateBandColors() {
    digitString = resistance.toString();
    length = digitString.length;

    if (resistance < 0) { // Do NOT update the Color Bands if resistance is UNDEFINED.
        return;
    }

    if (numberOfColorBands == 4) { // Do the first three bands of color bars.
        if (resistance < 10) { // Force the first band color to black.
            firstBand.downImage = "stock_images\\Button0blackDown.PNG";
            firstBand.image = "stock_images\\Button0blackNormal.PNG";
            firstBand.overImage = "stock_images\\Button0blackOver.PNG";
            bandNumberValues[0] = 0;
        } else { // if(resistance >= 10)
            // Process the first digit.
            digit = digitString.charAt(0);
            firstBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            firstBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            firstBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[0] = digit;
        }

        if (resistance < 10) { // Process the first digit.
            if (length == 0) {
                digit = 0;
            } else {
                digit = digitString.charAt(0);
            }
            secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[1] = digit;
        } else { // Process the second digit.
            digit = digitString.charAt(1);
            secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[1] = digit;
        }

        // Process the multiplier.
        if (resistance < 100) {
            thirdBand.downImage = "stock_images\\Button0blackDown.PNG";
            thirdBand.image = "stock_images\\Button0blackNormal.PNG";
            thirdBand.overImage = "stock_images\\Button0blackOver.PNG";
            bandNumberValues[2] = 0;
        } else {
            digit = (digitString.length - 2);
            thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[2] = digit;
        }
    } else { // if(numberOfColorBands == 5) { // Do the first four bands of color bars. 
        if (resistance < 100) { // Force the first band color to black.
            firstBand.downImage = "stock_images\\Button0blackDown.PNG";
            firstBand.image = "stock_images\\Button0blackNormal.PNG";
            firstBand.overImage = "stock_images\\Button0blackOver.PNG";
            bandNumberValues[0] = 0;
            if (resistance < 10) {
                secondBand.downImage = "stock_images\\Button0blackDown.PNG";
                secondBand.image = "stock_images\\Button0blackNormal.PNG";
                secondBand.overImage = "stock_images\\Button0blackOver.PNG";
                bandNumberValues[1] = 0;
                digit = digitString.charAt(0);
                thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                bandNumberValues[2] = digit;
            } else { // Resistance is between 10 and 99.
                // Process the first digit.
                digit = digitString.charAt(0);
                secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                bandNumberValues[1] = digit;
                // Process the second digit.
                digit = digitString.charAt(1);
                thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
                thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
                thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
                bandNumberValues[2] = digit;
            }
        } else { // if(resistance >= 100)
            // Process the first digit.
            digit = digitString.charAt(0);
            firstBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            firstBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            firstBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[0] = digit;
            // Process the second digit.
            digit = digitString.charAt(1);
            secondBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            secondBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            secondBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[1] = digit;
            // Process the third digit.
            digit = digitString.charAt(2);
            thirdBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            thirdBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            thirdBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[2] = digit;
        }

        // Process the multiplier.
        if (resistance < 1000) {
            fourthBand.downImage = "stock_images\\Button0blackDown.PNG";
            fourthBand.image = "stock_images\\Button0blackNormal.PNG";
            fourthBand.overImage = "stock_images\\Button0blackOver.PNG";
            bandNumberValues[3] = 0;
        } else {
            digit = (digitString.length - 3);
            fourthBand.downImage = "stock_images\\Button" + buttonStrings[digit] + "Down.PNG";
            fourthBand.image = "stock_images\\Button" + buttonStrings[digit] + "Normal.PNG";
            fourthBand.overImage = "stock_images\\Button" + buttonStrings[digit] + "Over.PNG";
            bandNumberValues[3] = digit;
        }
    }

    return;
}