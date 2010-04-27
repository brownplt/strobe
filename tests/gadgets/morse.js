//4 vars added
//5 init i to 0 instead of undefined
//4 changes to settimeout to not use a string
var token = 0; //CLAUDIU: move here for func lift

function view_onOpen() /*:  -> Void */ {
    edit1.value = "Type the text...";
    edit2.value = "- -.-- .--. . + - .... . + - . -..- - .-.-.- .-.-.- .-.-.- ";
    edit2.readonly = true;
    bplay.visible = true;
    edit2.bold = true;
    edit2.size = 12;
}

function radio1_onclick() /*:  -> Void */ {
    edit1.value = "";
    edit2.value = "";
    bplay.visible = true;
    bplay.caption = "Play";
    edit2.bold = true;
    edit2.size = 12;
}

function radio2_onclick() /*:  -> Void */ {
    stopAudio();
    edit1.value = "";
    edit2.value = "";
    bplay.visible = false;
    edit2.bold = false;
    edit2.size = 10;
}

function edit1_onchange() /*:  -> Void */ {
    if (radio2.value == true) code2Text();
    else text2Code();
}

function edit1_onclick() /*:  -> Void */ {
    if (edit1.value == "Type the text...") {
        edit1.value = "";
        edit2.value = "";
    }
}

function bplay_onclick() /*:  -> Void */ {
    if (edit2.value.length != 0) {
        if (bplay.caption == "Play") {
            bplay.caption = "Stop";
            playAudio();
        }
        else {
            stopAudio();
            bplay.caption = "Play";
        }
    }
}

function playAudio() /*:  -> Void */ {
    var source = edit2.value,
        i = 0;
    var audio_dit = framework.audio.open(storage.extract("dit.wav"));
    var audio_dah = framework.audio.open(storage.extract("dah.wav"));
    var audio_pause = framework.audio.open(storage.extract("pause.wav"));
    if (source.length != 0) {
            for (i = 0; i < source.length; i++) {
                switch (source.charAt(i)) {
                case '.':
                    token = setTimeout(
                      function()/*:->Void*/{ audio_dit.play(); }, i * 500);
                    break;
                case '-':
                    token = setTimeout(
                      function()/*:->Void*/{ audio_dah.play(); }, i * 500);
                    break;
                default:
                    token = setTimeout(
                      function()/*:->Void*/{ audio_pause.play(); }, i * 500);
                    break;
                }
            }
            token = setTimeout(
              function()/*:->Void*/{ bplay.caption = "Play"; },
              source.length * 500);
        }
}

function stopAudio() /*:  -> Void */ {
    for (var i = 1; i <= token; i++)
    clearTimeout(i);
}

//codes a character and appends it to output


function writeCode(c) /*: String -> Void */ {
    var output = edit2;
    switch (c) {
    case 'a':
        output.value += ".- ";
        break;
    case 'b':
        output.value += "-... ";
        break;
    case 'c':
        output.value += "-.-. ";
        break;
    case 'd':
        output.value += "-.. ";
        break;
    case 'e':
        output.value += ". ";
        break;
    case 'f':
        output.value += "..-. ";
        break;
    case 'g':
        output.value += "--. ";
        break;
    case 'h':
        output.value += ".... ";
        break;
    case 'i':
        output.value += ".. ";
        break;
    case 'j':
        output.value += ".--- ";
        break;
    case 'k':
        output.value += "-.- ";
        break;
    case 'l':
        output.value += ".-.. ";
        break;
    case 'm':
        output.value += "-- ";
        break;
    case 'n':
        output.value += "-. ";
        break;
    case 'o':
        output.value += "--- ";
        break;
    case 'p':
        output.value += ".--. ";
        break;
    case 'q':
        output.value += "--.- ";
        break;
    case 'r':
        output.value += ".-. ";
        break;
    case 's':
        output.value += "... ";
        break;
    case 't':
        output.value += "- ";
        break;
    case 'u':
        output.value += "..- ";
        break;
    case 'v':
        output.value += "...- ";
        break;
    case 'w':
        output.value += ".-- ";
        break;
    case 'x':
        output.value += "-..- ";
        break;
    case 'y':
        output.value += "-.-- ";
        break;
    case 'z':
        output.value += "--.. ";
        break;
    case '0':
        output.value += "----- ";
        break;
    case '1':
        output.value += ".---- ";
        break;
    case '2':
        output.value += "..--- ";
        break;
    case '3':
        output.value += "...-- ";
        break;
    case '4':
        output.value += "....- ";
        break;
    case '5':
        output.value += "..... ";
        break;
    case '6':
        output.value += "-.... ";
        break;
    case '7':
        output.value += "--... ";
        break;
    case '8':
        output.value += "---.. ";
        break;
    case '9':
        output.value += "----. ";
        break;
    case '.':
        output.value += ".-.-.- ";
        break;
    case ',':
        output.value += "--..-- ";
        break;
    case '?':
        output.value += "..--.. ";
        break;
    case '!':
        output.value += "-.-.-- ";
        break;
    case '/':
        output.value += "-..-. ";
        break;
    case '(':
        output.value += "-.--. ";
        break;
    case ')':
        output.value += "-.--.- ";
        break;
    case ':':
        output.value += "---... ";
        break;
    case ';':
        output.value += "-.-.-. ";
        break;
    case '=':
        output.value += "-...- ";
        break;
    case '+':
        output.value += ".-.-. ";
        break;
    case '-':
        output.value += "-....- ";
        break;
    case '_':
        output.value += "..--.- ";
        break;
    case ' ':
        output.value += "+ ";
        break;
    default:
        break;
    }
}

//Text to Morse code function [Max 1000 characters]


function text2Code() /*:  -> Void */ {
    var possiblecomb = "abcdefghijklmnopqrstuvwxyz0123456789.,?!/():;=+-_";
    var input = edit1.value.toLowerCase();
    var output = edit2;
    if (input.length > 1000) //max 1000 characters
    {
        output.value = "The text is too large...";
    }
    else {
        output.value = "";
        for (var i = 0; i < input.length; i++)
        writeCode(input.charAt(i));
    }
}

//decodes a morse combination and appends it to output


function writeText(s) /*: String -> Void */ {
    var output = edit2;
    switch (s) {
    case '.-':
        output.value += "a";
        break;
    case '-...':
        output.value += "b";
        break;
    case '-.-.':
        output.value += "c";
        break;
    case '-..':
        output.value += "d";
        break;
    case '.':
        output.value += "e";
        break;
    case '..-.':
        output.value += "f";
        break;
    case '--.':
        output.value += "g";
        break;
    case '....':
        output.value += "h";
        break;
    case '..':
        output.value += "i";
        break;
    case '.---':
        output.value += "j";
        break;
    case '-.-':
        output.value += "k";
        break;
    case '.-..':
        output.value += "l";
        break;
    case '--':
        output.value += "m";
        break;
    case '-.':
        output.value += "n";
        break;
    case '---':
        output.value += "o";
        break;
    case '.--.':
        output.value += "p";
        break;
    case '--.-':
        output.value += "q";
        break;
    case '.-.':
        output.value += "r";
        break;
    case '...':
        output.value += "s";
        break;
    case '-':
        output.value += "t";
        break;
    case '..-':
        output.value += "u";
        break;
    case '...-':
        output.value += "v";
        break;
    case '.--':
        output.value += "w";
        break;
    case '-..-':
        output.value += "x";
        break;
    case '-.--':
        output.value += "y";
        break;
    case '--..':
        output.value += "z";
        break;
    case '-----':
        output.value += "0";
        break;
    case '.----':
        output.value += "1";
        break;
    case '..---':
        output.value += "2";
        break;
    case '...--':
        output.value += "3";
        break;
    case '....-':
        output.value += "4";
        break;
    case '.....':
        output.value += "5";
        break;
    case '-....':
        output.value += "6";
        break;
    case '--...':
        output.value += "7";
        break;
    case '---..':
        output.value += "8";
        break;
    case '----.':
        output.value += "9";
        break;
    case '.-.-.-':
        output.value += ".";
        break;
    case '--..--':
        output.value += ",";
        break;
    case '..--..':
        output.value += "?";
        break;
    case '-.-.--':
        output.value += "!";
        break;
    case '-..-.':
        output.value += "/";
        break;
    case '-.--.':
        output.value += "(";
        break;
    case '-.--.-':
        output.value += ")";
        break;
    case '---...':
        output.value += ";";
        break;
    case '-.-.-.':
        output.value += ";";
        break;
    case '-...-':
        output.value += "=";
        break;
    case '.-.-.':
        output.value += "+";
        break;
    case '-....-':
        output.value += "-";
        break;
    case '..--.-':
        output.value += "_";
        break;
    default:
        break;
    }
}

//Morse to Text function [MAX 4000 characters]


function code2Text() /*:  -> Void */ {
    var input = edit1.value;
    var output = edit2,
        s="";
    var len = input.length,
        i=0, comb = "";
    output.value = "";
    if (len < 4000) {
            for (i = 0; i < len; i++) {
                if (input.charAt(i) != '.' && input.charAt(i) != '-' && input.charAt(i) != '+' && input.charAt(i) != ' ') {
                    s = input;
                    edit1.value = "";
                    for (var j = 0; j <= s.length; j++) {
                        if (s.charAt(j) == '.' || s.charAt(j) == '-' || s.charAt(j) == '+' || s.charAt(j) == ' ') {
                            edit1.value += s.charAt(j);
                        }
                    }
                    alert("This is not a valid Morse Code. It should contain only .-+ and the character \"space\"");
                    code2Text();
                    return;
                }
                if (input.charAt(i) != ' ') {
                    if (input.charAt(i) != '.' || input.charAt(i) != '-' || input.charAt(i) != '+') if (input.charAt(i) == '+') output.value += " ";
                    else comb += input.charAt(i);
                }
                else {
                    writeText(comb);
                    comb = "";
                }
            }
        }
    else output.value = "The code is too large...";
}
