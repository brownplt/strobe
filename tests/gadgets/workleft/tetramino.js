//note: took out "getLevel" since it was never used.

var tetris = null;

var X = 0;
var Y = 0;
var curPiece = 0;



// PARAMETERS
var nSquares = 4;
var nTypes = 7;
var boardHeight = 16;
var boardWidth = 10;
var Level = 1;
var speed0 = 700;
var speedK = 60;
var speed = speed0 - speedK * Level;
var nLines = 0;


// GLOBAL VARIABLES
var curX = 1;
var curY = 1;
var skyline = boardHeight - 1;
var serialN = 0;

var boardLoaded = 1;
var gamePaused = false;
var gameStarted = false;
var sayingBye = 0;
var timerID = null;

var tetrisMusic = null;
var tetrisMusicSrc = gadget.storage.extract("music/Korobeiniki.mid");

// ARRAYS
var f = /*: Array<Int> */ []; // new Array();
for (var i = 0; i < 20; i++) {
    f[i] = /*: Int */ [];
    for (var j = 0; j < 20; j++) {
        f[i][j] = 0;
    }
}

var xToErase = [0,0,0,0]; // new Array(0, 0, 0, 0);
var yToErase = [0,0,0,0]; //new Array(0, 0, 0, 0);
var dx = [0,0,0,0]; //new Array(0, 0, 0, 0);
var dy = [0,0,0,0];//new Array(0, 0, 0, 0);
var dx_ = [0,0,0,0]; // new Array(0, 0, 0, 0);
var dy_ = [0,0,0,0]; //new Array(0, 0, 0, 0);
var dxBank = /*: Array<Int> */ []; //new Array();
var dyBank = /*: Array<Int> */ []; // new Array();
dxBank[1] = [0,1,-1,0]; // new Array(0, 1, - 1, 0);
dyBank[1] = [0,0,0,1]; //new Array(0, 0, 0, 1);
dxBank[2] = [0,1,-1,-1]; //new Array(0, 1, - 1, - 1);
dyBank[2] = [0,0,0,1];//new Array(0, 0, 0, 1);
dxBank[3] = [0,1,-1,1]; //new Array(0, 1, - 1, 1);
dyBank[3] = [0, 0, 0, 1];
dxBank[4] = [0, - 1, 1, 0];
dyBank[4] = [0, 0, 1, 1];
dxBank[5] = [0, 1, - 1, 0];
dyBank[5] = [0, 0, 1, 1];
dxBank[6] = [0, 1, - 1, - 2];
dyBank[6] = [0, 0, 0, 0];
dxBank[7] = [0, 1, 1, 0];
dyBank[7] = [0, 0, 1, 1];


// FUNCTIONS

function init() /*: -> Undef */ {
    resetGame();
}


function view_onOpen() /*: -> Undef */ {

    options.putDefaultValue("keyLeftHR", "Left");
    options.putDefaultValue("keyLeft", 37);
    options.putDefaultValue("keyRightHR", "Right");
    options.putDefaultValue("keyRight", 39);
    options.putDefaultValue("keyRotateHR", "Up");
    options.putDefaultValue("keyRotate", 38);
    options.putDefaultValue("keyDownHR", "Down");
    options.putDefaultValue("keyDown", 40);
    options.putDefaultValue("keyFallHR", "Space");
    options.putDefaultValue("keyFall", 32);
    options.putDefaultValue("tetrisMusic", true);

    plugin.onAddCustomMenuItems = AddCustomMenuItems;

    for (var x = 0; x < 10; x++) {
        for (var y = 0; y < 16; y++) {
            divBackground.appendElement("<div name=\"s" + y + "_" + x + "\" x=\"" + (x * 15) + "\" y=\"" + (y * 15) + "\" height=\"15\" width=\"15\" background=\"#FFC000\" />");
        }
    }
    divBackground.focus();
    init();
    labelStatus.visible = false;
    btnStart.visible = true;
}

// Adds our plugin specific items to the menu


function AddCustomMenuItems(menu) /*: Menu -> Undef */ {
    menu.AddItem(strMenuNew, 0, OnMenuClicked);
}

function OnMenuClicked(itemText) /*: {} -> Undef */ {
    if (itemText == strMenuNew) {
        gameStart();
    }
}

function gameLeave() /*: -> Undef */ {
    pause();
    labelStatus.innerText = strPaused;
    labelStatus.visible = true;
}

function gameFocus() /*: -> Undef */ {
    if (gamePaused) {
        resume();
        labelStatus.visible = false;
    }
}

function gameStart() /*: -> Undef */ {
    init();
    start();
    btnStart.visible = false;
    labelStatus.visible = false;
    divBackground.focus();
}

function gameKeyDown() /*: -> Undef */ {
    if (!gameStarted || gamePaused) return;
    //gadget.debug.trace("KeyDown: "+event.keyCode);
    switch (event.keyCode) {
    case options.getValue("keyLeft"):
        moveleft();
        break;
    case options.getValue("keyRight"):
        moveright();
        break;
    case options.getValue("keyRotate"):
        rotate();
        break;
    case options.getValue("keyDown"):
        movedown();
        break;
    case options.getValue("keyFall"):
        fall();
        break;
    }
}

function gameKeyUp() /*: -> Undef */ {
    //gadget.debug.trace("KeyUp: "+event.keyCode);
}


function resetGame() /*: -> Undef */ {
    for (var i = 0; i < boardHeight; i++) {
        for (var j = 0; j < boardWidth; j++) {
            f[i][j] = 0;
            if (boardLoaded) {
                divBackground.children.item("s" + i + "_" + j).background = getBackground(0);
            }
        }
    }
    gameStarted = false;
    gamePaused = false;
    nLines = 0;
    Level = 1;
    infoLines.innerText = nLines;
    infoLevel.innerText = Level;
    serialN = 0;
    skyline = boardHeight - 1;
}

function start() /*: -> Undef */ {
    if (gameStarted) {
        if (!boardLoaded) return;
        if (gamePaused) resume();
        return;
    }
    getPiece();
    drawPiece();
    gameStarted = true;
    gamePaused = false;
    timerID = setTimeout("play()", speed);
    view.caption = GADGET_NAME;
    playMusic();
}

function playMusic() /*: -> Undef */ {
    if (options.getValue("tetrisMusic")) {
        tetrisMusic = framework.audio.play(tetrisMusicSrc);
        tetrisMusic.onstatechange = function (clip, new_state) /*: {} * Int -> Undef */ {
            ClipStateChange(clip, new_state);
        };
    }
}

function ClipStateChange(clip, new_state) /*: {} * Int -> Undef */ {
    gadget.debug.trace("State changed to " + new_state);
    if (new_state == gddSoundStateStopped) {
        gadget.debug.trace("Restart Music");
        tetrisMusic = null;
        setTimeout("playMusic();", 500);
    }
}

function pause() /*: -> Undef  */ {
    if (boardLoaded && gameStarted) {
        if (gamePaused) {
            //resume(); 
            return;
        }
        clearTimeout(timerID);
        gamePaused = true;
        view.caption = GADGET_NAME + " (" + strPaused + ")";
        if (options.getValue("tetrisMusic")) {
            tetrisMusic.pause();
        }
    }
}

function resume() /*: -> Undef */ {
    if (boardLoaded && gameStarted && gamePaused) {
        play();
        gamePaused = false;
        view.caption = GADGET_NAME;
        if (options.getValue("tetrisMusic")) {
            tetrisMusic.play();
        }
    }
}

function play() /*: -> Undef */ {
    if (movedown()) {
        timerID = setTimeout("play()", speed);
        return;
    }
    else {
        fillMatrix();
        removeLines();
        if (skyline > 0 && getPiece()) {
            timerID = setTimeout("play()", speed);
            return;
        }
        else {
            /*            activeL_ = 0;
            activeU_ = 0;
            activeR_ = 0;
            activeD_ = 0;*/
            gameStarted = false;
            labelStatus.innerText = strGameOver;
            labelStatus.visible = true;
            btnStart.visible = true;
            if (options.getValue("tetrisMusic")) {
                tetrisMusic.stop();
            }
        }
    }
}

function fillMatrix() /*: -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        X = curX + dx[k];
        Y = curY + dy[k];
        if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth) {
            f[Y][X] = curPiece;
            if (Y < skyline) skyline = Y;
        }
    }
}

function removeLines() /*: -> Undef */ {
    for (var i = 0; i < boardHeight; i++) {
        var gapFound = 0;
        for (var j = 0; j < boardWidth; j++) {
            if (f[i][j] == 0) {
                gapFound = 1;
                break;
            }
        }
        if (gapFound) continue;
        for (var k = i; k >= skyline; k--) {
            for (var j = 0; j < boardWidth; j++) {
                f[k][j] = f[k - 1][j];
                divBackground.children.item("s" + k + "_" + j).background = getBackground(f[k][j]);
            }
        }
        for (var j = 0; j < boardWidth; j++) {
            f[0][j] = 0;
            divBackground.children.item("s" + 0 + "_" + j).background = getBackground(0);
        }
        nLines++;
        skyline++;
        infoLines.innerText = nLines;
        gadget.debug.trace("Lines achieved: " + nLines);
        if (nLines % 5 == 0) {
            Level++;
            if (Level > 10) Level = 10;
        }
        speed = speed0 - speedK * Level;
        infoLevel.innerText = Level;
        gadget.debug.trace("Level is now: " + Level);
    }
}

function drawPiece() /*: -> Undef */ {
    if (boardLoaded) {
        for (var k = 0; k < nSquares; k++) {
            X = curX + dx[k];
            Y = curY + dy[k];
            if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth && f[Y][X] != - curPiece) {
                divBackground.children.item("s" + Y + "_" + X).background = getBackground(curPiece);
                // eval('self.f1.document.s'+Y+'_'+X+'.src=Img'+curPiece+'.src');
                f[Y][X] = -curPiece;
            }
            X = xToErase[k];
            Y = yToErase[k];
            if (f[Y][X] == 0) {
                divBackground.children.item("s" + Y + "_" + X).background = getBackground(0);
                //eval('self.f1.document.s'+Y+'_'+X+'.src=Img0.src');
            }
        }
    }
}

function erasePiece() /*: -> Undef */ {
    if (boardLoaded) {
        for (var k = 0; k < nSquares; k++) {
            X = curX + dx[k];
            Y = curY + dy[k];
            if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth) {
                xToErase[k] = X;
                yToErase[k] = Y;
                f[Y][X] = 0;
            }
        }
    }
}

function pieceFits(X, Y) /*: Int * Int -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        var theX = X + dx_[k];
        var theY = Y + dy_[k];
        if (theX < 0 || theX >= boardWidth || theY >= boardHeight) return 0;
        if (theY > - 1 && f[theY][theX] > 0) return 0;
    }
    return 1;
}

function moveleft() /*: -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        dx_[k] = dx[k];
        dy_[k] = dy[k];
    }
    if (pieceFits(curX - 1, curY)) {
        erasePiece();
        curX--;
        drawPiece();
    }
}

function moveright() /*: -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        dx_[k] = dx[k];
        dy_[k] = dy[k];
    }
    if (pieceFits(curX + 1, curY)) {
        erasePiece();
        curX++;
        drawPiece();
    }
}

function rotate() /*: -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        dx_[k] = dy[k];
        dy_[k] = -dx[k];
    }
    if (pieceFits(curX, curY)) {
        erasePiece();
        for (var k = 0; k < nSquares; k++) {
            dx[k] = dx_[k];
            dy[k] = dy_[k];
        }
        drawPiece();
    }
}

function movedown() /*: -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        dx_[k] = dx[k];
        dy_[k] = dy[k];
    }
    if (pieceFits(curX, curY + 1)) {
        erasePiece();
        curY++;
        drawPiece();
        return 1;
    }
    return 0;
}

function fall() /*: -> Undef */ {
    for (var k = 0; k < nSquares; k++) {
        dx_[k] = dx[k];
        dy_[k] = dy[k];
    }
    if (!pieceFits(curX, curY + 1)) return;
    clearTimeout(timerID);
    erasePiece();
    while (pieceFits(curX, curY + 1)) {
      curY++;
    }
    drawPiece();
    timerID = setTimeout("play()", speed);
}

function getPiece(N) /*: Int -> Undef */ {
    curPiece = (getPiece.arguments.length == 0) ? 1 + Math.floor(nTypes * Math.random()) : N;
    curX = 5;
    curY = 0;
    for (var k = 0; k < nSquares; k++) {
        dx[k] = dxBank[curPiece][k];
        dy[k] = dyBank[curPiece][k];
    }
    for (var k = 0; k < nSquares; k++) {
        dx_[k] = dx[k];
        dy_[k] = dy[k];
    }
    if (pieceFits(curX, curY)) {
        drawPiece();
        return 1;
    }
    return 0;
}

function getBackground(index) /*: Int -> String */ {
    if (index == 0) {
        return "";
    }
    else {
        return "images\\block" + index + ".png";
    }
}