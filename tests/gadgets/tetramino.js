//note: took out "getLevel" since it was never used.

var tetris = null;

function view_onOpen() /*: -> Void */ {

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

    for (x = 0; x < 10; x++) {
        for (y = 0; y < 16; y++) {
            divBackground.appendElement("<div name=\"s" + y + "_" + x + "\" x=\"" + (x * 15) + "\" y=\"" + (y * 15) + "\" height=\"15\" width=\"15\" background=\"#FFC000\" />");
        }
    }
    divBackground.focus();
    init();
    labelStatus.visible = false;
    btnStart.visible = true;
}

// Adds our plugin specific items to the menu


function AddCustomMenuItems(menu) /*: Menu -> Void */ {
    menu.AddItem(strMenuNew, 0, OnMenuClicked);
}

function OnMenuClicked(itemText) /*: {} -> Void */ {
    if (itemText == strMenuNew) {
        gameStart();
    }
}

function gameLeave() /*: -> Void */ {
    pause();
    labelStatus.innerText = strPaused;
    labelStatus.visible = true;
}

function gameFocus() /*: -> Void */ {
    if (gamePaused) {
        resume();
        labelStatus.visible = false;
    }
}

function gameStart() /*: -> Void */ {
    init();
    start();
    btnStart.visible = false;
    labelStatus.visible = false;
    divBackground.focus();
}

function gameKeyDown() /*: -> Void */ {
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

function gameKeyUp() {
    //gadget.debug.trace("KeyUp: "+event.keyCode);
}


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
var gamePaused = 0;
var gameStarted = 0;
var sayingBye = 0;
var timerID = null;

var tetrisMusic = null;
var tetrisMusicSrc = storage.extract("music/Korobeiniki.mid");

// ARRAYS
var f = new Array();
for (i = 0; i < 20; i++) {
    f[i] = new Array();
    for (j = 0; j < 20; j++) {
        f[i][j] = 0;
    }
}

var xToErase = new Array(0, 0, 0, 0);
var yToErase = new Array(0, 0, 0, 0);
var dx = new Array(0, 0, 0, 0);
var dy = new Array(0, 0, 0, 0);
var dx_ = new Array(0, 0, 0, 0);
var dy_ = new Array(0, 0, 0, 0);
var dxBank = new Array();
var dyBank = new Array();
dxBank[1] = new Array(0, 1, - 1, 0);
dyBank[1] = new Array(0, 0, 0, 1);
dxBank[2] = new Array(0, 1, - 1, - 1);
dyBank[2] = new Array(0, 0, 0, 1);
dxBank[3] = new Array(0, 1, - 1, 1);
dyBank[3] = new Array(0, 0, 0, 1);
dxBank[4] = new Array(0, - 1, 1, 0);
dyBank[4] = new Array(0, 0, 1, 1);
dxBank[5] = new Array(0, 1, - 1, 0);
dyBank[5] = new Array(0, 0, 1, 1);
dxBank[6] = new Array(0, 1, - 1, - 2);
dyBank[6] = new Array(0, 0, 0, 0);
dxBank[7] = new Array(0, 1, 1, 0);
dyBank[7] = new Array(0, 0, 1, 1);


// FUNCTIONS

function resetGame() /*: -> Void */ {
    for (var i = 0; i < boardHeight; i++) {
        for (var j = 0; j < boardWidth; j++) {
            f[i][j] = 0;
            if (boardLoaded) {
                divBackground.children.item("s" + i + "_" + j).background = getBackground(0);
            }
        }
    }
    gameStarted = 0;
    gamePaused = 0;
    nLines = 0;
    Level = 1;
    infoLines.innerText = nLines;
    infoLevel.innerText = Level;
    serialN = 0;
    skyline = boardHeight - 1;
}

function init() /*: -> Void */ {
    resetGame();
}

function start() /*: -> Void */ {
    if (gameStarted) {
        if (!boardLoaded) return;
        if (gamePaused) resume();
        return;
    }
    getPiece();
    drawPiece();
    gameStarted = 1;
    gamePaused = 0;
    timerID = setTimeout("play()", speed);
    view.caption = GADGET_NAME;
    playMusic();
}

function playMusic() /*: -> Void */ {
    if (options.getValue("tetrisMusic")) {
        tetrisMusic = framework.audio.play(tetrisMusicSrc);
        tetrisMusic.onstatechange = function (clip, new_state) /*: {} * Int -> Void */ {
            ClipStateChange(clip, new_state);
        };
    }
}

function ClipStateChange(clip, new_state) /*: {} * Int -> Void */ {
    gadget.debug.trace("State changed to " + new_state);
    if (new_state == gddSoundStateStopped) {
        gadget.debug.trace("Restart Music");
        tetrisMusic = null;
        setTimeout("playMusic();", 500);
    }
}

function pause() /*: -> Void  */ {
    if (boardLoaded && gameStarted) {
        if (gamePaused) {
            //resume(); 
            return;
        }
        clearTimeout(timerID);
        gamePaused = 1;
        view.caption = GADGET_NAME + " (" + strPaused + ")";
        if (options.getValue("tetrisMusic")) {
            tetrisMusic.pause();
        }
    }
}

function resume() /*: -> Void */ {
    if (boardLoaded && gameStarted && gamePaused) {
        play();
        gamePaused = 0;
        view.caption = GADGET_NAME;
        if (options.getValue("tetrisMusic")) {
            tetrisMusic.play();
        }
    }
}

function play() /*: -> Void */ {
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
            activeL_ = 0;
            activeU_ = 0;
            activeR_ = 0;
            activeD_ = 0;
            gameStarted = 0;
            labelStatus.innerText = strGameOver;
            labelStatus.visible = true;
            btnStart.visible = true;
            if (options.getValue("tetrisMusic")) {
                tetrisMusic.stop();
            }
        }
    }
}

function fillMatrix() /*: -> Void */ {
    for (var k = 0; k < nSquares; k++) {
        X = curX + dx[k];
        Y = curY + dy[k];
        if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth) {
            f[Y][X] = curPiece;
            if (Y < skyline) skyline = Y;
        }
    }
}

function removeLines() /*: -> Void */ {
    for (var i = 0; i < boardHeight; i++) {
        gapFound = 0;
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

function drawPiece() /*: -> Void */ {
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

function erasePiece() /*: -> Void */ {
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

function pieceFits(X, Y) /*: Int * Int - Void */ {
    for (var k = 0; k < nSquares; k++) {
        theX = X + dx_[k];
        theY = Y + dy_[k];
        if (theX < 0 || theX >= boardWidth || theY >= boardHeight) return 0;
        if (theY > - 1 && f[theY][theX] > 0) return 0;
    }
    return 1;
}

function moveleft() /*: -> Void */ {
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

function moveright() /*: -> Void */ {
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

function rotate() /*: -> Void */ {
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

function movedown() /*: -> Void */ {
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

function fall() /*: -> Void */ {
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

function getPiece(N) /*: -> Void */ {
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