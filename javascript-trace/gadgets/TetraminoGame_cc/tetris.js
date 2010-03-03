// *** TYPES FOR gadgets/TetraminoGame_cc/tetris.js *** 
/*::
  function resetGame : ( -> Void)
  function init : ( -> Void)
  function start : ( -> Void)
  function playMusic : ( -> Void)
    function tetrisMusic.onstatechange : (Dom * Int -> Void)
  function ClipStateChange : (Dom * Int -> Void)
  function pause : ( -> Void)
  function resume : ( -> Void)
  function play : ( -> Void)
  function fillMatrix : ( -> Void)
  function removeLines : ( -> Void)
  function drawPiece : ( -> Void)
  function erasePiece : ( -> Void)
  function pieceFits : (Int * Int -> Int)
  function moveleft : ( -> Void)
  function moveright : ( -> Void)
  function rotate : ( -> Void)
  function movedown : ( -> Int)
  function fall : ( -> Void)
  function getPiece : ( -> Int)
  function getBackground : (Int -> String)
*/
//note: took out "getLevel" since it was never used.

// PARAMETERS
nSquares = 4;
nTypes = 7;
boardHeight = 16;
boardWidth = 10;
Level = 1;
speed0 = 700;
speedK = 60;
speed = speed0 - speedK * Level;
nLines = 0;

// GLOBAL VARIABLES
curX = 1;
curY = 1;
skyline = boardHeight - 1;
serialN = 0;

boardLoaded = 1;
gamePaused = 0;
gameStarted = 0;
sayingBye = 0;
timerID = null;

tetrisMusic = null;
tetrisMusicSrc = storage.extract("music/Korobeiniki.mid");

// ARRAYS
f = new Array();
for (i = 0; i < 20; i++) {
    f[i] = new Array();
    for (j = 0; j < 20; j++) {
        f[i][j] = 0;
    }
}

xToErase = new Array(0, 0, 0, 0);
yToErase = new Array(0, 0, 0, 0);
dx = new Array(0, 0, 0, 0);
dy = new Array(0, 0, 0, 0);
dx_ = new Array(0, 0, 0, 0);
dy_ = new Array(0, 0, 0, 0);
dxBank = new Array();
dyBank = new Array();
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

function resetGame() {
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

function init() {
    resetGame();
}

function start() {
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

function playMusic() {
    if (options.getValue("tetrisMusic")) {
        tetrisMusic = framework.audio.play(tetrisMusicSrc);
        tetrisMusic.onstatechange = function (clip, new_state) {
            ClipStateChange(clip, new_state);
        };
    }
}

function ClipStateChange(clip, new_state) {
    gadget.debug.trace("State changed to " + new_state);
    if (new_state == gddSoundStateStopped) {
        gadget.debug.trace("Restart Music");
        tetrisMusic = null;
        setTimeout("playMusic();", 500);
    }
}

function pause() {
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

function resume() {
    if (boardLoaded && gameStarted && gamePaused) {
        play();
        gamePaused = 0;
        view.caption = GADGET_NAME;
        if (options.getValue("tetrisMusic")) {
            tetrisMusic.play();
        }
    }
}

function play() {
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

function fillMatrix() {
    for (var k = 0; k < nSquares; k++) {
        X = curX + dx[k];
        Y = curY + dy[k];
        if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth) {
            f[Y][X] = curPiece;
            if (Y < skyline) skyline = Y;
        }
    }
}

function removeLines() {
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

function drawPiece() {
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

function erasePiece() {
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

function pieceFits(X, Y) {
    for (var k = 0; k < nSquares; k++) {
        theX = X + dx_[k];
        theY = Y + dy_[k];
        if (theX < 0 || theX >= boardWidth || theY >= boardHeight) return 0;
        if (theY > - 1 && f[theY][theX] > 0) return 0;
    }
    return 1;
}

function moveleft() {
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

function moveright() {
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

function rotate() {
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

function movedown() {
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

function fall() {
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

function getPiece(N) {
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

function getBackground(index) {
    if (index == 0) {
        return "";
    }
    else {
        return "images\\block" + index + ".png";
    }
}