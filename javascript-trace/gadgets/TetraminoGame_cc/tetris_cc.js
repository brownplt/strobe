__initnumargs("gadgets/TetraminoGame_cc/tetris.js",
              [0,0,0,0,2,2,0,0,0,0,0,0,0,2,0,0,0,0,0,1,1]);
var nSquares = 4;
var nTypes = 7;
var boardHeight = 16;
var boardWidth = 10;
var Level = 1;
var speed0 = 700;
var speedK = 60;
var speed = speed0 - speedK * Level;
var nLines = 0;
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
var f = __new(Array,[]);
for (i = 0; i < 20; i++)
{
  f[i] = __new(Array,[]);
  for (j = 0; j < 20; j++)
  {
    f[i][j] = 0;
  };
};
var xToErase = __new(Array,[0,0,0,0]);
var yToErase = __new(Array,[0,0,0,0]);
var dx = __new(Array,[0,0,0,0]);
var dy = __new(Array,[0,0,0,0]);
var dx_ = __new(Array,[0,0,0,0]);
var dy_ = __new(Array,[0,0,0,0]);
var dxBank = __new(Array,[]);
var dyBank = __new(Array,[]);
dxBank[1] = __new(Array,[0,1,- 1,0]);
dyBank[1] = __new(Array,[0,0,0,1]);
dxBank[2] = __new(Array,[0,1,- 1,- 1]);
dyBank[2] = __new(Array,[0,0,0,1]);
dxBank[3] = __new(Array,[0,1,- 1,1]);
dyBank[3] = __new(Array,[0,0,0,1]);
dxBank[4] = __new(Array,[0,- 1,1,0]);
dyBank[4] = __new(Array,[0,0,1,1]);
dxBank[5] = __new(Array,[0,1,- 1,0]);
dyBank[5] = __new(Array,[0,0,1,1]);
dxBank[6] = __new(Array,[0,1,- 1,- 2]);
dyBank[6] = __new(Array,[0,0,0,0]);
dxBank[7] = __new(Array,[0,1,1,0]);
dyBank[7] = __new(Array,[0,0,1,1]);
var resetGame = __typedjs(function  ()
                          {
                            for (var i = 0; i < boardHeight; i++)
                            {
                              for (var j = 0; j < boardWidth; j++)
                              {
                                f[i][j] = 0;
                                if (boardLoaded)
                                {
                                  divBackground.children.item("s" + i + "_" + j).background = getBackground(0);
                                };
                              };
                            };
                            gameStarted = 0;
                            gamePaused = 0;
                            nLines = 0;
                            Level = 1;
                            infoLines.innerText = nLines;
                            infoLevel.innerText = Level;
                            serialN = 0;
                            skyline = boardHeight - 1;
                          },
                          0,
                          "resetGame",
                          "gadgets/TetraminoGame_cc/tetris.js",
                          0);
var init = __typedjs(function  ()
                     {
                       resetGame();
                     },
                     0,
                     "init",
                     "gadgets/TetraminoGame_cc/tetris.js",
                     1);
var start = __typedjs(function  ()
                      {
                        if (gameStarted)
                        {
                          if (! boardLoaded)
                          return;
                          if (gamePaused)
                          resume();
                          return;
                        };
                        getPiece();
                        drawPiece();
                        gameStarted = 1;
                        gamePaused = 0;
                        timerID = setTimeout("play()",speed);
                        view.caption = GADGET_NAME;
                        playMusic();
                      },
                      0,
                      "start",
                      "gadgets/TetraminoGame_cc/tetris.js",
                      2);
var playMusic = __typedjs(function  ()
                          {
                            if (options.getValue("tetrisMusic"))
                            {
                              tetrisMusic = framework.audio.play(tetrisMusicSrc);
                              tetrisMusic.onstatechange = __typedjs(function  (clip,new_state)
                                                                    {
                                                                      ClipStateChange(clip,
                                                                                      new_state);
                                                                    },
                                                                    1,
                                                                    "tetrisMusic.onstatechange",
                                                                    "gadgets/TetraminoGame_cc/tetris.js",
                                                                    4);
                            };
                          },
                          0,
                          "playMusic",
                          "gadgets/TetraminoGame_cc/tetris.js",
                          3);
var ClipStateChange = __typedjs(function  (clip,new_state)
                                {
                                  gadget.debug.trace("State changed to " + new_state);
                                  if (new_state == gddSoundStateStopped)
                                  {
                                    gadget.debug.trace("Restart Music");
                                    tetrisMusic = null;
                                    setTimeout("playMusic();",500);
                                  };
                                },
                                0,
                                "ClipStateChange",
                                "gadgets/TetraminoGame_cc/tetris.js",
                                5);
var pause = __typedjs(function  ()
                      {
                        if (boardLoaded && gameStarted)
                        {
                          if (gamePaused)
                          {
                            return;
                          };
                          clearTimeout(timerID);
                          gamePaused = 1;
                          view.caption = GADGET_NAME + " (" + strPaused + ")";
                          if (options.getValue("tetrisMusic"))
                          {
                            tetrisMusic.pause();
                          };
                        };
                      },
                      0,
                      "pause",
                      "gadgets/TetraminoGame_cc/tetris.js",
                      6);
var resume = __typedjs(function  ()
                       {
                         if (boardLoaded && gameStarted && gamePaused)
                         {
                           play();
                           gamePaused = 0;
                           view.caption = GADGET_NAME;
                           if (options.getValue("tetrisMusic"))
                           {
                             tetrisMusic.play();
                           };
                         };
                       },
                       0,
                       "resume",
                       "gadgets/TetraminoGame_cc/tetris.js",
                       7);
var play = __typedjs(function  ()
                     {
                       if (movedown())
                       {
                         timerID = setTimeout("play()",speed);
                         return;
                       }
                       else {
                              fillMatrix();
                              removeLines();
                              if (skyline > 0 && getPiece())
                              {
                                timerID = setTimeout("play()",speed);
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
                                     if (options.getValue("tetrisMusic"))
                                     {
                                       tetrisMusic.stop();
                                     };
                                   };
                            };
                     },
                     0,
                     "play",
                     "gadgets/TetraminoGame_cc/tetris.js",
                     8);
var fillMatrix = __typedjs(function  ()
                           {
                             for (var k = 0; k < nSquares; k++)
                             {
                               X = curX + dx[k];
                               Y = curY + dy[k];
                               if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth)
                               {
                                 f[Y][X] = curPiece;
                                 if (Y < skyline)
                                 skyline = Y;
                               };
                             };
                           },
                           0,
                           "fillMatrix",
                           "gadgets/TetraminoGame_cc/tetris.js",
                           9);
var removeLines = __typedjs(function  ()
                            {
                              for (var i = 0; i < boardHeight; i++)
                              {
                                gapFound = 0;
                                for (var j = 0; j < boardWidth; j++)
                                {
                                  if (f[i][j] == 0)
                                  {
                                    gapFound = 1;
                                    break;
                                  };
                                };
                                if (gapFound)
                                continue;
                                for (var k = i; k >= skyline; k--)
                                {
                                  for (var j = 0; j < boardWidth; j++)
                                  {
                                    f[k][j] = f[k - 1][j];
                                    divBackground.children.item("s" + k + "_" + j).background = getBackground(f[k][j]);
                                  };
                                };
                                for (var j = 0; j < boardWidth; j++)
                                {
                                  f[0][j] = 0;
                                  divBackground.children.item("s" + 0 + "_" + j).background = getBackground(0);
                                };
                                nLines++;
                                skyline++;
                                infoLines.innerText = nLines;
                                gadget.debug.trace("Lines achieved: " + nLines);
                                if (nLines % 5 == 0)
                                {
                                  Level++;
                                  if (Level > 10)
                                  Level = 10;
                                };
                                speed = speed0 - speedK * Level;
                                infoLevel.innerText = Level;
                                gadget.debug.trace("Level is now: " + Level);
                              };
                            },
                            0,
                            "removeLines",
                            "gadgets/TetraminoGame_cc/tetris.js",
                            10);
var drawPiece = __typedjs(function  ()
                          {
                            if (boardLoaded)
                            {
                              for (var k = 0; k < nSquares; k++)
                              {
                                X = curX + dx[k];
                                Y = curY + dy[k];
                                if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth && f[Y][X] != - curPiece)
                                {
                                  divBackground.children.item("s" + Y + "_" + X).background = getBackground(curPiece);
                                  f[Y][X] = - curPiece;
                                };
                                X = xToErase[k];
                                Y = yToErase[k];
                                if (f[Y][X] == 0)
                                {
                                  divBackground.children.item("s" + Y + "_" + X).background = getBackground(0);
                                };
                              };
                            };
                          },
                          0,
                          "drawPiece",
                          "gadgets/TetraminoGame_cc/tetris.js",
                          11);
var erasePiece = __typedjs(function  ()
                           {
                             if (boardLoaded)
                             {
                               for (var k = 0; k < nSquares; k++)
                               {
                                 X = curX + dx[k];
                                 Y = curY + dy[k];
                                 if (0 <= Y && Y < boardHeight && 0 <= X && X < boardWidth)
                                 {
                                   xToErase[k] = X;
                                   yToErase[k] = Y;
                                   f[Y][X] = 0;
                                 };
                               };
                             };
                           },
                           0,
                           "erasePiece",
                           "gadgets/TetraminoGame_cc/tetris.js",
                           12);
var pieceFits = __typedjs(function  (X,Y)
                          {
                            for (var k = 0; k < nSquares; k++)
                            {
                              theX = X + dx_[k];
                              theY = Y + dy_[k];
                              if (theX < 0 || theX >= boardWidth || theY >= boardHeight)
                              return 0;
                              if (theY > - 1 && f[theY][theX] > 0)
                              return 0;
                            };
                            return 1;
                          },
                          0,
                          "pieceFits",
                          "gadgets/TetraminoGame_cc/tetris.js",
                          13);
var moveleft = __typedjs(function  ()
                         {
                           for (var k = 0; k < nSquares; k++)
                           {
                             dx_[k] = dx[k];
                             dy_[k] = dy[k];
                           };
                           if (pieceFits(curX - 1,curY))
                           {
                             erasePiece();
                             curX--;
                             drawPiece();
                           };
                         },
                         0,
                         "moveleft",
                         "gadgets/TetraminoGame_cc/tetris.js",
                         14);
var moveright = __typedjs(function  ()
                          {
                            for (var k = 0; k < nSquares; k++)
                            {
                              dx_[k] = dx[k];
                              dy_[k] = dy[k];
                            };
                            if (pieceFits(curX + 1,curY))
                            {
                              erasePiece();
                              curX++;
                              drawPiece();
                            };
                          },
                          0,
                          "moveright",
                          "gadgets/TetraminoGame_cc/tetris.js",
                          15);
var rotate = __typedjs(function  ()
                       {
                         for (var k = 0; k < nSquares; k++)
                         {
                           dx_[k] = dy[k];
                           dy_[k] = - dx[k];
                         };
                         if (pieceFits(curX,curY))
                         {
                           erasePiece();
                           for (var k = 0; k < nSquares; k++)
                           {
                             dx[k] = dx_[k];
                             dy[k] = dy_[k];
                           };
                           drawPiece();
                         };
                       },
                       0,
                       "rotate",
                       "gadgets/TetraminoGame_cc/tetris.js",
                       16);
var movedown = __typedjs(function  ()
                         {
                           for (var k = 0; k < nSquares; k++)
                           {
                             dx_[k] = dx[k];
                             dy_[k] = dy[k];
                           };
                           if (pieceFits(curX,curY + 1))
                           {
                             erasePiece();
                             curY++;
                             drawPiece();
                             return 1;
                           };
                           return 0;
                         },
                         0,
                         "movedown",
                         "gadgets/TetraminoGame_cc/tetris.js",
                         17);
var fall = __typedjs(function  ()
                     {
                       for (var k = 0; k < nSquares; k++)
                       {
                         dx_[k] = dx[k];
                         dy_[k] = dy[k];
                       };
                       if (! pieceFits(curX,curY + 1))
                       return;
                       clearTimeout(timerID);
                       erasePiece();
                       while (pieceFits(curX,curY + 1))
                       {
                         curY++;
                       };
                       drawPiece();
                       timerID = setTimeout("play()",speed);
                     },
                     0,
                     "fall",
                     "gadgets/TetraminoGame_cc/tetris.js",
                     18);
var getPiece = __typedjs(function  (N)
                         {
                           curPiece = (getPiece.arguments.length == 0) ? 1 + Math.floor(nTypes * Math.random()) : N;
                           curX = 5;
                           curY = 0;
                           for (var k = 0; k < nSquares; k++)
                           {
                             dx[k] = dxBank[curPiece][k];
                             dy[k] = dyBank[curPiece][k];
                           };
                           for (var k = 0; k < nSquares; k++)
                           {
                             dx_[k] = dx[k];
                             dy_[k] = dy[k];
                           };
                           if (pieceFits(curX,curY))
                           {
                             drawPiece();
                             return 1;
                           };
                           return 0;
                         },
                         0,
                         "getPiece",
                         "gadgets/TetraminoGame_cc/tetris.js",
                         19);
var getBackground = __typedjs(function  (index)
                              {
                                if (index == 0)
                                {
                                  return "";
                                }
                                else {
                                       return "images\\block" + index + ".png";
                                     };
                              },
                              0,
                              "getBackground",
                              "gadgets/TetraminoGame_cc/tetris.js",
                              20);
