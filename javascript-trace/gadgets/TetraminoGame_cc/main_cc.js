var tetris = null;
var view_onOpen = __typedjs(function  ()
                            {
                              options.putDefaultValue("keyLeftHR","Left");
                              options.putDefaultValue("keyLeft",37);
                              options.putDefaultValue("keyRightHR","Right");
                              options.putDefaultValue("keyRight",39);
                              options.putDefaultValue("keyRotateHR","Up");
                              options.putDefaultValue("keyRotate",38);
                              options.putDefaultValue("keyDownHR","Down");
                              options.putDefaultValue("keyDown",40);
                              options.putDefaultValue("keyFallHR","Space");
                              options.putDefaultValue("keyFall",32);
                              options.putDefaultValue("tetrisMusic",true);
                              //plugin.onAddCustomMenuItems = AddCustomMenuItems;
                              for (x = 0; x < 10; x++)
                              {
                                for (y = 0; y < 16; y++)
                                {
                                  divBackground.appendElement("<div name=\"s" + y + "_" + x + "\" x=\"" + (x * 15) + "\" y=\"" + (y * 15) + "\" height=\"15\" width=\"15\" background=\"#FFC000\" />");
                                };
                              };
                              divBackground.focus();
                              init();
                              labelStatus.visible = false;
                              btnStart.visible = true;
                            },
                            undefined,
                            "view_onOpen",
                            "gadgets/TetraminoGame_cc/main.js",
                            0);
var AddCustomMenuItems = __typedjs(function  (menu)
                                   {
                                     menu.AddItem(strMenuNew,0,OnMenuClicked);
                                   },
                                   undefined,
                                   "AddCustomMenuItems",
                                   "gadgets/TetraminoGame_cc/main.js",
                                   1);
var OnMenuClicked = __typedjs(function  (itemText)
                              {
                                if (itemText == strMenuNew)
                                {
                                  gameStart();
                                };
                              },
                              undefined,
                              "OnMenuClicked",
                              "gadgets/TetraminoGame_cc/main.js",
                              2);
var gameLeave = __typedjs(function  ()
                          {
                            pause();
                            labelStatus.innerText = strPaused;
                            labelStatus.visible = true;
                          },
                          undefined,
                          "gameLeave",
                          "gadgets/TetraminoGame_cc/main.js",
                          3);
var gameFocus = __typedjs(function  ()
                          {
                            if (gamePaused)
                            {
                              resume();
                              labelStatus.visible = false;
                            };
                          },
                          undefined,
                          "gameFocus",
                          "gadgets/TetraminoGame_cc/main.js",
                          4);
var gameStart = __typedjs(function  ()
                          {
                            init();
                            start();
                            btnStart.visible = false;
                            labelStatus.visible = false;
                            divBackground.focus();
                          },
                          undefined,
                          "gameStart",
                          "gadgets/TetraminoGame_cc/main.js",
                          5);
var gameKeyDown = __typedjs(function  ()
                            {
                              if (! gameStarted || gamePaused)
                              return;
                              switch (event.keyCode)
                              {case
                               options.getValue("keyLeft") :
                                 moveleft();
                                 break;
                               case
                               options.getValue("keyRight") :
                                 moveright();
                                 break;
                               case
                               options.getValue("keyRotate") :
                                 rotate();
                                 break;
                               case
                               options.getValue("keyDown") :
                                 movedown();
                                 break;
                               case
                               options.getValue("keyFall") :
                                 fall();
                                 break;};
                            },
                            undefined,
                            "gameKeyDown",
                            "gadgets/TetraminoGame_cc/main.js",
                            6);
var gameKeyUp = __typedjs(function  ()
                          {
                          },
                          undefined,
                          "gameKeyUp",
                          "gadgets/TetraminoGame_cc/main.js",
                          7);
