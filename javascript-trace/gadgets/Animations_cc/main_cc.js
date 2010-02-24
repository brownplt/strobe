var FADE_DURATION = 3000;
var FADE_INTERVAL = 6000;
var View_onOpen = __typedjs(function  ()
                            {
                              fadeImages();
                              view.setInterval(fadeImages,FADE_INTERVAL);
                            },
                            undefined,
                            "View_onOpen",
                            0);
var isPuppyTurn = true;
var fadeImages = __typedjs(function  ()
                           {
                             var outImage;
                             var inImage;
                             if (isPuppyTurn)
                             {
                               outImage = kitty;
                               inImage = puppy;
                             }
                             else {
                                    outImage = puppy;
                                    inImage = kitty;
                                  };
                             view.beginAnimation(__typedjs(function  ()
                                                           {
                                                             setElementOpacity(outImage);
                                                           },
                                                           arguments.callee,
                                                           "",
                                                           0),
                                                 255,
                                                 0,
                                                 FADE_DURATION);
                             view.beginAnimation(__typedjs(function  ()
                                                           {
                                                             setElementOpacity(inImage);
                                                           },
                                                           arguments.callee,
                                                           "",
                                                           1),
                                                 0,
                                                 255,
                                                 FADE_DURATION);
                             isPuppyTurn = ! isPuppyTurn;
                           },
                           undefined,
                           "fadeImages",
                           1);
var setElementOpacity = __typedjs(function  (element)
                                  {
                                    element.opacity = event.value;
                                  },
                                  undefined,
                                  "setElementOpacity",
                                  2);
