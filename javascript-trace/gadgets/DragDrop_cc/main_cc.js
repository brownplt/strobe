var Utils = (__typedjs(function  ()
                       {
                         var createDragFilesImagesList = __typedjs(function  (obj)
                                                                   {
                                                                     var files = [];
                                                                     if (! obj)
                                                                     {
                                                                       return files;
                                                                     };
                                                                     var e = __new(Enumerator,
                                                                                   [obj]);
                                                                     var validExtensions = {png: true, gif: true, jpg: true, jpeg: true};
                                                                     while (! e.atEnd())
                                                                     {
                                                                       var path = e.item();
                                                                       var extension = extractExtension(path).toLowerCase();
                                                                       if (validExtensions[extension] === true)
                                                                       {
                                                                         files.push(path + "");
                                                                       };
                                                                       e.moveNext();
                                                                     };
                                                                     return files;
                                                                   },
                                                                   arguments.callee,
                                                                   "createDragFilesImagesList",
                                                                   0);
                         var extractExtension = __typedjs(function  (s)
                                                          {
                                                            return s.substring(s.lastIndexOf(".") + 1);
                                                          },
                                                          arguments.callee,
                                                          "extractExtension",
                                                          1);
                         return {createDragFilesImagesList: createDragFilesImagesList, extractExtension: extractExtension};
                       },
                       undefined,
                       "Utils",
                       0))();
var ViewHandlers = (__typedjs(function  ()
                              {
                                var onOpen = __typedjs(function  ()
                                                       {
                                                         label.innerText = strings.DRAG_IMAGES_HERE;
                                                       },
                                                       arguments.callee,
                                                       "onOpen",
                                                       0);
                                var onDragDrop = __typedjs(function  ()
                                                           {
                                                             var images = Utils.createDragFilesImagesList(event.dragFiles);
                                                             var MAX_DISPLAY = 4;
                                                             var numImages = images.length;
                                                             var i;
                                                             image0.src = "";
                                                             image1.src = "";
                                                             image2.src = "";
                                                             image3.src = "";
                                                             for (i = 0; i < numImages && i < MAX_DISPLAY; ++i)
                                                             {
                                                               switch (i)
                                                               {case
                                                                0 :
                                                                  image0.src = images[i];
                                                                  break;
                                                                case
                                                                1 :
                                                                  image1.src = images[i];
                                                                  break;
                                                                case
                                                                2 :
                                                                  image2.src = images[i];
                                                                  break;
                                                                case
                                                                3 :
                                                                  image3.src = images[i];
                                                                  break;};
                                                             };
                                                             label.vAlign = "bottom";
                                                             if (numImages > MAX_DISPLAY)
                                                             {
                                                               var numOthers = numImages - MAX_DISPLAY;
                                                               label.innerText = "... " + strings.AND + " " + numOthers + " " + strings.OTHERS + " ...";
                                                             }
                                                             else {
                                                                    label.innerText = "";
                                                                  };
                                                           },
                                                           arguments.callee,
                                                           "onDragDrop",
                                                           1);
                                var onDragOver = __typedjs(function  ()
                                                           {
                                                             var images = Utils.createDragFilesImagesList(event.dragFiles);
                                                             var numImages = images.length;
                                                             if (numImages === 0)
                                                             {
                                                               event.returnValue = false;
                                                               return;
                                                             };
                                                             label.innerText = strings.THERE_ARE + " " + numImages + " " + strings.IMAGES;
                                                           },
                                                           arguments.callee,
                                                           "onDragOver",
                                                           2);
                                var onDragOut = __typedjs(function  ()
                                                          {
                                                            label.innerText = strings.DRAG_IMAGES_HERE;
                                                          },
                                                          arguments.callee,
                                                          "onDragOut",
                                                          3);
                                return {onOpen: onOpen, onDragDrop: onDragDrop, onDragOver: onDragOver, onDragOut: onDragOut};
                              },
                              undefined,
                              "ViewHandlers",
                              1))();
