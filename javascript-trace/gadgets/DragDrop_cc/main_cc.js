var ViewHandlers = {};
ViewHandlers.onOpen = __typedjs(function  ()
                                {
                                  label.innerText = strings.DRAG_IMAGES_HERE;
                                },
                                undefined,
                                "ViewHandlers.onOpen",
                                0);
ViewHandlers.onDragDrop = __typedjs(function  ()
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
                                    undefined,
                                    "ViewHandlers.onDragDrop",
                                    1);
ViewHandlers.onDragOver = __typedjs(function  ()
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
                                    undefined,
                                    "ViewHandlers.onDragOver",
                                    2);
ViewHandlers.onDragOut = __typedjs(function  ()
                                   {
                                     label.innerText = strings.DRAG_IMAGES_HERE;
                                   },
                                   undefined,
                                   "ViewHandlers.onDragOut",
                                   3);
var Utils = {};
Utils.createDragFilesImagesList = __typedjs(function  (obj)
                                            {
                                              var files = [];
                                              if (! obj)
                                              {
                                                return files;
                                              };
                                              var e = __new(Enumerator,[obj]);
                                              var validExtensions = {png: true, gif: true, jpg: true, jpeg: true};
                                              while (! e.atEnd())
                                              {
                                                var path = e.item();
                                                var extension = Utils.extractExtension(path).toLowerCase();
                                                if (validExtensions[extension] === true)
                                                {
                                                  files.push(path + "");
                                                };
                                                e.moveNext();
                                              };
                                              return files;
                                            },
                                            undefined,
                                            "Utils.createDragFilesImagesList",
                                            4);
Utils.extractExtension = __typedjs(function  (s)
                                   {
                                     return s.substring(s.lastIndexOf(".") + 1);
                                   },
                                   undefined,
                                   "Utils.extractExtension",
                                   5);
