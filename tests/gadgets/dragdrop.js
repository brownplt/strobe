/*
Copyright (C) 2007 Google Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

//REFACTORED TO WORK WITH TJS
//it's just a bunch of mututally recursive functions which
//happen to be put into structs instead of vars..

/**
 * @fileoverview Drag and Drop Sample Gadget
 * 
 * Simple example of drag and drop features
 * 
 * 'view', 'basicElement' (and elements inheriting from 'basicElement') 
 * have a property called 'dropTarget'.
 * 
 * If set to true, the element's ondrag* events will fire 
 * when a drag/drop operation is initiated by the user.
 * 
 * See main.xml and take a look at the main 'div':
 * 'dropTarget' is set to true 
 * and the ondrag* events are set to event handlers.
 * 
 * If you wish to cancel a drag/drop operation, 
 * set 'event.returnValue' to 'false' (Boolean) within the event handler.
 * 
 * Another thing to know, is within the handler
 * 'event.dragFiles' is a collection object containing the paths of the file(s)
 * in the drag operation.
 */

/**
 * Utils namespace
 */
var Utils = (function(){

/**
 * Convert event.dragfiles object to an array of strings
 * @param {Object} obj collection of drag files
 * @return {Array} Array of filepaths
 */
var createDragFilesImagesList = function(obj) {
  var files = [];

  if (!obj) {
    return files;
  }

  var e = new Enumerator(obj);

  var validExtensions = {
    png: true,
    gif: true,
    jpg: true,
    jpeg: true };
    
  while (!e.atEnd()) {
    var path = e.item();
    var extension = extractExtension(path).toLowerCase();
    
    if (validExtensions[extension] === true) {
      files.push(path + '');
    }
    e.moveNext();
  }
    
  return files;
};

/**
 * Extract the extension from a path
 * @param {String} The path
 * @return {String} The extension
 */
var extractExtension = function(s) {
  return s.substring(s.lastIndexOf('.') + 1); 
};

return {
  createDragFilesImagesList: createDragFilesImagesList,
  extractExtension: extractExtension
};

})();

/**
 * ViewHandlers namespace
 */
var ViewHandlers = (function(){

var onOpen = function() {
  label.innerText = strings.DRAG_IMAGES_HERE;
};

/**
 * Executed when the user drops an object
 */  
var onDragDrop = function() {
  var images = Utils.createDragFilesImagesList(event.dragFiles);
  
  var MAX_DISPLAY = 4;
  var numImages = images.length;
  
  var i;

  // clear images
  image0.src = '';  
  image1.src = '';
  image2.src = '';
  image3.src = '';
  
  for (i = 0; i < numImages && i < MAX_DISPLAY; ++i) {
    switch (i) {
      case 0:
        image0.src = images[i];
        break;
      case 1:
        image1.src = images[i];
        break;
      case 2:
        image2.src = images[i];
        break;
      case 3:
        image3.src = images[i];
        break;
    }  
  }
  
  label.vAlign = 'bottom';
  
  if (numImages > MAX_DISPLAY) {
    var numOthers = numImages - MAX_DISPLAY;
    label.innerText = '... ' + strings.AND + ' ' + numOthers + ' ' + strings.OTHERS + ' ...';
  } else {
    label.innerText = '';
  }
};

/**
 * Executed when the user drags an object over
 */  
var onDragOver = function() {
  var images = Utils.createDragFilesImagesList(event.dragFiles);
  var numImages = images.length;
  
  // There are no images, cancel the default event
  if (numImages === 0) {
    event.returnValue = false;
    return;
  }
  
  label.innerText = strings.THERE_ARE + ' ' + numImages + ' ' + strings.IMAGES;
};

/**
 * Executed when the user drags out
 */  
var onDragOut = function() {
  label.innerText = strings.DRAG_IMAGES_HERE;
};


return {
  onOpen: onOpen,
  onDragDrop: onDragDrop,
  onDragOver: onDragOver,
  onDragOut: onDragOut,
};

})();