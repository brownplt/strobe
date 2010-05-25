//Aw this example required tons of changes.
//changes:
//BUG (kinda?):  _ to callback func
//BUG?: Added 4 var to implicit globals
//lift main() up
//loadImage callback strangeness, made a var nullalble
//unhappy change: "new Array" was used to create array of an
//  initial size. had to change it to not. likely makes the
//  code slower.
//2x Math.floor to make sure array access happens properly
// had to add check for img being undefined cause of callback being strange
// uygly 3-part check for local variables not being Null upon calling
//   them. dnno if can refactor in a prettier way. very ugly
//2x downcasts from creating/getting elements
//3x: temporary change normals.push() to normals[normals.length] =.

//changes to the inferred annotatinos:
//abs, drawLight, getDataFromImage, loadImage,


function abs(x) /*: Double -> Double */ {
    return x > 0 ? x : -x;
}

// this two functions were promoted to be global
// to make firefoxs jit happy - URGH
function clamp(x, min, max) /*: (Double + Int) * Int * Int -> (Double + Int) */{
    if(x < min) return min;
    if(x > max) return max-1;
    return x;
}

// this is basically where the magic happens
function drawLight(canvas, ctx, normals, textureData, directionlut, shiny, specularity, lx, ly, lz) /*: HTMLCanvasElement * CanvasRenderingContext2D * Array<(Double + Int)> * Array<Int> * Array<Double> * Int * Double * Int * Int * Int -> Void */ {
    var imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    var data = imgData.data;
    var i = 0;
    var ni = 0;
    var dx = 0.0, dy = 0.0, dz = 0.0;
    var lz2 = lz*lz;

    var mindx = 0;
    var maxdx = 0;
    for(var y = 0; y < canvas.height; y++) {
        for(var x = 0; x < canvas.width; x++) {
            // get surface normal
            var nx = normals[ni];
            var ny = normals[ni+1];
            var nz = normals[ni+2];

            // make it a bit faster by only updateing the direction
            // for every other pixel
            //if(shiny > 0 || (ni&1) == 0){
                // calculate the light direction vector
            dx = lx - x;
            dy = ly - y;
            var magInv = directionlut[Math.floor((dx > 0 ? dx : -dx) + (dy > 0 ? dy : -dy)*canvas.width)];
            dx *= magInv;
            dy *= magInv;
            dz = lz*magInv;
            /*
                //var lui = ((dx+256)>>1+(dy+256)<<7)*3
                dz = lz;
                // normalize it
                magInv = 1.0/Math.sqrt(dx*dx + dy*dy + lz2);
                dx *= magInv;
                dy *= magInv;
                dz *= magInv;
            */
            //}

            // take the dot product of the direction and the normal
            // to get the amount of specularity
            var dot = dx*nx + dy*ny + dz*nz;
            var spec = Math.pow(dot, 20)*specularity;
            spec += Math.pow(dot, 400)*shiny;
            // spec + ambient
            var intensity = spec + 0.5;

            for(var channel = 0; channel < 3; channel++) {
                data[i+channel] = Math.floor(Math.round(clamp(textureData[i+channel]*intensity, 0, 255)));
            }
            i += 4;
            ni += 3;
        }
    }
    ctx.putImageData(imgData, 0, 0);
}


function normalmap(canvasId, texture, normalmap, specularity, shiny) /*: String * String * String * Double * Int -> Void */ {

    var canvas = /*:downcast HTMLCanvasElement + Void*/(document.getElementById(canvasId));
    if(canvas.getContext == undefined) {
        document.write('unsupported browser');
        return;
    }

    var ctx = canvas.getContext('2d');

    var normalData = null;
    var textureData = null;

    function getDataFromImage(img) /*: Undefined + HTMLImageElement -> ImageData */ {
        canvas.width = img.width;
        canvas.height = img.height;
        ctx.clearRect(0, 0, img.width, img.height);
        if (typeof img !== "undefined")
          ctx.drawImage(img, 0 ,0);
        return ctx.getImageData(0, 0, img.width, img.height);
    }

    function loadImage(src, callback) /*: String * (Event -> Void) -> HTMLImageElement */ {
        var img = /*:downcast HTMLImageElement*/(document.createElement('img'));
        img.onload = callback;
        img.src = src;
        return img;
    }

    var normals = /*:Double*/ [];
    var textureData = /*:upcast Undefined + Array<Int>*/undefined;
    var directionlut = /*:upcast Undefined + Array<Double>*/undefined;

    // assume lz is always 100
    var lz = 100;

    function main() /*: -> Void */ {
        var rect = canvas.getBoundingClientRect();
        canvas.onmousemove = function(e) /*: Event -> Void */ {
          if ((typeof canvas !== "undefined")) { if
              (typeof textureData !== "undefined") { if
              (typeof directionlut !== "undefined") {
            drawLight(canvas, ctx, normals, textureData, directionlut,
              shiny, specularity, e.clientX+50, e.clientY+50, lz);
          }}}
        };
    }

  var normalsImg = /*:upcast Undefined + HTMLImageElement*/undefined;
  loadImage(normalmap, function(_) /*: Event -> Void */ {
        var data = getDataFromImage(normalsImg).data;
        // precalculate the normals
        for(var i = 0; i < canvas.height*canvas.width*4; i+=4) {
            var nx = data[i]+0.0;
            // flip the y value
            var ny = 255.0-data[i+1];
            var nz = data[i+2]+0.0;

            // normalize
            var magInv = 1.0/Math.sqrt(nx*nx + ny*ny + nz*nz);
            nx *= magInv;
            ny *= magInv;
            nz *= magInv;

            normals[normals.length] = nx;
            normals[normals.length] = ny;
            normals[normals.length] = nz;
        }
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        // and use that knowledge to build a lookup table
        // for normalizing the direction vectors
        directionlut = /*:Double*/[];
        var i = 0;
        for(var y = 0; y < canvas.height; y++){
            for(var x = 0; x < canvas.width; x++){
                var magInv = 1.0/Math.sqrt(x*x + y*y + lz*lz);
                directionlut[i++] = magInv;
            }
        }
         var textureImg = /*:upcast Undefined + HTMLImageElement*/undefined;
         textureImg = loadImage(texture, function(_) /*: Event -> Void */ {
           textureData = getDataFromImage(textureImg).data;
           main();
         });
       console.log(directionlut.length.toString());
    });
}

normalmap('normalMapCanvas', 'face.jpg', 'normals.jpg', 1.2, 0);