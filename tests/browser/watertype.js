//MODIFICATIONS:
//Bugs:
//  2x: added local var x,y
//  2x: added var
//  1x: added func arg
//  6x: moved var into function since was only used locally
//  1x: check for image not being defined ?
//  1x: math.floor to give int value to setinterval
//Bozo:
//  1x: moved init stuff to end cause of func lifting
//  2x: moved var down so didnt have to put in initial value
//  4x: upcast+init-to-undef-combo
//  4x: downcast from createelement
//  1x: really stupid undef check
//  15x: init vars so never undefined
//  7x: empty array annotations
//  2x: stupid .push()
//Annots:
//   added 3x, since couldn't test touching stuff


var QUALITY_X = 6,
    QUALITY_Y = 3,
    WIDTH = Math.floor(window.innerWidth / QUALITY_X),
    HEIGHT = Math.floor(window.innerHeight / QUALITY_Y),
    SIZE = WIDTH * HEIGHT,
    HEIGHT_HALF = Math.floor(HEIGHT / 2),
    TEXT_OFFSETY = Math.floor((HEIGHT - 64) / 2),

    context = /*:upcast Undef + CanvasRenderingContext2D*/undefined,
    image = /*:upcast Undef + ImageData*/undefined,
    data = [0], buffer1 = [0], buffer2 = [0],

    dataHeightMap = [0],
    contextText = /*:upcast Undef + CanvasRenderingContext2D*/undefined,
    dataText = [0],

    input = /*:upcast Undef + HTMLElement*/undefined, text="",

isUserInteracting = /*: upcast Bool */ false, pointers = [[0.0]];

function init() /*: -> Undef */ {

    var container = document.getElementById('container');

    // input box
    input = document.createElement("input");
    input.type = "text";
    input.value = "type";
    input.style.position = "absolute";
    input.style.top = "10px";
    input.style.left = "10px";
    input.style.opacity = "0";
    if (typeof input != "undefined") container.appendChild(input);
    input.focus();

    // Height Map (Water)
    var canvasHeightMap = /*:downcast HTMLCanvasElement*/(document.createElement("canvas"));
    canvasHeightMap.width = WIDTH;
    canvasHeightMap.height = HEIGHT;

    var contextHeightMap = canvasHeightMap.getContext("2d");
    var imageHeightMap = contextHeightMap.getImageData(0, 0, WIDTH, HEIGHT);
    dataHeightMap = imageHeightMap.data;

    buffer1 = /*:Int*/[];
    buffer2 = /*:Int*/[];

    for (var i = 0; i < SIZE; i++) {

        buffer1[i] = 128;
        buffer2[i] = 128;
    }

    // Text
    var canvasText = /*:downcast HTMLCanvasElement*/(document.createElement("canvas"));
    canvasText.width = WIDTH;
    canvasText.height = 128;

    contextText = canvasText.getContext("2d");
    contextText.font = "80px Helvetica";
    contextText.fillStyle = "rgb(255, 0, 0)";
    contextText.textAlign = "center";

    // Output (Parallax)
    var canvas = /*:downcast HTMLCanvasElement*/(document.createElement("canvas"));
    canvas.width = WIDTH;
    canvas.height = HEIGHT;
    canvas.style.width = window.innerWidth + "px";
    canvas.style.height = window.innerHeight + "px";
    container.appendChild(canvas);

    context = canvas.getContext("2d");
    context.fillStyle = "rgb(0, 0, 0)";
    context.fillRect(0, 0, WIDTH, HEIGHT);

    image = context.getImageData(0, 0, WIDTH, HEIGHT);
    data = image.data;

    document.addEventListener('mousedown', onDocumentMouseDown, false);
    document.addEventListener('mousemove', onDocumentMouseMove, false);
    document.addEventListener('mouseup', onDocumentMouseUp, false);
    document.addEventListener('mouseout', onDocumentMouseOut, false);
    document.addEventListener('touchstart', onDocumentTouchStart, false);
    document.addEventListener('touchmove', onDocumentTouchMove, false);
    document.addEventListener('touchend', onDocumentTouchEnd, false);

    document.addEventListener('keypress', onDocumentKeyPress, false);

}

// Event Handlers

function onDocumentMouseDown(event) /*: Event -> Undef */ {

    event.preventDefault();

    isUserInteracting = true;

    input.focus();

    pointers = [
        [event.clientX / QUALITY_X, event.clientY / QUALITY_Y]
    ];

}

function onDocumentMouseMove(event) /*: Event -> Undef */  {

    pointers = [
        [event.clientX / QUALITY_X, event.clientY / QUALITY_Y]
    ];

}

function onDocumentMouseUp(event) /*: Event -> Undef */  {

    isUserInteracting = false;

}

function onDocumentMouseOut(event) /*: Event -> Undef */  {

    isUserInteracting = false;

}

function onDocumentTouchStart(event) /*: Event -> Undef */  {

    isUserInteracting = true;

    event.preventDefault();

    pointers = /*:Array<Num>*/[];

    for (var i = 0; i < event.touches.length; i++) {

        //pointers.push([event.touches[i].pageX / QUALITY_X, event.touches[i].pageY / QUALITY_Y]);
        pointers[pointers.length] = ([event.touches[i].pageX / QUALITY_X, event.touches[i].pageY / QUALITY_Y]);

    }

}

function onDocumentTouchMove(event) /*: Event -> Undef */  {

    event.preventDefault();

    pointers = /*:Array<Num>*/[];

    for (var i = 0; i < event.touches.length; i++) {

        //pointers.push([event.touches[i].pageX / QUALITY_X, event.touches[i].pageY / QUALITY_Y]);
        pointers[pointers.length] = ([event.touches[i].pageX / QUALITY_X, event.touches[i].pageY / QUALITY_Y]);

    }

}

function onDocumentTouchEnd(event) /*: Event -> Undef */  {

    event.preventDefault();

    isUserInteracting = false;

}

function onDocumentKeyPress(event) /*: Event -> Undef */  {

    switch (event.keyCode) {

    case 13:
        input.value = "";
        break;
    }

}

//

function emit(x, y) /*: ((Int + Num) * (Int + Num) -> Undef) */ {

    buffer1[Math.floor(x) + (Math.floor(y + 40) * WIDTH)] = 256;

}

function writeText(string) /*: Str -> Undef */ {

    text = string;

    contextText.clearRect(0, 0, WIDTH, 128);
    contextText.fillText(string, WIDTH / 2, 63);

    var imageText = contextText.getImageData(0, 0, WIDTH, 128);
    dataText = imageText.data;

}

function processText() /*: -> Undef */ {
    var y = 0, x = 0;
    for (y = 0; y < 128; y++) {

        for (x = 0; x < WIDTH; x++) {

            if (dataText[(x + y * WIDTH) * 4] > 0) {

                emit(x, y + TEXT_OFFSETY);

            }

        }

    }

}

function loop(_) /*: Int -> Undef */ {

    var x=0, y=0, yz=0, pixel=0, index=0;

    if (isUserInteracting) {

        for (var i = 0; i < pointers.length; i++) {

            emit(pointers[i][0], pointers[i][1]);

        }

    }

    // Water
    var iMax = (WIDTH * HEIGHT) - WIDTH;

    for (var i = WIDTH; i < iMax; i++) {

        pixel = ((buffer1[i - 1] + buffer1[i + 1] + buffer1[i - WIDTH] + buffer1[i + WIDTH]) >> 1) - buffer2[i];
        buffer2[i] = pixel -= (pixel - 128) >> 4;

        dataHeightMap[i * 4] = pixel > 255 ? 255 : pixel < 0 ? 0 : pixel;

    }

    var tempbuffer = buffer1;
    buffer1 = buffer2;
    buffer2 = tempbuffer;

    // Text
    if (text != input.value) {

        writeText(input.value);

    }

    processText();

    // Parallax
    // Thx: http://pixelero.wordpress.com/2009/07/05/belousovzhabotinsky-with-perspective/
    var indices = /*:Int*/[];

    for (x = 0; x < WIDTH; x++) {

        var levels = /*:Int*/[];
        var pixels = /*:Int*/[];

        for (y = 0; y < HEIGHT; y++) {

            index = indices[y] = (x + y * WIDTH) * 4;
            pixels[y] = dataHeightMap[index];
            levels[y - (dataHeightMap[index] * HEIGHT >> 10)] = y;

        }

        yz = -1;
        for (y = 0; y < HEIGHT; y++) {

            pixels[y] = levels[y] > yz ? pixel = pixels[yz = levels[y]] : pixel;

            index = indices[y];
            data[index + 1] = pixel - 64 + (y >> 2);
            data[index + 2] = pixel - 32 + (y >> 2);

        }
    }

    if (typeof image != "undefined") context.putImageData(image, 0, 0);

}

init();
setInterval(loop, Math.floor(1000 / 60));

