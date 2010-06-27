//TODO: get do/while loops to work
//TODO: get array properties to work (.push)

//changed 1 Host annot to HTMLElement + Null, other 2 to Event

//1 settimeout 'clock()' to clock
//2 'var' added
//added 1 ignore variable to an event handler
//3 tostrings added
//
//1 function lifted
//1 var annot on ctx
//2 added empty array annot
//3 downcast
//1 refactor .push() into array setting
//change 15 to 15.0 for correct type

var ctx = /*:upcast Undef + CanvasRenderingContext2D*/undefined;

// mouse position cache
var mouse = {x:-100, y:-100};

// our prey we want to hunt
var prey  = {x:Math.random()*310, y:Math.random()*310};

// cache 2*PI for arc()
var circle = Math.PI * 2;

var tries = 1;
var score = 0;
var max_score = 0;

var inc_score = 15.0;


function Ball(x, y, xsee, ysee) /*: constructor (Num * Num * Num * Num -> {x : Num, y : Num, xsee : Num, ysee : Num, move : ([Ball]  -> Undef)})  */ {

	this.x = x;
	this.y = y;
	this.xsee = xsee;
	this.ysee = ysee;

	this.move = function() /*: [Ball]  -> Undef  */ {

		if(this.x > 315) {
			this.x = 315;
			this.xsee = -this.xsee;
		} else if(this.x < 5) {
			this.x = 5;
			this.xsee = -this.xsee;
		}

		if(this.y > 315) {
			this.y = 315;
			this.ysee = -this.ysee;
		} else if(this.y < 5) {
			this.y = 5;
			this.ysee = -this.ysee;
		}

		this.x+= this.xsee;
		this.y+= this.ysee;

		ctx.beginPath();
		ctx.arc(this.x, this.y, 5, 0, circle, true);
		ctx.closePath();
		ctx.fill();
	};
}
// our ball object holder
var balls = /*:Ball*/ [];


function $(id) /*: Str -> HTMLElement + Null */ {
	return document.getElementById(id);
}

function updateStat() /*:  -> Undef  */ {

	(/*:downcast HTMLLabel*/($('tries'))).innerHTML = tries.toStr();
	(/*:downcast HTMLLabel*/($('score'))).innerHTML = score.toStr();

	(/*:downcast HTMLLabel*/($('max_score'))).innerHTML = max_score.toStr();
}

function createBall() /*:  -> Undef  */ {
        var x = 0.0, y = 0.0; //type init
//	do {
		x = Math.random() * 315;
		y = Math.random() * 315;
//	} while(prey.x <= x + 35 && x <= prey.x + 55 && prey.y <= y + 35 && y <= prey.y + 55);


//	balls.push(new Ball(x, y, Math.random() * 5.5 - 2.75, Math.random() * 5.5 - 2.75));
	balls[balls.length] = (new Ball(x, y, Math.random() * 5.5 - 2.75, Math.random() * 5.5 - 2.75));

}

function init() /*:  -> Undef  */ {
	ctx = (/*:downcast HTMLCanvasElement*/($('canvas'))).getContext('2d');
	clock();
}

function clock() /*:  -> Undef  */ {

	// global clear is faster for many balls
	ctx.clearRect(0, 0, 320, 320);

	ctx.fillStyle = "#c00";
	ctx.fillRect(prey.x, prey.y, 20, 20);

	// we are still red
	ctx.beginPath();
	ctx.arc(mouse.x, mouse.y, 10, 0, circle, true);
	ctx.closePath();
	ctx.fill();

	ctx.fillStyle = "#333";
	for(var i = 0; i < balls.length; i++) {
		balls[i].move();

		if(
		balls[i].x <= mouse.x + 15 && mouse.x <= balls[i].x + 15 &&
		balls[i].y <= mouse.y + 15 && mouse.y <= balls[i].y + 15 &&
		((mouse.x-balls[i].x)*(mouse.x-balls[i].x) + (mouse.y-balls[i].y)*(mouse.y-balls[i].y)) <= 225) {
			balls = /*:Ball*/[];

			if(max_score < score) {
				max_score = score;
			}

			score = 0;
			tries++;

			updateStat();
		}
	}

	if(inc_score > 5.5) inc_score-= 0.2;

	window.setTimeout(clock, 20);
}

document.onclick = function(_) /*: Event -> Undef  */ {

	for(var i=0; i < balls.length; i++) {
		balls[i].xsee =-balls[i].xsee;
		balls[i].ysee =-balls[i].ysee;
	}
};

document.onmousemove = function(e) /*: Event -> Undef  */ {

	mouse.x = e.pageX;
	mouse.y = e.pageY;

	/////////////

	if(prey.x <= mouse.x + 10 && mouse.x <= prey.x + 30 && prey.y <= mouse.y + 10 && mouse.y <= prey.y + 30) {
		prey = {x:Math.random()*300, y:Math.random()*300};
		createBall();
		score+= Math.floor(inc_score);
		inc_score = 15;
		updateStat();
	}
};
