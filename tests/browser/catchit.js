
var ctx;

// our ball object holder
var balls = new Array();

// mouse position cache
var mouse = {x:-100, y:-100};

// our prey we want to hunt
var prey  = {x:Math.random()*310, y:Math.random()*310};

// cache 2*PI for arc()
var circle = Math.PI * 2;

var tries = 1;
var score = 0;
var max_score = 0;

var inc_score = 15;

function $(id) {
	return document.getElementById(id);
}

function updateStat() {

	$('tries').innerHTML = tries;
	$('score').innerHTML = score;

	$('max_score').innerHTML = max_score;
}

function Ball(x, y, xsee, ysee) {

	this.x = x;
	this.y = y;
	this.xsee = xsee;
	this.ysee = ysee;

	this.move = function() {

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
	}
}

function createBall() {

	do {
		x = Math.random() * 315;
		y = Math.random() * 315;
	} while(prey.x <= x + 35 && x <= prey.x + 55 && prey.y <= y + 35 && y <= prey.y + 55);

	balls.push(new Ball(x, y, Math.random() * 5.5 - 2.75, Math.random() * 5.5 - 2.75));
}

function init() {
	ctx = $('canvas').getContext('2d');
	clock();
}

function clock() {

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
			balls = new Array();

			if(max_score < score) {
				max_score = score;
			}

			score = 0;
			tries++;

			updateStat();
		}
	}

	if(inc_score > 5.5) inc_score-= 0.2;

	window.setTimeout('clock()', 20);
}

document.onclick = function() {

	for(var i=0; i < balls.length; i++) {
		balls[i].xsee =-balls[i].xsee;
		balls[i].ysee =-balls[i].ysee;
	}
}

document.onmousemove = function(e) {

	mouse.x = e.pageX;
	mouse.y = e.pageY;

	/////////////

	if(prey.x <= mouse.x + 10 && mouse.x <= prey.x + 30 && prey.y <= mouse.y + 10 && mouse.y <= prey.y + 30) {
		prey = {x:Math.random()*300, y:Math.random()*300}
		createBall();
		score+= Math.floor(inc_score);
		inc_score = 15;
		updateStat();
	}
}