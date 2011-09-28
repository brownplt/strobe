//MODIFICATIONS:
//bugs:
//  4x add parameter
//  6x add Math.floor to something that should be int (implicit conv)
//bozo:
//  3x add initial value
//  1x downcast from getelbyid
//  1x upcast for init value
//  3x initial value from 0 to 0.0
//annots:
//  1x change from {} to ImageData


// Copyright by Krzysztof Pasek (www.guciek.net)
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 2 or 3.

function mouse() /*:  -> {x : Int, y : Int, pressed : Bool} */ {
	var ob = {
		x: -1,
		y: -1,
		pressed: false
	};
	function onmove(e) /*: MouseEvent -> Undef */ {
		ob.x = e.clientX;
		ob.y = e.clientY;
	}
	function ondown(_) /*: Event -> Undef */ {
		ob.pressed = true;
	}
	function onup(_) /*: Event -> Undef */ {
		ob.pressed = false;
	}
	document.addEventListener('mousemove', onmove, false);
	document.addEventListener('mousedown', ondown, false);
	document.addEventListener('mouseup', onup, false);
	return ob;
}
function draw() /*:  -> {draw : ( -> Undef), init : ( -> Undef)} */ {
	var w=0, h=0;
        var c = /*:upcast Undef + CanvasRenderingContext2D*/undefined;
	var reach = 50;
	var m = mouse();
	var lastmx = 0, lastmy = 0;
	function init() /*:  -> Undef */ {
		w = window.innerWidth;
		h = window.innerHeight;
		var elem = /*:downcast HTMLCanvasElement*/(document.getElementById("view"));
		elem.width = w;
		elem.height = h;
		c = elem.getContext("2d");
		c.lineCap = "round";
		c.strokeStyle = "rgb(255,255,255)";
		c.fillStyle = "rgb(255,255,255)";
		c.fillRect(0, 0, w, h);
	}
	function modify_region(i, centerx, centery) /*: ImageData * Int * Int -> Undef */ {
		if (m.pressed) {
			for (var y = i.height-1; y >= 0; y--) {
				for (var x = 0; x < i.width; x++) {
					var pos = 4*(i.width*y + x);
					var a = x-centerx;
					var b = y-centery;
					var v = 1 - (a*a + b*b)/(reach*reach);
					if (v < 0) { v = 0; }
					v = v*v;
					if (y >= 1) {
						i.data[pos+0] += Math.floor(v*(i.data[pos+0-i.width*4]-i.data[pos+0]));
						i.data[pos+1] += Math.floor(v*(i.data[pos+1-i.width*4]-i.data[pos+1]));
						i.data[pos+2] += Math.floor(v*(i.data[pos+2-i.width*4]-i.data[pos+2]));
					}
				}
			}
		} else {
			// This part is more optimized for fast execution.
			var rr = 1/(reach*reach);
			var pos = 0;
			var w = i.width;
			var t = 0.0;
			var v = 0.0;
			var a = 0;
			var vv = 0.0;
			var idata = i.data;
			for (var y = 0; y < i.height; y++) {
				var b = y-centery;
				var bb = b*b;
				for (var x = 0; x < w; x++) {
					a = x-centerx;
					v = 1 - (a*a + bb)*rr;
					if (v <= 0) {
						pos += 4;
						continue;
					}
					vv = v*v;
					v = 6*(vv - vv*v);
					t = idata[pos];
					t -= v*10;
					if (t < 0) t += 256;
					idata[pos] = Math.floor(t);
					pos++;
					t = idata[pos];
					t -= v*21.23553;
					if (t < 0) t += 256;
					idata[pos] = Math.floor(t);
					pos++;
					t = idata[pos];
					t -= v*46.72232;
					if (t < 0) t += 256;
					idata[pos] = Math.floor(t);
					pos += 2;
				}
			}
		}
	}
	function do_draw() /*:  -> Undef */ {
		if (m.x < 0) { return; }
		if ((lastmx == m.x) && (lastmy == m.y)) {
			if (reach < 100) { reach++; }
		} else {
			lastmx = m.x;
			lastmy = m.y;
			reach = 50;
		}
		var x1 = m.x-reach;
		var x2 = m.x+reach;
		var y1 = m.y-reach;
		var y2 = m.y+reach;
		if (x1 < 0) { x1 = 0; }
		if (y1 < 0) { y1 = 0; }
		if (x1 > w-1) { x1 = w-1; }
		if (y1 > h-1) { y1 = h-1; }
		if (x2 > w) { x2 = w; }
		if (y2 > h) { y2 = h; }
		if (x2 < x1+1) { x2 = x1+1; }
		if (y2 < y1+1) { y2 = y1+1; }
		var i = c.getImageData(x1, y1, x2-x1, y2-y1);
		modify_region(i, m.x-x1, m.y-y1);
		c.putImageData(i, x1, y1);
	}
	return {
		draw: do_draw,
		init: init
	};
}
function timer() /*:  -> {start : (Int -> Undef)} */ {
	var d = draw();
	function step(_) /*: Int -> Undef */ {
		try {
			d.draw();
			setTimeout(step, 8);
		} catch (err) {
//			alert("Error: "+err);
//			throw err;
		}
	}
	function start(_) /*: Int -> Undef */ {
		try {
			d.init();
			setTimeout(step, 1);
		} catch (err) {
//			alert("Error: "+err);
//			throw err;
		}
	}
	return {
		start: start
	};
}
setTimeout(timer().start, 100);
