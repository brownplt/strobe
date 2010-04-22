var h = 0;        
var m = 0;
var s = 0;
var tmr;
var pause = 0;
var done = 0;
var prevSelect = 0;
var confirmed = 0;
var controlAltPressed = 0;

function startCount(){
	hours.enabled = false;
	minutes.enabled = false;
	seconds.enabled = false;

	if(done == 0){
		
		view.beginAnimation(startOff, 255, 0, 500);
		view.beginAnimation(stopOn, 0, 255, 500);
		if(clear.opacity == 255)
		{
			view.beginAnimation(clearOff, 255, 0, 500);	
		}
		view.beginAnimation(moveStop,196, 99, 750);
		done = 1;
	}
	stop.enabled = true; 
	modeSwitch.enabled = false;
	clear.enabled = false;
	start.enabled = false;
	if(upH.opacity == 255){
		view.beginAnimation(fadeID, 255, 0, 450);
		upH.enabled = false;
		downH.enabled = false;
		upM.enabled = false;
		downM.enabled = false;
		upS.enabled = false;
		downS.enabled = false;
	}
	if(modeSwitch.image == "stopwatchTAB.png"){
		s++;
		if(s == 60){
			s = 0;
			m++;
			if(m == 60){
			   m = 0;
			   h++;
			}	
		}
		hours.innerText = correctOutput(h);
		seconds.innerText = correctOutput(s);
		minutes.innerText = correctOutput(m);
		blink();
		stop.focus();
		t = setTimeout("startCount()", 1000);
	}
	else{
		if(s != 0 || h != 0 || m != 0){
			startTimer();
			hours.innerText = correctOutput(h);
			seconds.innerText = correctOutput(s);
			minutes.innerText = correctOutput(m);
			blink();
			stop.focus();
			t = setTimeout("startCount()", 1000);
		}
		else{			
			stopCount();
			alarm();
		}
	}
	stop.focus();	
}
function blink(){	
	if(blinky.visible == true){
		blinky.visible = false;
		blinky2.visible = false;
	}
	if(pause == 2){
		blinky.visible = true;
		blinky2.visible = true;
		pause = 0;
	}
	pause++;
}
function correctOutput(h){
	if(h < 10 && h > 0){
		h = "0" + h;
	}
	else if(h == 0){
		h = "00"
	}
	return h;
}
function clearCount(){
	h = 0;
	s = 0;
	m = 0;
  	hours.innerText = correctOutput(h);
	seconds.innerText = correctOutput(s);
	minutes.innerText = correctOutput(m);
	
	clear.enabled = false;
	
	view.beginAnimation(moveGreenBack,196, 99, 750);
	view.beginAnimation(clearOff, 255, 0, 500);

	if(modeSwitch.image == "timerTAB.png"){
		start.enabled = false;
		view.beginAnimation(startOff, 255, 0, 500);
		view.resizeTo(284, 80); 
		
	}
	modeSwitch.focus();
}
function stopCount(){	
	hours.enabled = true;
	minutes.enabled = true;
	seconds.enabled = true;
	start.enabled = true;
	if(modeSwitch.image == "stopwatchTAB.png" || s != 0 || h != 0 || m != 0){
		clear.enabled = true;
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(stopOn, 255, 0, 500); // reusing function 
		view.beginAnimation(startOff, 0, 255, 500); // reusing function	
	}
	stop.enabled = false;
	blinky.visible = true;
	blinky2.visible = true;
	clearInterval(t);
	
	
	if(modeSwitch.image == "timerTAB.png"){
		if(upH.opacity != 255)
		{		
			view.beginAnimation(fadeID, 0, 255, 450);	
			upH.enabled = true;
			downH.enabled = true;
			upM.enabled = true;
			downM.enabled = true;
			upS.enabled = true;
			downS.enabled = true;
		}
		if(h == 0 && m == 0 && s == 0){
			view.beginAnimation(stopOn, 255, 0, 500); // reusing function 
			
			start.x = 99;
			start.enabled = false;	
		}
	}
	done = 0;
	modeSwitch.enabled = true;
	modeSwitch.focus();
	
}
function stopOn(){stop.opacity = event.value;}
function startOff(){start.opacity = event.value;}
function moveStop(){stop.x = event.value;}
function moveGreenBack(){start.x = event.value;}
function clearOff(){clear.opacity = event.value;}	
//----------------------- stopwatch ends here ------------------------------

//---------------------------- Timer Starts Here --------------------------
function fasterEdit(whichEdit){
	switch(whichEdit){
		case 0: // stop increasing or decreasing when left mouse button released 
			clearInterval(tmr);
			break;
		case 1:
			increaseSec();
			tmr = setTimeout("fasterEdit(1)", 200);
			break;
		case 2:
			decreaseSec();
			tmr = setTimeout("fasterEdit(2)", 200);
			break;
		case 3:
			increaseMin();
			tmr = setTimeout("fasterEdit(3)", 200);
			break;
		case 4:
			decreaseMin();
			tmr = setTimeout("fasterEdit(4)", 200);
			break;
		case 5:
			increaseHr();
			tmr = setTimeout("fasterEdit(5)", 200);
			break;
		case 6:
			decreaseHr();
			tmr = setTimeout("fasterEdit(6)", 200);
			break;
	}
}
function increaseSec(){
	s++;
	if(s == 60){
		s = 0;
	}
	seconds.innerText = correctOutput(s);
	if(s > 0 && start.opacity == 0){
		start.enabled = true;
		clear.enabled = true;
		view.resizeTo(284,170);
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(startOff, 0, 255, 500);
	}
	else if(s == 0 && m == 0 && h == 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500); // reusing function
			start.enabled = false;
			view.beginAnimation(clearOff, 255, 0, 500);
			clear.enabled = false;
			view.resizeTo(284,80);
	}
	modeSwitch.focus();
}
function decreaseSec(){
	s--;
	if(s == -1){	
		s = 59;
	}
	seconds.innerText = correctOutput(s);
	if(s > 0 && start.opacity == 0){
		start.enabled = true;
		clear.enabled = true;
		view.resizeTo(284,170);
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(startOff, 0, 255, 500); // reusing function
	}
	else if(s == 0 && m == 0 && h == 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500); // reusing function
			start.enabled = false;
			view.beginAnimation(clearOff, 255, 0, 500);
			clear.enabled = false;
			view.resizeTo(284,80);
	}
	modeSwitch.focus();
}
function increaseMin(){
	m++;
	if(m == 60){	
		m = 0;
	}
	minutes.innerText = correctOutput(m);
	if(m > 0 && start.opacity == 0){
		start.enabled = true;
		clear.enabled = true;
		view.resizeTo(284,170);
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(startOff, 0, 255, 500); // reusing function;
	}
	else if(m == 0 && s == 0 && h == 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500); // reusing function
			start.enabled = false;
			view.beginAnimation(clearOff, 255, 0, 500);
			clear.enabled = false;
			view.resizeTo(284,80);
	}
	modeSwitch.focus();
}
function decreaseMin(){
	m--;
	if(m == -1){
		m = 59
	}
	minutes.innerText = correctOutput(m);
	if(m > 0 && start.opacity == 0){
		start.enabled = true;
		clear.enabled = true;
		view.resizeTo(284,170);
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(startOff, 0, 255, 500); // reusing function
	}
	else if(m == 0 && s == 0 && h == 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500); // reusing function
			start.enabled = false;
			view.beginAnimation(clearOff, 255, 0, 500);
			clear.enabled = false;
			view.resizeTo(284,80);
	}
	modeSwitch.focus();
}
function increaseHr(){
	h++;
	if(h == 100){
		h = 0;
	}
	hours.innerText = correctOutput(h);
	if(h > 0 && start.opacity == 0){
		start.enabled = true;
		clear.enabled = true;
		view.resizeTo(284,170);
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(startOff, 0, 255, 500);
	}
	else if(h == 0 && s == 0 && m == 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500);
			start.enabled = false;
			view.beginAnimation(clearOff, 255, 0, 500);
			clear.enabled = false;
			view.resizeTo(284,80);
	}
	modeSwitch.focus();
}
function decreaseHr(){
	h--;
	if(h == -1){
		h = 99;
	}
	hours.innerText = correctOutput(h);
	if(h > 0 && start.opacity == 0){
		start.enabled = true;
		clear.enabled = true;
		view.resizeTo(284,170);
		view.beginAnimation(clearOff, 0, 255, 500);
		view.beginAnimation(moveGreenBack,99, 196, 750);
		view.beginAnimation(startOff, 0, 255, 500); 
	}
	else if(h == 0 && s == 0 && m == 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500); 
			start.enabled = false;
			view.beginAnimation(clearOff, 255, 0, 500);
			clear.enabled = false;
			view.resizeTo(284,80);
	}
	modeSwitch.focus();
}
function startTimer(){
	s--;

	if(s == -1 && m > 0){	
		s = 59;
		m--;
	}
	else if(s == -1 && m == 0 && h > 0){
			h--;
			m = 59;
			s = 59;
	}
}
function hoverAppear(){
	hover.opacity = event.value;
}
function rotateHover(){
	hover.rotation = event.value;
}
function coolHover(){
	if(hover.opacity == 0)
	{
		view.beginAnimation(hoverAppear, 0, 255, 400); 
	}
	view.beginAnimation(rotateHover, 0, 359, 600);
	
	r = setTimeout("coolHover()", 601);
}
function stopCoolHover(){
	clearInterval(r);
	hover.rotation = 0;
	hover.opacity = 0;
}
function rotateModeSwitch(){
	modeSwitch.rotation = event.value;
}
function fadeID(){
	upH.opacity = event.value;
	downH.opacity = event.value;
	upM.opacity = event.value;
	downM.opacity = event.value;
	upS.opacity = event.value;
	downS.opacity = event.value;
}
function changeMode(){
	stopCoolHover();
	modeSwitch.enabled = false;
	view.beginAnimation(rotateModeSwitch, 0, -180, 300);
	if(modeSwitch.image == "stopwatchTAB.png"){
		modeSwitch.image = "timerTAB.png";
	}
	else{
		modeSwitch.image = "stopwatchTAB.png";
		if(start.x == 196){
			view.beginAnimation(moveGreenBack,196, 99, 750);
		}	
	}
	view.beginAnimation(rotateModeSwitch, -180, 0, 300);
	if(modeSwitch.image == "stopwatchTAB.png"){
		view.beginAnimation(fadeID, 255, 0, 450);
		if(start.opacity == 0){
			view.resizeTo(284,170);
			view.beginAnimation(startOff, 0, 255, 500); // reusing function
			start.enabled = true;
		}
	}
	else{
		view.beginAnimation(fadeID, 0, 255, 450);
		upH.enabled = true;
		downH.enabled = true;
		upM.enabled = true;
		downM.enabled = true;
		upS.enabled = true;
		downS.enabled = true;
		if(h == 0 && m == 0 && s== 0 && start.opacity == 255){
			view.beginAnimation(startOff, 255, 0, 500);
			view.resizeTo(284,80);
			start.enabled = false;
		}
	}
	modeSwitch.enabled = true;
	modeSwitch.focus();
	stopCoolHover();	
}
function alarm(){	
	alarming = audio.open("alarm.mp3");
	view.beginAnimation(rotateAlarmButton, 90, 0, 500);	
	stopAlarmSound.enabled = true;
	alarming.onStateChange = mediaStateChange;
	alarming.play();
	stopAlarmSound.focus();	
}
function mediaStateChange(media, state){
	if(state == gddSoundStateStopped){
	      m = setTimeout(function() { alarming.play(); }, 400);
  	}
}
function rotateAlarmButton(){
	stopAlarmSound.rotation = event.value;
	if(event.value == 90){
		view.resizeTo(284,80);
	}
}
function stopAlarm(){
	audio.stop(alarming);
	clearInterval(m);
	view.beginAnimation(rotateAlarmButton, 0, 90, 500);
	// flushing the stuff begins below
	h = 0;
	m = 0;
	s = 0;
	hours.innerText = correctOutput(h);
	seconds.innerText = correctOutput(s);
	minutes.innerText = correctOutput(m);
	stopAlarmSound.enabled = false;
	modeSwitch.focus();
}
function triSelect(whichEdit){
	var mailMan; // variable named mailman after its only purpose - to take value from whichEdit and pass it to prevSelect

	switch(whichEdit){
		case 1:
			if(upS.opacity == 255){
				upS.visible = false;
				downS.visible = false;
			}
			triSelectSec1.visible = true;
			triSelectSec2.visible = true;
			mailMan = 1;
			break;
		case 2:
			if(upM.opacity == 255){
				upM.visible = false;
				downM.visible = false;
			}
			triSelectMin1.visible = true;
			triSelectMin2.visible = true;
			mailMan = 2;
			break;
		case 3:
			if(upH.opacity == 255){
				upH.visible = false;
				downH.visible = false;
			}
			triSelectHrs1.visible = true;
			triSelectHrs2.visible = true;
			mailMan = 3;
			break;
	}
	triDeselect();
	prevSelect = mailMan;
}
function triDeselect(){
	// turn off previous selection
	switch(prevSelect){
		case 1:
			triSelectSec1.visible = false;
			triSelectSec2.visible = false;
			upS.visible = true;
			downS.visible = true;
			break;
		case 2:
			triSelectMin1.visible = false;
			triSelectMin2.visible = false;
			upM.visible = true;
			downM.visible = true;
			break;
		case 3:
			triSelectHrs1.visible = false;
			triSelectHrs2.visible = false;
			upH.visible = true;
			downH.visible = true;
			break;
	}
	if(confirmed == 0 && prevSelect != 0)
	{
		editCancel(prevSelect);
	}
}	
//keyboard functionality(setting the timer currently the only use)
function editOn(whichEdit){
	switch(whichEdit){
		case 1: 
			secEdit.visible = true;
			secEdit.enabled = true;
			seconds.enabled = false;
			seconds.visible = false;
			secEdit.focus();
			break;
		case 2:
			minEdit.visible = true;
			minEdit.enabled = true;
			minutes.enabled = false;
			minutes.visible = false;
			minEdit.focus();
			break;
		case 3: 
			hrsEdit.visible = true;
			hrsEdit.enabled = true;
			hours.enabled = false;
			hours.visible = false;
			hrsEdit.focus();
			break;
	}
	triSelect(whichEdit);
}

function clearInitialTime(whichEdit){
	switch(whichEdit){
		case 1:
			secEdit.value = "";
			break;
		case 2:
			minEdit.value = "";
			break;
		case 3:
			hrsEdit.value = "";
			break;
	}
}
function limitEdit(whichEdit){
	switch(whichEdit){
		case 1:
			if((secEdit.value.length == 2 || (event.keyCode < 48 || event.keyCode > 57)) && (event.keyCode != 8)){
				event.returnValue = false
			}
			break;
		case 2:
			if((minEdit.value.length == 2 || (event.keyCode < 48 || event.keyCode > 57))&& (event.keyCode != 8)){
				event.returnValue = false
			}
			break;
		case 3: 
			if((hrsEdit.value.length == 2 || (event.keyCode < 48 || event.keyCode > 57))&& (event.keyCode != 8)){
				event.returnValue = false
			}
			break;
	}
}
function exitAlternate(whichEdit){
	limitEdit(whichEdit);	
	if(event.keyCode == 13){
		editOff(whichEdit);
	}
	if(event.keyCode == 81 || event.keyCode == 31 || event.keyCode == 27){
		triDeselect();
		editCancel(whichEdit);
		modeSwitch.focus();
	}
	switch(whichEdit){
		case 1: 
			if(event.keyCode == 77 || event.keyCode == 37){
				triDeselect();
				editCancel(whichEdit);
				editOn(2);
			}
			if(event.keyCode == 72 || event.keyCode == 39){
				triDeselect();
				editCancel(whichEdit);
				editOn(3);
			}
			if(event.keyCode == 83){
				triDeselect();
				editCancel(whichEdit);
				modeSwitch.focus();
			}
			break;
		case 2:
			if(event.keyCode == 72 || event.keyCode == 37){
				triDeselect();
				editCancel(whichEdit);
				editOn(3);
			}
			if(event.keyCode == 83 || event.keyCode == 39){
				triDeselect();
				editCancel(whichEdit);
				editOn(1);
			}
			if(event.keyCode == 77){
				triDeselect();
				editCancel(whichEdit);
				modeSwitch.focus();
			}
			break;
		case 3:
			if(event.keyCode == 83 || event.keyCode == 37){
				triDeselect();
				editCancel(whichEdit);
				editOn(1);
			}
			if(event.keyCode == 77 || event.keyCode == 39){
				triDeselect();
				editCancel(whichEdit);
				editOn(2);
			}
			if(event.keyCode == 72){
				triDeselect();
				editCancel(whichEdit);
				modeSwitch.focus();
			}
			break;
	}
}
function editCancel(whichEdit){
	switch(whichEdit){
		case 1:
			secEdit.visible = false;
			secEdit.enabled = false;
			seconds.enabled = true;
			seconds.visible = true;
			break;
		case 2:
			minEdit.visible = false;			
			minEdit.enabled = false;
			minutes.enabled = true;
			minutes.visible = true;	
			break;
		case 3: 
			hours.enabled = true;
			hours.visible = true;
			hrsEdit.visible = false;
			hrsEdit.enabled = false;
			break;
	}
	prevSelect = 0;
}
function editOff(whichEdit){
	switch(whichEdit){
		case 1: 
			if(secEdit.value != "")
			{
				s = secEdit.value;
			}
			secEdit.visible = false;
			secEdit.enabled = false;
			seconds.enabled = true;
			seconds.visible = true;	
			if(s >= 60){
				s = s - 60;
				m++;
				seconds.innerText = correctOutput(s);
				minutes.innerText = correctOutput(m);
			}
			else{
				seconds.innerText = correctOutput(s);
			}
			if(s > 0 && clear.opacity == 0){
				start.enabled = true;
				clear.enabled = true;
				view.resizeTo(284,170);
				view.beginAnimation(clearOff, 0, 255, 500);
				clear.enabled = true;
				view.beginAnimation(moveGreenBack,99, 196, 750);
				if(start.opacity == 0){
					view.beginAnimation(startOff, 0, 255, 500);
					start.enabled = true;
				}
			}
			else if(s == 0 && m == 0 & h == 0){
					if(clear.opacity == 255){					
						view.beginAnimation(clearOff, 255, 0, 500);
						clear.enabled = false;
					}
					if(modeSwitch.image == "timerTAB.png"){
						view.beginAnimation(startOff, 255, 0, 500); 
						start.enabled = false;
						view.resizeTo(284,80);
					}
			}
			minEdit.opacity = 0;
			triSelectMin1.opacity = 0;
			triSelectMin2.opacity = 0;
			editOn(2);
			triDeselect();
			editCancel(2);	
			minEdit.opacity = 255;
			triSelectMin1.opacity = 255;
			triSelectMin2.opacity = 255;	
			break;
		case 2:
			if(minEdit.value != ""){
				m = minEdit.value;
			}			
			minEdit.visible = false;			
			minEdit.enabled = false;
			minutes.enabled = true;
			minutes.visible = true;	
			if(m >= 60){
				m = m - 60;
				h++;
				minutes.innerText = correctOutput(m);
				hours.innerText = correctOutput(h);
			}
			else{
				minutes.innerText = correctOutput(m);
			}
			if(m > 0 && clear.opacity == 0){
				start.enabled = true;
				clear.enabled = true;
				view.resizeTo(284,170);
				view.beginAnimation(clearOff, 0, 255, 500);
				clear.enabled = true;
				view.beginAnimation(moveGreenBack,99, 196, 750);
				if(start.opacity == 0){
					view.beginAnimation(startOff, 0, 255, 500);
					start.enabled = true;
				}
			}
			else if(s == 0 && m == 0 & h == 0){
					if(clear.opacity == 255){					
						view.beginAnimation(clearOff, 255, 0, 500);
						clear.enabled = false;
					}
					if(modeSwitch.image == "timerTAB.png"){
						view.beginAnimation(startOff, 255, 0, 500); 
						start.enabled = false;
						view.resizeTo(284,80);
					}
			}
			hrsEdit.opacity = 0;
			triSelectHrs1.opacity = 0;
			triSelectHrs2.opacity = 0;
			editOn(3);
			triDeselect();
			editCancel(3);	
			hrsEdit.opacity = 255;
			triSelectHrs1.opacity = 255;
			triSelectHrs2.opacity = 255;
			break;
		case 3: 
			if(hrsEdit.value != ""){
				h = hrsEdit.value;
			}
			hours.enabled = true;
			hours.visible = true;
			hrsEdit.visible = false;
			hrsEdit.enabled = false;	
			hours.innerText = correctOutput(h);
			if(h > 0 && clear.opacity == 0){
				start.enabled = true;
				clear.enabled = true;
				view.resizeTo(284,170);
				view.beginAnimation(clearOff, 0, 255, 500);
				clear.enabled = true;
				view.beginAnimation(moveGreenBack,99, 196, 750);
				if(start.opacity == 0){
					view.beginAnimation(startOff, 0, 255, 500);
					start.enabled = true;
				} 
			}
			else if(s == 0 && m == 0 & h == 0){
					if(clear.opacity == 255){					
						view.beginAnimation(clearOff, 255, 0, 500);
						clear.enabled = false;
					}
					if(modeSwitch.image == "timerTAB.png"){
						view.beginAnimation(startOff, 255, 0, 500); 
						start.enabled = false;
						view.resizeTo(284,80);
					}
			}
			minEdit.opacity = 0;
			triSelectMin1.opacity = 0;
			triSelectMin2.opacity = 0;
			editOn(2);
			triDeselect();
			editCancel(2);	
			minEdit.opacity = 255;
			triSelectMin1.opacity = 255;
			triSelectMin2.opacity = 255;
			break;
	}
	confirmed = 1;
	triDeselect();
	confirmed = 0;
	modeSwitch.focus();

}
function clearSpecific(whichOne){
	if(clear.opacity == 255){
		switch(whichOne){
			case 1:
				s = 0;
				seconds.innerText = "00";
				if(s == 0 && m == 0 & h == 0){
					view.beginAnimation(clearOff, 255, 0, 500);
					clear.enabled = false;
					if(modeSwitch.image == "timerTAB.png"){
						view.beginAnimation(startOff, 255, 0, 500); 
						start.enabled = false;
						view.resizeTo(284,80);
					}
				}
				break;
			case 2:
				m = 0;
				minutes.innerText = "00";
				if(s == 0 && m == 0 & h == 0){
					view.beginAnimation(clearOff, 255, 0, 500);
					clear.enabled = false;
					if(modeSwitch.image == "timerTAB.png"){
						view.beginAnimation(startOff, 255, 0, 500); 
						start.enabled = false;
						view.resizeTo(284,80);
					}
				}
				break;
			case 3:
				h = 0;
				hours.innerText = "00";
				if(s == 0 && m == 0 & h == 0){
					view.beginAnimation(clearOff, 255, 0, 500);
					clear.enabled = false;
					if(modeSwitch.image == "timerTAB.png"){
						view.beginAnimation(startOff, 255, 0, 500); 
						start.enabled = false;
						view.resizeTo(284,80);
					}
				}
				break;
		}
	}
	modeSwitch.focus();
}
function kSupport(){
	modeSwitch.focus();
}
function kBoard(){
	controlAltPressed = (event.keyCode == 17) ? 1 : 0;
	/*start.enabled = true;
	start.caption = event.keyCode;*///debug purpose
	switch(event.keyCode){
		case 36:
			stopWatchTimer.visible = false;
			mainmenu.visible = true;
			view.resizeTo(mainmenu.width, mainmenu.height);
			break;
		case 32:
			if(start.opacity == 255){
				startCount();
			}
			if(stop.opacity == 255){
				stopCount();
				modeSwitch.focus();
			}
			else if(stopAlarmSound.rotation == 0){
				stopAlarm();
			}
			break;
		case 72:
			if(controlAltPressed == 0 && stop.opacity == 0){
				editOn(3);
			}
			else{
				controlAltPressed = 0;
				clearSpecific(3);				
			}					 
			break;
		case 104:
			if(controlAltPressed == 0 && stop.opacity == 0){
				editOn(3);
			}
			else{
				controlAltPressed = 0;
				clearSpecific(3);				
			}
			break;
		case 109:
			if(controlAltPressed == 0 && stop.opacity == 0){
				editOn(2);
			}
			else{
				controlAltPressed = 0;
				clearSpecific(2);				
			}
			break;
		case 77: 
			if(controlAltPressed == 0 && stop.opacity == 0){
				editOn(2);
			}
			else{
				controlAltPressed = 0;
				clearSpecific(2);				
			}
			break;
		case 115:
			if(controlAltPressed == 0 && stop.opacity == 0){
				editOn(1);
			}
			else{
				controlAltPressed = 0;
				clearSpecific(1);				
			}
			break;
		case 83:
			if(controlAltPressed == 0 && stop.opacity == 0){
				editOn(1);
			}
			else{
				controlAltPressed = 0;
				clearSpecific(1);				
			}
			break;
		case 9:
			if(stop.enabled == false){			
				modeSwitch.killFocus();
				changeMode();
			}
			break;
	}
	if((event.keyCode == 127 || event.keyCode == 67 || event.keyCode == 99) && clear.enabled == true){
		clearCount();
	}
}
