// *** TYPES FOR gadgets/Metronome_cc/main.js *** 
/*::
  function on_viewOpen : ( -> Void)
  function onAddCustomMenuItems : (Dom -> Void)
  function onMoreGadgetsClick : (String -> Void)
  function onStart : ( -> Void)
  function onStop : ( -> Void)
  function incr : ( -> Void)
  function decr : ( -> Void)
  function onPlay : ( -> Void)
  function onAudioStateChange : (Dom * Int -> Void)
  function startedAudio : ( -> Void)
  function stoppedAudio : ( -> Void)
  function check_key : ( -> Void)
*/

var curAudioClip_ = null;
var timer = null;
var flag = 0;
var bpm;
var AUDIO_CLIP_URI = "tick.wav";

function on_viewOpen() {
  options.putDefaultValue("bpm",100);
  bpm = options.getValue("bpm");
  bpm_display.innerText = bpm;
	//pluginHelper.onAddCustomMenuItems = onAddCustomMenuItems;
}

function onAddCustomMenuItems(menu) {
  menu.AddItem("More Gadgets", 0, onMoreGadgetsClick);
}

function onMoreGadgetsClick()  {
	framework.openURL("http://www.gdgadgets.com");
}

function onStart() {
  if(flag == 0) {
    onStop();
    var time = parseInt((60/bpm)*1000);
    timer = setInterval(onPlay,time);
    btn.image = "stop.png";
    btn.overImage = "stop_over.png";
    btn.downImage = "stop_over.png";
    flag = 1;
  }
  else {
    onStop();
    btn.image = "play.png";
    btn.overImage = "play_over.png";
    btn.downImage = "play_over.png";
    flag = 0;
  }
}

function onStop() {
  if(timer) {
    clearInterval(timer);
    timer = null;
  }
}

function incr() {
  if(280 > bpm) {
    bpm++;
    options.putValue("bpm",bpm);
    bpm_display.innerText = bpm;
    if(flag == 1) {
      flag = 0;
      onStart();
    }
  }
}

function decr() {
  if(60 < bpm) {
    bpm-=1;
    options.putValue("bpm",bpm);
    bpm_display.innerText = bpm;
    if(flag == 1) {
      flag = 0;
      onStart();
    }
  }
}

function onPlay() {
  if (curAudioClip_ == null) {      // Not playing anything
    curAudioClip_ = framework.audio.play(AUDIO_CLIP_URI, onAudioStateChange); 
    startedAudio();
  } else {                  // Already playing something
    curAudioClip_.stop();
    curAudioClip_ = null;
    stoppedAudio();
  }
}

function onAudioStateChange(audioClip, state) {
  if (state == gddSoundStateStopped) {
    stoppedAudio();
    curAudioClip_ = null;
  } else if (state == gddSoundStatePlaying) {
    startedAudio();
  }
}

function startedAudio() {
  status_image.src = "green.png";
}

function stoppedAudio() {
  status_image.src = "red.png";
}

function check_key() {
  if(event.keycode == 45)
    decr();
  if(event.keycode == 43)
    incr();
}