var curAudioClip_ = /*:upcast Audioclip + Bool*/ false;
var timer = 0; // ARJUN
var flag = 0;
var bpm = 0;
var AUDIO_CLIP_URI = "tick.wav";

function on_viewOpen() /*: -> Void */ {
  options.putDefaultValue("bpm",100);
  // ARJUN: Int
  bpm = /*:downcast Int */(options.getValue("bpm"));
  bpm_display.innerText = bpm.toString();
	//pluginHelper.onAddCustomMenuItems = onAddCustomMenuItems;
}

function onAddCustomMenuItems(menu) /*: Menu -> Void */ {
  menu.AddItem("More Gadgets", 0, onMoreGadgetsClick);
}

function onMoreGadgetsClick(_ /* Arjun: ignored arg */) /*: { } -> Void */ {
	framework.openURL("http://www.gdgadgets.com");
}

function onStart() /*: -> Void */ {
  if(flag == 0) {
    onStop();
    var time = parseInt((60/bpm)*1000.0, undefined);
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

function onStop() /*: -> Void */ {
  if(timer != 0) {
    clearInterval(timer);
    timer = 0;
  }
}

function incr() /*: -> Void */ {
  if(280 > bpm) {
    bpm++;
    options.putValue("bpm",bpm);
    bpm_display.innerText = bpm.toString();
    if(flag == 1) {
      flag = 0;
      onStart();
    }
  }
}

function decr() /*: -> Void */ {
  if(60 < bpm) {
    bpm-=1;
    options.putValue("bpm",bpm);
    bpm_display.innerText = bpm.toString();
    if(flag == 1) {
      flag = 0;
      onStart();
    }
  }
}

function onPlay() /*: -> Void */ {
  if (typeof curAudioClip_ === "boolean") {      // Not playing anything
    curAudioClip_ = framework.audio.play(AUDIO_CLIP_URI, onAudioStateChange);
    startedAudio();
  } else {                  // Already playing something
    curAudioClip_.stop();
    curAudioClip_ = false; // Claudiu: changed from null
    stoppedAudio();
  }
}

function onAudioStateChange(audioClip, state) /*: Audioclip * Int -> Void */ {
  if (state == gddSoundStateStopped) {
    stoppedAudio();
    curAudioClip_ = false; // Claudiu: changed from null
  } else if (state == gddSoundStatePlaying) {
    startedAudio();
  }
}

function startedAudio() /*: -> Void */ {
  status_image.src = "green.png";
}

function stoppedAudio() /*: -> Void */ {
  status_image.src = "red.png";
}

function check_key() /*: -> Void */ {
  if(event.keycode == 45)
    decr();
  if(event.keycode == 43)
    incr();
}
