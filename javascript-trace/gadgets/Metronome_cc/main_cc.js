var curAudioClip_ = null;
var timer = null;
var flag = 0;
var bpm;
var AUDIO_CLIP_URI = "tick.wav";
var on_viewOpen = __typedjs(function  ()
                            {
                              options.putDefaultValue("bpm",100);
                              bpm = options.getValue("bpm");
                              bpm_display.innerText = bpm;
                              //pluginHelper.onAddCustomMenuItems = onAddCustomMenuItems;
                            },
                            undefined,
                            "on_viewOpen",
                            "gadgets/Metronome_cc/main.js",
                            0);
var onAddCustomMenuItems = __typedjs(function  (menu)
                                     {
                                       menu.AddItem("More Gadgets",0,onMoreGadgetsClick);
                                     },
                                     undefined,
                                     "onAddCustomMenuItems",
                                     "gadgets/Metronome_cc/main.js",
                                     1);
var onMoreGadgetsClick = __typedjs(function  ()
                                   {
                                     framework.openURL("http://www.gdgadgets.com");
                                   },
                                   undefined,
                                   "onMoreGadgetsClick",
                                   "gadgets/Metronome_cc/main.js",
                                   2);
var onStart = __typedjs(function  ()
                        {
                          if (flag == 0)
                          {
                            onStop();
                            var time = parseInt((60 / bpm) * 1000);
                            timer = setInterval("onPlay()",time);
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
                               };
                        },
                        undefined,
                        "onStart",
                        "gadgets/Metronome_cc/main.js",
                        3);
var onStop = __typedjs(function  ()
                       {
                         if (timer)
                         {
                           clearInterval(timer);
                           timer = null;
                         };
                       },
                       undefined,
                       "onStop",
                       "gadgets/Metronome_cc/main.js",
                       4);
var incr = __typedjs(function  ()
                     {
                       if (280 > bpm)
                       {
                         bpm++;
                         options.putValue("bpm",bpm);
                         bpm_display.innerText = bpm;
                         if (flag == 1)
                         {
                           flag = 0;
                           onStart();
                         };
                       };
                     },
                     undefined,
                     "incr",
                     "gadgets/Metronome_cc/main.js",
                     5);
var decr = __typedjs(function  ()
                     {
                       if (60 < bpm)
                       {
                         bpm--;
                         options.putValue("bpm",bpm);
                         bpm_display.innerText = bpm;
                         if (flag == 1)
                         {
                           flag = 0;
                           onStart();
                         };
                       };
                     },
                     undefined,
                     "decr",
                     "gadgets/Metronome_cc/main.js",
                     6);
var onPlay = __typedjs(function  ()
                       {
                         if (curAudioClip_ == null)
                         {
                           curAudioClip_ = framework.audio.play(AUDIO_CLIP_URI,
                                                                onAudioStateChange);
                           startedAudio();
                         }
                         else {
                                curAudioClip_.stop();
                                curAudioClip_ = null;
                                stoppedAudio();
                              };
                       },
                       undefined,
                       "onPlay",
                       "gadgets/Metronome_cc/main.js",
                       7);
var onAudioStateChange = __typedjs(function  (audioClip,state)
                                   {
                                     if (state == gddSoundStateStopped)
                                     {
                                       stoppedAudio();
                                       curAudioClip_ = null;
                                     }
                                     else if (state == gddSoundStatePlaying)
                                          {
                                            startedAudio();
                                          };
                                   },
                                   undefined,
                                   "onAudioStateChange",
                                   "gadgets/Metronome_cc/main.js",
                                   8);
var startedAudio = __typedjs(function  ()
                             {
                               status_image.src = "green.png";
                             },
                             undefined,
                             "startedAudio",
                             "gadgets/Metronome_cc/main.js",
                             9);
var stoppedAudio = __typedjs(function  ()
                             {
                               status_image.src = "red.png";
                             },
                             undefined,
                             "stoppedAudio",
                             "gadgets/Metronome_cc/main.js",
                             10);
var check_key = __typedjs(function  ()
                          {
                            if (event.keycode == 45)
                            decr();
                            if (event.keycode == 43)
                            incr();
                          },
                          undefined,
                          "check_key",
                          "gadgets/Metronome_cc/main.js",
                          11);
