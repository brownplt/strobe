var CONFIG_EVENT_DATE = __new(Date,["1/1/3000"]);
var update = __typedjs(function  ()
                       {
                         var now = __new(Date,[]);
                         var diff = getDateDiff(now,CONFIG_EVENT_DATE);
                         if (diff.isPassed)
                         {
                           debug.trace("Event has passed, go to end state.");
                           complete();
                           return;
                         };
                         if (diff.days >= 1)
                         {
                           var daysUntil = diff.days + 1;
                           timeLeftLabel.innerText = daysUntil + " " + (daysUntil > 1 ? strings.DAYS : strings.DAY) + " " + strings.UNTIL;
                           var nextUpdateMs;
                           if (diff.days == 1)
                           {
                             var dayBefore = __new(Date,[CONFIG_EVENT_DATE]);
                             dayBefore.setDate(dayBefore.getDate() - 1);
                             nextUpdateMs = dayBefore - now;
                           }
                           else {
                                  var tomorrow = makeTomorrow(now);
                                  nextUpdateMs = tomorrow - now;
                                };
                           debug.trace("Next update in " + nextUpdateMs + " ms.");
                           view.setTimeout(update,nextUpdateMs);
                         }
                         else {
                                var s = "";
                                if (diff.hours > 0)
                                {
                                  s += diff.hours + " ";
                                  s += (diff.hours > 1 ? strings.HOURS : strings.HOUR) + " ";
                                };
                                if (diff.minutes > 0)
                                {
                                  s += diff.minutes + " ";
                                  s += (diff.minutes > 1 ? strings.MINUTES : strings.MINUTE) + " ";
                                };
                                s += diff.seconds + " ";
                                s += (diff.seconds > 1 ? strings.SECONDS : strings.SECOND) + " ";
                                s += strings.UNTIL;
                                timeLeftLabel.innerText = s;
                                view.setTimeout(update,1000);
                              };
                       },
                       undefined,
                       "update",
                       0);
var makeTomorrow = __typedjs(function  (d)
                             {
                               var tomorrow = __new(Date,
                                                    [(d.getMonth() + 1) + "/" + d.getDate() + "/" + d.getYear()]);
                               tomorrow.setDate(tomorrow.getDate() + 1);
                               return tomorrow;
                             },
                             undefined,
                             "makeTomorrow",
                             1);
var getDateDiff = __typedjs(function  (start,end)
                            {
                              var ret = {};
                              var diff = end - start;
                              ret.isPassed = diff <= 0;
                              diff = Math.abs(diff);
                              ret.msec = diff % 1000;
                              diff = diff / 1000;
                              ret.seconds = Math.floor(diff % 60);
                              diff = diff / 60;
                              ret.minutes = Math.floor(diff % 60);
                              diff = diff / 60;
                              ret.hours = Math.floor(diff % 24);
                              diff = diff / 24;
                              ret.days = Math.floor(diff);
                              return ret;
                            },
                            undefined,
                            "getDateDiff",
                            2);
var complete = __typedjs(function  ()
                         {
                           timeLeftLabel.innerText = "";
                           eventNameLabel.innerText = "";
                           completedLabel.innerText = strings.CONFIG_COMPLETED;
                         },
                         undefined,
                         "complete",
                         3);
