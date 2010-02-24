/*::
  function update : ([Dom]  -> Void)
  function makeTomorrow : ([Dom] Date -> Date)
  function getDateDiff : ([Dom] Date * Date -> {isPassed : Bool, msec : Int, seconds : Int, minutes : Int, hours : Int, days : Int})
*/

/*
Copyright (C) 2007 Google Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

// Change this to the date of the event.
var CONFIG_EVENT_DATE = new Date('1/1/3000');

// Updates the gadget.
function update() {
  var now = new Date(); 
  var diff = getDateDiff(now, CONFIG_EVENT_DATE);
  
  if (diff.isPassed) {
    debug.trace('Event has passed, go to end state.');
    complete();
    return;
  }

  if (diff.days >= 1) {
    // Days until the event.
    var daysUntil = diff.days + 1;
    
    timeLeftLabel.innerText = daysUntil + ' ' +
        (daysUntil > 1 ? strings.DAYS : strings.DAY) + ' ' +
        strings.UNTIL;
        
    // Determine next update.
    var nextUpdateMs;
    
    if (diff.days == 1) {
      var dayBefore = new Date(CONFIG_EVENT_DATE);
      dayBefore.setDate(dayBefore.getDate() - 1);
      nextUpdateMs = dayBefore - now;
    } else {
      // Update tomorrow midnight.
      var tomorrow = makeTomorrow(now);
      nextUpdateMs = tomorrow - now;
    }

    debug.trace('Next update in ' + nextUpdateMs + ' ms.');
    view.setTimeout(update, nextUpdateMs);
  } else {
    // Start the countdown!
    var s = '';

    if (diff.hours > 0) {
      s += diff.hours + ' ';
      s += (diff.hours > 1 ? strings.HOURS : strings.HOUR) + ' ';
    }
    if (diff.minutes > 0) {
      s += diff.minutes + ' ';
      s += (diff.minutes > 1 ? strings.MINUTES : strings.MINUTE) + ' ';
    }
    s += diff.seconds + ' ';
    s += (diff.seconds > 1 ? strings.SECONDS : strings.SECOND) + ' ';
        
    s += strings.UNTIL;
        
    timeLeftLabel.innerText = s;
        
    // Update again in one second.
    view.setTimeout(update, 1000);
  }
}

// Creates a date object for tomorrow midnight.
function makeTomorrow(d) {
  var tomorrow = new Date((d.getMonth() + 1) + '/' + 
                          d.getDate() + '/' +
                          d.getYear());
  tomorrow.setDate(tomorrow.getDate() + 1);
  
  return tomorrow;
}

// Calculates the difference between two dates.
// - start: the start date.
// - end: the end date.
//
// Returns an Object with the following fields:
// - isPassed: Indicates whether the start date is >= than end date.
// - days: Day component of difference.
// - hours: Hour component of difference.
// - minutes: Minute component of difference.
// - seconds: Second component of difference.
// - msec: Millisecond component of difference.
//
// All data will be non-negative. Use "isPassed" to determine the relation
// between the dates.
function getDateDiff(start, end) {
  var ret = {};

  var diff = end - start;
 
  ret.isPassed = diff <= 0;
  
  diff = Math.abs(diff);
  
  ret.msec = diff % 1000;       

  // Seconds.  
  diff = diff / 1000;  
  ret.seconds = Math.floor(diff % 60);    
  
  // Minutes.  
  diff = diff / 60;  
  ret.minutes = Math.floor(diff % 60);    

  // Hours.  
  diff = diff / 60;  
  ret.hours = Math.floor(diff % 24);

  // Days.
  diff = diff / 24;  
  ret.days = Math.floor(diff);
  
  return ret;
}

// Called when date has passed.
// Displays the "completed" message.
function complete() {
  timeLeftLabel.innerText = '';
  eventNameLabel.innerText = '';
  completedLabel.innerText = strings.CONFIG_COMPLETED;
}
