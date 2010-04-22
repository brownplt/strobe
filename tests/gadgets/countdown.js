/* Changes:
 * in getDateDiff, a bunch of casts, and have to
 *   add ".0" to the int literals to make them floats.
 * Otherwise, just had to insert some .toString()s
 */

// Change this to the date of the event.
var CONFIG_EVENT_DATE = new Date('1/1/3000');

// Updates the gadget.
function update() /*: -> Void */ {
  var now = new Date(undefined);
  var diff = getDateDiff(now, CONFIG_EVENT_DATE);

  if (diff.isPassed) {
    gadget.debug.trace('Event has passed, go to end state.');
    complete();
    return;
  }

  if (diff.days >= 1) {
    // Days until the event.
    var daysUntil = diff.days + 1;

    //Claudiu: daysUntil --> daysUntil.toString()
    timeLeftLabel.innerText = daysUntil.toString() + ' ' +
        (daysUntil > 1 ? strings.DAYS : strings.DAY) + ' ' +
        strings.UNTIL;

    // Determine next update.
    var nextUpdateMs = 0;

    if (diff.days == 1) {
      var dayBefore = new Date(CONFIG_EVENT_DATE);
      dayBefore.setDate(dayBefore.getDate() - 1);
      nextUpdateMs = dayBefore.valueOf() - now.valueOf();
    } else {
      // Update tomorrow midnight.
      var tomorrow = makeTomorrow(now);
      nextUpdateMs = tomorrow.valueOf() - now.valueOf();
    }

    view.setTimeout(update, nextUpdateMs);
    //Claudiu: .toString()
    gadget.debug.trace('Next update in ' + nextUpdateMs.toString() + ' ms.');
  } else {
    // Start the countdown!
    var s = '';

    if (diff.hours > 0) {
      s += diff.hours.toString() + ' ';
      s += (diff.hours > 1 ? strings.HOURS : strings.HOUR) + ' ';
    }
    if (diff.minutes > 0) {
      s += diff.minutes.toString() + ' ';
      s += (diff.minutes > 1 ? strings.MINUTES : strings.MINUTE) + ' ';
    }
    s += diff.seconds.toString() + ' ';
    s += (diff.seconds > 1 ? strings.SECONDS : strings.SECOND) + ' ';

    s += strings.UNTIL;

    timeLeftLabel.innerText = s;

    // Update again in one second.
    view.setTimeout(update, 1000);
  }
}

// Creates a date object for tomorrow midnight.
function makeTomorrow(d) /*:  Date -> Date */ {
  var tomorrow = new Date((d.getMonth() + 1).toString() + '/' +
                          d.getDate().toString() + '/' +
                          d.getYear().toString());
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
function getDateDiff(start, end)
  /*: Date * Date -> { isPassed : Bool, msec : Int, seconds : Int,
                       minutes : Int, hours : Int, days : Int } */
{
  var diff = (end.valueOf() - start.valueOf());

  var isPassed = diff <= 0;

  diff = Math.abs(diff);

  var msec = (diff % 1000);

  // Seconds.
  // Claudiu: put the Math.floor around the div, not around the mod.
  diff = Math.floor(diff / 1000);
  var seconds = diff % 60;

  // Minutes.
  diff = Math.floor(diff / 60);
  var minutes = diff % 60;

  // Hours.
  diff = Math.floor(diff / 60);
  var hours = diff % 24;

  // Days.
  diff = Math.floor(diff / 24);
  var days = diff;

  return { isPassed: isPassed, msec: msec, seconds: seconds,
          minutes : minutes, hours: hours, days: days };
}

// Called when date has passed.
// Displays the "completed" message.
function complete() /*: -> Void */ {
  timeLeftLabel.innerText = '';
  eventNameLabel.innerText = '';
  completedLabel.innerText = strings.CONFIG_COMPLETED;
}
