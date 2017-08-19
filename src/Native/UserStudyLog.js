window.userStudyEventNumber = 0

var _user$project$Native_UserStudyLog = {

    log : function(eventName) { return function(info) {
/*      var now = new Date()
      var eventNumber = (window.userStudyEventNumber += 1);

      var logLine = eventNumber + "\t" + now.toJSON() + "\t" + (now - 0) + "\t" + eventName + "\t" + info;

      console.log(logLine);

      function logRemotely() {
        var request = new XMLHttpRequest();

        // Retry request on failure after 5 second wait.
        // (We're trying not to drop events.)
        request.onreadystatechange = function () {
          if (request.readyState == 4 && request.status != 200) {
            setTimeout(logRemotely, 5 * 1000);
          }
        };

        request.open("POST", "/log_event", true);
        request.send(logLine);
      }
      logRemotely(); */

    }; },

};
