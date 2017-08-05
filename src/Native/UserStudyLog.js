window.userStudyEventNumber = 0

var _user$project$Native_UserStudyLog = {

    log : function(eventName) { return function(info) {
      var now = new Date()
      var eventNumber = (window.userStudyEventNumber += 1);

      var logLine = eventNumber + "\t" + now.toJSON() + "\t" + (now - 0) + "\t" + eventName + "\t" + info;

      console.log(logLine);

      var request = new XMLHttpRequest();
      request.open("POST", "/log_event", true);
      request.send(logLine);
    }; },

};
