var _user$project$Native_UserStudyLog = {

    log : function(eventName) { return function(info) {
      var now = new Date()

      var logLine = now.toJSON() + "\t" + (now - 0) + "\t" + eventName + "\t" + info;

      console.log(logLine);

      var request = new XMLHttpRequest();
      request.open("POST", "/log_event", true);
      request.send(logLine);
    }; },

};
