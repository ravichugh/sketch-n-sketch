var _user$project$Native_ImpureGoodies = {

    randomFloat : function(_) {
      return Math.random();
    },

    crashToNothing : function(thunk) {
      try {
        // Just (thunk ())
        var result = thunk({ctor: '_Tuple0'});
        return _elm_lang$core$Maybe$Just(result);
      } catch(err) {
        // Nothing
        console.log(err);
        return _elm_lang$core$Maybe$Nothing;
      }
    },

    crashToError : function(thunk) {
      try {
        // Ok (thunk ())
        var result = thunk({ctor: '_Tuple0'});
        return _elm_lang$core$Result$Ok(result);
      } catch(err) {
        // Err (toString err)
        return _elm_lang$core$Result$Err(err.toString());
      }
    },

    timedRun : function(thunk) {
      var start = (new Date()).getTime();
      var result = thunk(_elm_lang$core$Native_Utils.Tuple0);
      var end = (new Date()).getTime();

      return _elm_lang$core$Native_Utils.Tuple2(result, end-start);
    }

};
