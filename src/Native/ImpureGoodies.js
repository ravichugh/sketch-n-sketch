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
        if (err.ctor === undefined) { // Internal crash, not something thrown with ImpureGoodies.throw below.
          // Nothing
          console.log(err);
          return _elm_lang$core$Maybe$Nothing;
        } else {
          throw err;
        }
      }
    },

    stringCharAt : function(index) {
      return function(string) {
        if(index >= string.length || index < 0) {
          return _elm_lang$core$Maybe$Nothing;
        } else {
          return _elm_lang$core$Maybe$Just(string[index]);
        }
      }
    },

    crashToError : function(thunk) {
      try {
        // Ok (thunk ())
        var result = thunk({ctor: '_Tuple0'});
        return _elm_lang$core$Result$Ok(result);
      } catch(err) {
        if (err.ctor === undefined) { // Internal crash, not something thrown with ImpureGoodies.throw below.
          // Err (toString err)
          return _elm_lang$core$Result$Err(err.toString());
        } else {
          throw err;
        }
      }
    },

    throw : function(exception) {
      throw(exception)
    },

    tryCatch : function(exceptionConstructorName) { return function(thunk) { return function(catchThunk) {
      try {
        return thunk({ctor: '_Tuple0'});
      } catch(exception) {
        if (exception.ctor === exceptionConstructorName) {
          return catchThunk(exception);
        } else {
          throw exception;
        }
      }
    }}},

    mutateRecordField : function(record) { return function(fieldName) { return function(newValue) {
      // Sanity check.
      if (typeof record[fieldName] == typeof newValue) {
        record[fieldName] = newValue;
        return record;
      } else {
        throw "ImpureGoodies.mutateRecordField: types do not match" + (typeof record[fieldName]) + " vs " + (typeof newValue);
      }
    }}},

    timedRun : function(thunk) {
      var start = (new Date()).getTime();
      var result = thunk(_elm_lang$core$Native_Utils.Tuple0);
      var end = (new Date()).getTime();

      return _elm_lang$core$Native_Utils.Tuple2(result, end-start);
    }

};
