var __globalBoolState__ = false;



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

    putCache: function(record) { return function(cacheName) { return function(newValue) {
      if(typeof record == "object") {
        //record[" cache_" + cacheName] = newValue;
        console.log("stored cache " + cacheName, record)
      } else
        throw "ImpureGoodies.putCache: this is not an object";
      return newValue;
    }}},

    getCache: function(record) { return function(cacheName) {
      if(typeof record != "object")
        throw "ImpureGoodies.putCache: this is not an object";
      console.log("getting cache " + cacheName, record)
      var res = record[" cache_" + cacheName];
      if (typeof res == "undefined")
        return _elm_lang$core$Maybe$Nothing;
      else
        return _elm_lang$core$Maybe$Just(res);
    }},

    toggleGlobalBool : function(_) {
      __globalBoolState__ = !__globalBoolState__;
      return __globalBoolState__;
    },

    getCurrentTime : function() {
      return (new Date()).getTime();
    },

    timedRun : function(thunk) {
      var start = (new Date()).getTime();
      var result = thunk(_elm_lang$core$Native_Utils.Tuple0);
      var end = (new Date()).getTime();

      return _elm_lang$core$Native_Utils.Tuple2(result, end-start);
    },

    evaluate : function(string) {
      return eval(string);
    },

    log : function(string) {
      console.log(string);
      return string;
    },

    htmlescape: (function() {
      var tagsToReplace = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;'
      };

      function replaceTag(tag) {
        return tagsToReplace[tag] || tag;
      }
      function safe_tags_replace(str) {
        return str.replace(/[&<>]/g, replaceTag);
      }
      return function(string) {
        return safe_tags_replace(string);
      }
    })(),

    htmlunescape: (function() {
      var tagsToReplace = {
        '&amp;': '&',
         '&lt;': '<',
        '&gt;': '>'
      };

      function replaceTag(tag) {
        return tagsToReplace[tag] || tag;
      }
      function safe_tags_replace(str) {
        return str.replace(/&(amp|lt|gt);/g, replaceTag);
      }
      return function(string) {
        return safe_tags_replace(string);
      }
    })(),

    emptyNativeRecord: function(dummy) {
      return {};
    },

    addPairToNativeRecord: function(key) {
      return function(value) {
        return function(record) {
          record[key] = value;
          return record;
        }
      }
    },

    setValueToNativeRecord: function(key) {
      return function(mbValue) {
        return function(record) {
          if(typeof record  == "object") {
            if(mbValue.ctor == "Nothing") {
              delete record[key];
            } else {
              record[key] = mbValue._0;
            }
          }
          return record;
        }
      }
    },

    keyPairsOfNativeRecord: function(record) {
      var recordKeys = Object.keys(record);
      if(recordKeys.length == 0) return _elm_lang$core$Maybe$Nothing;
      var acc = _elm_lang$core$Native_List.Nil;
      for(var i = 0; i < recordKeys.length; i ++) {
        var key = recordKeys[i];
        var value = record[key];
        acc = _elm_lang$core$Native_List.Cons(_elm_lang$core$Native_Utils.Tuple2(key, value), acc)
      }
      return acc;
    },

    nativeRecordGet: function(key) {
      return function(record) {
        if(typeof record == "object") {
          if(typeof record[key] == "undefined") {
              return _elm_lang$core$Maybe$Nothing;
          } else {
            return _elm_lang$core$Maybe$Just(record[key]);
          }
        } else return _elm_lang$core$Maybe$Nothing;
      }
    },

    nativeRecordKeys: function(record) {
     var result = Object.keys(record);
     var listResult = {ctor:"[]"};
     for(var i = result.length - 1; i >= 0; i--) {
       listResult = {ctor:"::",_0:result[i],_1:listResult};
     }
     return listResult;
    },

    toNativeArray: function(elmList) {
      var acc = [];
      while(elmList.ctor != "[]") {
        acc.push(elmList._0);
        elmList = elmList._1;
      }
      return acc;
    },

    fromNative: function(v) {
      return function(stringCallback) {
        return function(numCallback) {
          return function(boolCallback) {
            return function(listCallback) {
              return function(listRecordCallback) {
                if(typeof v == "string") return stringCallback(v);
                if(typeof v == "number") return numCallback(v);
                if(typeof v == "boolean") return boolCallback(v);
                if(typeof v == "function") throw "Cannot convert a native function to a value";
                if(typeof v == "object") {
                  if(Array.isArray(v)) {
                    var result = _elm_lang$core$Native_List.Nil;
                    for(var i = v.length - 1; i >= 0; i-- ) {
                      result = _elm_lang$core$Native_List.Cons(v[i], result)
                    }
                    return listCallback(result)
                  } else {
                    return listRecordCallback(_user$project$Native_ImpureGoodies.keyPairsOfNativeRecord(v))
                  }
                }
              }
            }
          }
        }
      }
    },

    hideType: function(v) {
      return v;
    }

};
