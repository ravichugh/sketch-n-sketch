var _user$project$Native_MissingNumberMethods = {
  exp: Math.exp,
  power: function(a) { return function(b) { return Math.pow(a, b); }},
  modulo: function(a) { return function(b) { return a % b; }},
};