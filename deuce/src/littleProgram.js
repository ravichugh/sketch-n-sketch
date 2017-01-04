
var topDefs =
  [ { ws_before_open_paren : " " , ws_before_ident : " "
    , ident : "a" , exp : " 1"
    , ws_before_close_paren : ""
    }
  , { ws_before_open_paren : " " , ws_before_ident : " "
    , ident : "b" , exp : " 2"
    , ws_before_close_paren : ""
    }
  , { ws_before_open_paren : " " , ws_before_ident : " "
    , ident : "c" , exp : " (+ a b)"
    , ws_before_close_paren : ""
    }
  ];

var dependencies =
  { "a" : []
  , "b" : []
  , "c" : ["a", "b"]
  };

