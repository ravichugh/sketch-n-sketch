port module WindowOnLoad exposing
  ( windowOnLoad
  )

port windowOnLoad : (() -> msg) -> Sub msg
