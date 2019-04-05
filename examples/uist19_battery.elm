-- Simple Black Battery, abstracted b/c the point of the presentation in Lillicon is that you could make different versions for different icon sizes

[x, y] as point = [66, 148]

h4 = 141

w = 274

fill = 362

h = 73

batteryFunc ([x, y] as point) h4 w fill h =
  let body = rect fill point w h4 in
  let head = rect fill [ x+ w, (h4 - h + 2! * y) / 2!] 40 h in
  [body, head]

battery = batteryFunc point h4 w fill h

svg (concat [
  battery
])