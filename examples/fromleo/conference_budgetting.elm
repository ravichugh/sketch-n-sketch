notBelow bound x = {
  apply x = freeze x
  update {input, outputNew} =
    if outputNew <= bound &&
       input     >  bound     then { values = [bound] }
    else if outputNew > bound then { values = outputNew }
    else                           { values = [] }
  }.apply x

exactly x = freeze x

days =  exactly 3
venue = exactly (10000 * days)
lunch = (notBelow 20) 30

participants = 200
fee          = 50
sponsors     = 20000

expenses = exactly participants * lunch * days + venue
income   = exactly participants * fee + sponsors

surplus = income - expenses

-- Change surplus to 0, it changes the lunch but will stop at 20, so the surplus will be negative.
-- Change surplus to 0 again, it changes the registration fee.
["div", [["style", "margin:20px"]], [
  ["TEXT", "Current surplus of conference:"],
  ["br", [], []],
  ["h3", [["id", "surplus"]], [
    ["TEXT", toString surplus]]],
  if surplus /= 0 then
    ["button", [["onclick", "document.getElementById('surplus').innerText = '0'"]], [["TEXT", "Set to zero"]]]
  else
    ["TEXT", "Hurray, the budget is coherent!"]
    ]]