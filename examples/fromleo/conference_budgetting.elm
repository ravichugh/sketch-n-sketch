-- Use of 'exactly' and 'notBelow X'
-- to specify how values can(not) be
-- changed.

days =  exactly 3
venue = exactly 10000 * days
lunch = (notBelow 20) 30

participants = 200
fee          = 50
sponsors     = 20000

expenses = exactly participants * lunch * days + venue
income   = exactly participants * fee + sponsors

surplus = income - expenses

-- Change surplus to 0 twice
main = <div style="margin:20px">
Current surplus of conference:<br>
<h3 id="surplus">@(toString surplus)</h3>
@(if surplus /= 0 then
  <button onclick=
 "document.getElementById('surplus').innerText = '0'"
 >Set to zero</button> else
  <span>Hurray, the budget is coherent!</span>)
</div>



-- Useful library definitions

exactly x = freeze x

notBelow bound = Update.lens {
  apply x = x
  update {input, outputNew} =
    if outputNew <= bound &&
       input     >  bound     then
     Ok (Inputs [bound])
    else if outputNew > bound then
     Ok (Inputs [outputNew])
    else
     Ok (Inputs [])
  }