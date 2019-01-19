--# updatedelay:0

-- 6 minutes to create the basic file without winner check
-- 11 more minutes to implement nextinitturn, winner check,
--    the styling, removing button display once chosen. 
-- 28 LoC not including style or white lines
-- Notice how easy it was to
-- 1) Make sure a player does not play where someone else already played
--     (if then else in cellDisplay)
-- 2) Record someone's move (turn, i) (always (nextturn turn, turn))
-- 3) Make sure no one can play after there is a winner.

grid = [[0, 0, 0],
        [0, 0, 0],
        [0, 0, 0]]
        
init = [[0, 0, 0],
        [0, 0, 0],
        [0, 0, 0]]

winner: Maybe Int
winner =
  let f x y = nth (nth grid x) y, {orElse} = Maybe in
  let hf x = if f x 0 /= 0 && f x 0 == f x 1 && f x 1 == f x 2 then Just (f x 0) else Nothing in
  let vf y = if f 0 y /= 0 && f 0 y == f 1 y && f 1 y == f 2 y then Just (f 0 y) else Nothing in
  let dg d = let h=f<<d in if h 0 0 == h 1 1 && h 1 1 == h 2 2 && h 0 0 /= 0 then Just (h 0 0) else Nothing in
  (hf 0) |> orElse (hf 1) |> orElse (hf 2) |> orElse
  (vf 0) |> orElse (vf 1) |> orElse (vf 2) |> orElse
  (dg identity) |> orElse (dg ((-) 2))

nextinitturn  = 2
turn          = 1
nextturn turn = freeze 3 - turn
renderNum i   =  case i of 0 -> ""; 1 -> "X"; 2 -> "O"

cellDisplay i =
  if i /= 0 || winner /= Nothing
  then  <div class="choice">@(renderNum i)</div>
  else  Html.button <div>@(renderNum (if i == 0 then turn else i))</div>  "" (turn, i) (always (nextturn turn, turn))

rowDisplay cells =
  <div class="row">@cells</div>

<span>
@List.map(List.map cellDisplay >> rowDisplay)(grid)
@Html.button("reset")("")(grid, turn, nextinitturn)(always (init, nextinitturn, nextturn nextinitturn))
<style>
.row .choice {
  font-size: 2em;
}
.row .choice, .row button {
  width: 50px;
  height: 50px;
  text-align: center;
  vertical-align: middle;
  display: inline-block;
  padding: 0 !important;
}
.row .choice {
  padding-top: 0.2em !important;
  margin: 0 !important;
}
.row > button > div {
  opacity: 0;
  font-size: 2em;
  cursor: pointer;
}
.row > button > div:hover {
  opacity: 0.5;
  color: #00F;
}
</style>
@(winner |> Maybe.map ((+) "The winner is " << renderNum) |> Maybe.withDefault "No current winner")
</span>