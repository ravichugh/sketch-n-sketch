notBelow bound x = {
  apply x = freeze x
  update {input, outputNew} =
    if outputNew <= bound && input > bound then
      {values = [bound]}
    else if outputNew > bound then
      { values = outputNew }
    else {values = []} }.apply x

exactly x = freeze x

days =
  exactly 3
venue =
  exactly (10000 * days)
lunch =
  (notBelow 20) 30

registeredParticipants =
  200
registrationFee =
  50
sponsors =
  20000

expenses =
  exactly registeredParticipants * lunch * days + exactly venue
income =
  exactly registeredParticipants * registrationFee +  exactly sponsors

surplus =
  income - expenses

-- Change surplus to 0, it changes the lunch but will stop at 20, so the surplus will be negative.
-- Change surplus to 0 again, it changes the registration fee.
["h3", [], [["TEXT", toString surplus]]]
