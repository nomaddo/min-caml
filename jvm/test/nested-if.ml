let rec is_one i = if i = 1 then true else false in
let a = if is_one 9 then if is_one 3 then print_int 4 else print_int 5 else print_int 2 in
if is_one 1 then
  if is_one 3
  then print_int 4
  else if is_one 3
       then print_int 5
       else print_int 2
else
  print_int 4
