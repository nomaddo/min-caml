let rec id x = x in
let rec add i j = id (i + 2) + j in
print_int (add 10 314)
