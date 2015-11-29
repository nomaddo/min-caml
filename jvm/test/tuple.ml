let rec id x = x in
let y = (1, 3.14) in
let (a, b) = id y in
print_int a
