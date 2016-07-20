let a = Array.create 10 3.14 in
let b = Array.create 10 10 in
a.(1) <- a.(2) +. 1.5;
b.(3) <- b.(2);
print_int (truncate a.(1) + b.(1))
