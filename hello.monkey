let fib = fn(n) {
    if (n < 2){ n }
    else { fib(n - 1) + fib(n - 2) }
};

let n = 20
let got = fib(n);
puts("Hello world: fib(" + str(n) + ") = " + str(got));
