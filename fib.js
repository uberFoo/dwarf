function fib(n) {
    if (n <= 1) {
        return n;
    }

    return fib(n - 1) + fib(n - 2);
}

const args = process.argv.slice(2);
console.log('fib of :', args, fib(args));
