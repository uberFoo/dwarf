#!/usr/bin/python3

def fib(n):    # write Fibonacci series up to n
    """Print a Fibonacci series up to n."""
    if (n<= 1):
        return 1
    else:
        return fib(n-1) + fib(n-2)

def main():
    print(fib(25))

if __name__ == "__main__":
    main()