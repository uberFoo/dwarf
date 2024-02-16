import sys
# Generate the fib numbers using recursive function call
def fib(n):
    if n <= 1:
        return n
    else:
        a = fib(n-1)
        b = fib(n-2)
        return a + b

# Check if command line arguments are provided
if len(sys.argv) > 1:
    # Access the command line arguments
    arg1 = sys.argv[1]

    arg1 = int(arg1)
    print(fib(arg1))
else:
    print("No command line arguments provided.")
