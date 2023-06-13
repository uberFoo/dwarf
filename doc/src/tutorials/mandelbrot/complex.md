# Coding a Complex Type

{{i: Complex numbers}} are really just a {{i: tuple}} of two {{i: float}}s.
One element is the real part of the number, and the other is the imaginary part.
The real part is plotted along the x-axis, and the imaginary part is plotted along the y-axis.

Addition and subtraction are defined as you would expect: perform the operation on the real and imaginary parts independently, e.g.: \\((a, m) + (b, n) = (a + b, m + n)\\).
To multiply two complex numbers, one must refer back to multiplying two binomials: \\((a + bi)(c + di) = ac + adi + bci + bdi^2\\).
But don't get caught up in the math — it's not on the test.

## Defining the Type

dwarf doesn't have tuples (yet) so we'll use *{{i: struct}}*s like so:

```dwarf
struct Complex {
    re: float,
    im: float,
}
```

This is how we declare a user defined type in dwarf.
It's the keyword *struct* followed by the name of the type, and then a block of fields.
Each field in the block is a name followed by a type, separated by a colon.
Each field is separated from another by a comma.
Trailing commas are just fine.

In this specific case we have a *struct* called `Complex` that has two fields, each of type *float*.
The first is called `re`, and the second, `im`.

Addition is fairly straightforward:

```dwarf
# struct Complex {
#     re: float,
#     im: float,
# }

impl Complex {
    fn add(self, other: Complex) -> Complex {
        Complex {
            re: self.re + other.re,
            im: self.im + other.im,
        }
    }
}
```

This is an *impl* block.
Functions that belong to the *struct* go into the *impl* block.


Similarly, the square function is not too bad:

```dwarf
# struct Complex {
#     re: float,
#     im: float,
# }

impl Complex {
    fn square(self) -> Complex {
        Complex {
            re: self.re * self.re - self.im * self.im,
            im: 2.0 * self.re * self.im,
        }
    }
}
```

Earlier I said that you know if you are in the set if you don't go to infinity and beyond.
We don't have that much time, and there's a shortcut.
While we are iterating, we can just check the absolute value of the complex number.
If it is greater than 2 then we know that the number will go to infinity.
When that happens we know that we are not in the set.

Rather than check the absolute value, we can just check of the value is greater than 4.
The problem of course is that 4 is a scalar, and we are dealing with complex numbers.
The solution is to take the *norm*, or *dot product* of the complex number.

```dwarf
# struct Complex {
#     re: float,
#     im: float,
# }

impl Complex {
    fn norm(self) -> float {
        self.re * self.re + self.im * self.im
    }
}
```
