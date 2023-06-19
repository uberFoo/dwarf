# Rendering the Set
```dwarf, editable
struct Complex {
    re: float,
    im: float,
}

impl Complex {
    fn zero() -> Complex {
        Complex { re: 0.0, im: 0.0 }
    }

    fn add(self, other: Complex) -> Complex {
        Complex {
            re: self.re + other.re,
            im: self.im + other.im,
        }
    }

    fn add_2(self, other: Complex) {
        self.re = self.re + other.re;
        self.im = self.im + other.im;
    }

    fn square(self) -> Complex {
        Complex {
            re: self.re * self.re - self.im * self.im,
            im: 2.0 * self.re * self.im,
        }
    }

    fn square_2(self) {
        let re = self.re * self.re - self.im * self.im;
        self.im = 2.0 * self.re * self.im;
        self.re = re;
    }

    fn norm_squared(self) -> float {
        self.re * self.re + self.im * self.im
    }
}

fn pixel_to_point(
    width: int,
    height: int,
    pixel_x: int,
    pixel_y: int,
    upper_left: Complex,
    lower_right: Complex,
) -> Complex {
    let w = lower_right.re - upper_left.re;
    let h = upper_left.im - lower_right.im;
    Complex {
        re: upper_left.re + pixel_x as float * w / width as float,
        im: upper_left.im - pixel_y as float * h / height as float,
    }
}

fn escape_time(c: Complex, limit: int) -> Option<int> {
    // debugger;
    let z = Complex::zero();
    for i in 1..limit {
        ComplexEx::square(z);
        ComplexEx::add(z, c);
        let foo = ComplexEx::norm_squared(z);
        if foo > 4.0 {
            return i;
        }
    }
    0
}

fn main() -> Complex {
    let ty = chacha::typeof("mandelbrot");
    print("Type: " + ty + "\n");
    let time = chacha::time(plot);
    print("Total time: " + time as string + "\n");
}

fn plot() -> Complex {
    let width = 42;
    let height = 10;
    let upper_left = Complex { re: -2.5, im: 1.0 };
    let lower_right = Complex { re: 2.0, im: -1.0 };
    // for t in 0..255 {
        let t = 50;
        for row in 0..height {
        // OMG, this is an ugly hack, and yet it's so badass. If it had worked. 😢
        // ChaCha::spawn(do_column, [row, width, height, upper_left, lower_right]);
        // if row > 1 {
        // debugger;
        // }
            do_column(t, row, width, height, upper_left, lower_right);
        }
    // This is busted too.
    // print(chacha::eps() + "\n");
    // }
    print(chacha::eps());
    print("\n");
}

fn do_column(time: int, row: int, width: int, height: int, upper_left: Complex, lower_right: Complex) {
    for column in 0..width {
        let point = pixel_to_point(width, height, column, row, upper_left, lower_right);
        let time = escape_time(point, time);
        if time > 0 {
            print(" ");
        } else {
            print("*");
        }
    }
    print("\n");
}
```