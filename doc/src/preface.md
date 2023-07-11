# Postface

I started dwarf on April 6, 2023.
It was meant to be as a DSL for it's parent project `sarzak`.
I wrote the date down because I wanted to know how long this side-trip took once I was done.
I was concerned that I'd spend too long on it, and I was excited to put it to use.
As you can see, it's all sort of gotten away from me since then.
It's a long way from being done, and I keep adding features.
It's grown far beyond the needs of my DSL, and yet I keep tweaking it.
I guess I'm having fun. 😀

Dwarf looks a lot like Rust — that's on purpose.
You see, I wanted to use `sarzak` to generate Rust code for dwarf.
Making that work was enough of a mental load.
Adding "programming language design" was more than I needed to deal with.
Like I already mentioned, dwarf is meant to be a DSL, and I plan on hacking on it quite a bit.
The more it looks like, and acts like Rust, the happier a cowboy be me.

Dwarf contains generated code — a lot of it.
Counting lines of code is a pretty terrible metric.
"Semantic density" varies within a language, and between languages.
Should you include comments?

Now that we've established `loc` is useless, here are some numbers.
As I write this there are (15/2)k (code/comments) `loc` that I've written myself.
The last time I generated the LuDog/AST-dwarf interface code it was 11k/300.
Each LuDog/AST backend "store" is (9/1.4)k, and I could generate at least five (Rc, (parking_lot | std)::Mutex, (parking_lot | std)::RwLock). [^std].
I have no idea what I'll end up actually shipping.
I won't bother to include the generated code upon which the stores depend.
Regardless, there are some metrics to gain insight into the ratio of generated to hand-written code.

The code generator is called `grace`, which I began in January 2023.
Dwarf is in fact necessary for me to continue work on grace.
Once I've escaped this recursive rabbit hole, I'll publish the whole mess.

So really, this is an experiment.
An exciting, fun, experiment

— Keith Star (June 8, 2023)

[^std]: After benchmarking the differences between Parking Lot and std, I got to digging.
Apparently the std sync stuff was recently updated.
