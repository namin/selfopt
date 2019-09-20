# selfopt

prototyping self-optimizing systems

## Usage Example

In `chez` Scheme:
- `(load "selfopt.scm")`
- `(pipeline-adaptive 1000 10000)`
- `(summary_per)`

You'll see that `f` wins as n<=26 and `f_iter` wins as n>26, and the
win is proportionally bigger the further from n=26.
This is the output of `(print_summary_per)`:

```
( 0 (     f 0.500) (f_iter 0.500) )
( 1 (     f 0.503) (f_iter 0.497) )
( 2 (     f 0.504) (f_iter 0.496) )
( 3 (     f 0.502) (f_iter 0.498) )
( 4 (     f 0.511) (f_iter 0.489) )
( 5 (     f 0.518) (f_iter 0.482) )
( 6 (     f 0.527) (f_iter 0.473) )
( 7 (     f 0.530) (f_iter 0.470) )
( 8 (     f 0.543) (f_iter 0.457) )
( 9 (     f 0.550) (f_iter 0.450) )
(10 (     f 0.563) (f_iter 0.437) )
(11 (     f 0.567) (f_iter 0.433) )
(12 (     f 0.577) (f_iter 0.423) )
(13 (     f 0.598) (f_iter 0.402) )
(14 (     f 0.614) (f_iter 0.386) )
(15 (     f 0.630) (f_iter 0.370) )
(16 (     f 0.645) (f_iter 0.355) )
(17 (     f 0.642) (f_iter 0.358) )
(18 (     f 0.638) (f_iter 0.362) )
(19 (     f 0.653) (f_iter 0.347) )
(20 (     f 0.686) (f_iter 0.314) )
(21 (     f 0.646) (f_iter 0.354) )
(22 (     f 0.660) (f_iter 0.340) )
(23 (     f 0.657) (f_iter 0.343) )
(24 (     f 0.630) (f_iter 0.370) )
(25 (     f 0.606) (f_iter 0.394) )
(26 (     f 0.570) (f_iter 0.430) )
(27 (f_iter 0.513) (     f 0.487) )
(28 (f_iter 0.572) (     f 0.428) )
(29 (f_iter 0.676) (     f 0.324) )
(30 (f_iter 0.759) (     f 0.241) )
(31 (f_iter 0.811) (     f 0.189) )
(32 (f_iter 0.863) (     f 0.137) )
(33 (f_iter 0.902) (     f 0.098) )
(34 (f_iter 0.947) (     f 0.053) )
(35 (f_iter 0.962) (     f 0.038) )
(36 (f_iter 0.973) (     f 0.027) )
(37 (f_iter 0.982) (     f 0.018) )
(38 (f_iter 0.988) (     f 0.012) )
(39 (f_iter 0.993) (     f 0.007) )
```

The example is contrived, comparing fibonacci written recursively and
iteratively, and looping once vs 1000n times to give the recursive
function an edge start.

Are there real domains where the best implementations/optimizations
depend on the input or the distribution of inputs?

- [network functions](https://github.com/Michael137/nfv-benchmark), under investigation.
- Anything else?
