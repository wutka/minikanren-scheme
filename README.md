# A Scheme Interpreter in mikiKanren

I have been watching Will Byrd's videos of his miniKanren uncourse
from a few years ago, and the homework after lesson 4 was to extend
the implementation to include cons, car, cdr and other stuff.

I extended it enough to implement a factorial function. Numbers are implemented
as lists of numbers. This means that adding and multiplying digits require
a lot of choices, but it seemed like an easy way to input the numbers.

Here is how you can compute the factorial of 7 (higher numbers take a long time):

```scheme
  (run 1 (q) (eval-expo
    '(let ((f
      (lambda (f n c p)
        (if (eq? n c)
            (* p c)
            (f f n (+ c (1)) (* p c))))))
      (f f (7) (1) (1)))
    '() q))
```

