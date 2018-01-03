# Exercise 1
Consider the following code:
```scm
(define (maki lst)
 (call/cc (lambda (exit)
            (for-each
                (lambda (x)
                    (call/cc (lambda (yield)
                        (exit (cons x yield))
                        )))
            lst))))

(define (doit)
 (let ((x (maki '(a b c d))))
    (when (cons? x)
        (displayln (car x))
        ((cdr x)))))
```

1. Give a brief description on how it works, and report the output of running (doit).
2. Write a macro, called curried, for defining curried functions that works in this example way:
```(define f (curried (x y) (+ x y)))```
so that f is a function that can be used e.g. like this: ```((f 3) 4) returns 7```.

## Solutions

### Question no. 1
```maki``` is evaluated with ```lst = '(a b c d)```. The first ```call/cc``` causes the creation of a new continuation object, ```exit```, pointing to the return of function ```maki```. Now the ```for-each``` syntax starts evaluating the lambda function ```l1``` on the first element of ```lst```, namely ```'a```. The ```call/cc``` creates a new continuation object, ```yield```, pointing right before the evaluation of the lambda on the second element of ```lst```.

The inner-most lambda ```l2``` calls the continuation object named ```exit```, passing a pair made of ```'a``` and the continuation object ```yield```.

```doit``` displays the first element of the pair, ```'a```, and tries to display ```yield```. This causes ```yield``` to get evaluated, restoring the continuation object pointed by ```yield``` itself, that is the evaluation of ```l1``` on the second element of ```lst```, namely ```'b```. The evaluation of ```l2``` creates a new continuation, replacing the one previously pointed by ```yield```, and the program follows the same execution flow as before.

The output of ```doit``` is then:
```
a
b
c
d
```

### Question no 2.
```scm
(define-syntax curried
  (syntax-rules ()
    [(_ (var1 var2) body)
     (lambda (var1)
       (lambda (var2) body))]))
```