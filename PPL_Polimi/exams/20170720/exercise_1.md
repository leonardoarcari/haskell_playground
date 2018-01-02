# Exercise 1
Consider the following code:
```scm
(define (print+sub x y)
    (display x)
    (display " ")
    (display y)
    (display " -> ")
    (- x y))

(define (puzzle)
 (call/cc (lambda (exit)
            (define (local e)
             (call/cc
              (lambda (local-exit)
                (exit (print+sub e
                                 (call/cc
                                 (lambda (new-exit)
                                  (set! exit new-exit)
                                  (local-exit #f))))))))
            (local 6)
            (exit 2))))
```

1. Describe how it works.
2. What is the output of the evaluation of ```(define x (puzzle))```? What is the value of ```x```?
3. Can you see problems with this code?

## Solutions
**Assumption**: While discussing the execution of this program I assume that the *call-with-continuation* is implemented with a *stack-strategy*.

#### Question no. 1
Immediately after ```puzzle``` is invoked, the ```call/cc``` expression causes the creation of a continuation object named ```exit``` pointing right at the end of function ```puzzle```. After defining function ```local```, the execution procedes by evaluating ```(local 6)```. The ```call/cc``` within ```local``` causes the creation of a new continuation object named ```local-exit``` pointing right before the evaluation of the expression ```(exit 2)```. Execution procedes by evaluating the ```exit``` object with expression ```e1 = (print+sub e ...)```.
My speculation here is that the address of the ```exit``` object is pushed on the execution stack at this very moment; the purpose of this comment will become clear later.
So we need to evaluate expression ```e1``` which require us to evaluate first ```e``` and ```e2 = (call/cc (lambda (new exit) ...))```. The evaluation of ```call/cc``` in ```e2``` creates a new continuation object named ```new-exit``` pointing within the evaluation of ```(print+sub e e2)```. 
Going with the evaluation of ```e2``` we set varibale ```exit``` to ```new-exit``` so now ```exit``` and ```new-exit``` point to the same continuation (i.e.: within the evaluation of ```(print+sub e e2)```).
Then we evaluate ```(local-exit #f)```. This causes the stack to be replaced with the continuation object pointed by ```local-exit```, that is the evaluation of ```(exit 2)```. But now ```exit``` points to the same continuation of ```new-exit``` causing the evaluation of ```(print+sub e 2)```.
The evaluation prints on stdout the value of ```e``` and ```" 2 -> "```, returning the result of ```e - 2```. At this point we can evaluate the expression ```(exit e1)``` with ```e1 = e - 2```. At first glance, you might think that because ```exit``` was set to ```new-exit``` we replace the current continuation with the one pointed by ```new-exit```, causing again the evaluation of ```print+sub``` and looping over and over.
Although, because of what I speculated above, the address where to jump after evaluating ```exit``` is already set on the execution stack, so it points to the original continuation, i.e. at the end of function ```puzzle```. So eventually, the evaluation of ```puzzle``` returns ```e - 2 ```.

#### Question no. 2
Replacing ```e``` with ```6``` in the description above, we understand that the program prints on the stdout the following:
```
6 2 -> 
```
and the value of ```x``` is ```4```.

#### Question no. 3
I can only spot one problem (aside from the cruelty of the exercise designer). I did not read the Scheme standard, but the behavior of this piece of code could be undefined, i.e. interpreter-implementation dependent. In fact, if the evaluation of ```(exit (print+sub e ...))``` is lazily evaluated, i.e. the continuation object pointed by ```exit``` is looked up only when all the expression parameters have been evaluated, we end up with a different behavior, that is: a loop as described above.