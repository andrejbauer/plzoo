A simple arithmetic calculator with variables. It handles addition, subtraction,
negation, multiplication and division of integers.

The language is interactive, it cannot load files. Example interaction:

    calc_var -- programming languages zoo
    Type Ctrl-D to exit
    calc_var> 2+2
    4
    calc_var> x=10
    calc_var> y=x+20
    calc_var> 3 * x + 7
    37
    calc_var> z
    Error: unknown variable z
    calc_var> x=20
    calc_var> x + y
    50
    calc_var> x / 0
    Error: division by zero
