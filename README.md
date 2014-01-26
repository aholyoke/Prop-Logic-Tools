Prop-Logic-Tools
================

An interpreter that parses propositional logic formulas and lets you run built-in algorithms on them (also generates truth tables).

The interpreter is incomplete so if you want to try it out, I recommend loading Main.hs into ghci and running the algorithms by hand.
eg.

	Prelude> :l Main.hs
	Main> makeTable "a|~b^c"
	a    b    c    a|(~b^c)
	T    T    T    T
	T    T    F    T
	T    F    T    T
	T    F    F    T
	F    T    T    F
	F    T    F    F
	F    F    T    T
	F    F    F    F
	Main> conjunctiveNormalForm (p "a|~b^c")
	(a|~b)^(a|c)
	Main> makeTable "(a|~b)^(a|c)"
	a    b    c    (a|~b)^(a|c)
	T    T    T    T
	T    T    F    T
	T    F    T    T
	T    F    F    T
	F    T    T    F 
	F    T    F    F 
	F    F    T    T
	F    F    F    F

In the example above, the function p is a utility function used to parse the string with no error-handling so that it can be passed directly to the algorithms.
The function makeTable does this already so it should not be used there.


This project uses Parsec (Daan Leijen) for parsing, Uniplate (Neil Mitchell) to cut down on boilerplate in the algorithms, and PrettyPrint.Boxes (Brent Yorgey) for the truth tables.
This is mostly untested and should not be used in production code, use at your own risk.

