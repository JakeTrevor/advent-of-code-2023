# advent-of-code 2023

This year I decided to solve the advent of code in haskell, because I have just finished my functional programming course and I wanted some more practise.

Each day in the calendar has its own haskell file. There is a template in `src/DayX.hs`. For actual problems, the `X` is replaced with the correct day number. All solution files export two functions, `part1` and `part2`, which have the signature `String -> IO ()`. The string here is the name of an input file, for the problem input.

Problem inputs are stored in `./data`, labelled as `dx.txt` for problem inputs, or `dx-test.txt` for the shorter and more tractable test inputs - again, `x` is replaced with the day number.

## Running the solutions

To select which solution you would like to run, you needd to edit `app/main.hs`. First, make sure you are importing the parts from the correct solution file. Then make sure you are running the correct part in the body of the main function.

I have found the best way to run and test the solutions is with the command:

```pwsh
stack build && cls && stack run -- .\data\dx.txt
```

## Packages

So far, I have made extensive use of the Parsec library.
