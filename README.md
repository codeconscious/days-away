# days-away

A small command line application that lists how many days away dates in a CSV file are from today. This is a small Haskell practice project.

## Requirements

- Haskell
- A CSV file in which each line contains a category, title, and date in YYYY-MM-DD format (with no headers)

Sample CSV input:

```
# Lines beginning with `#` and empty lines are ignored.

Test, Super Nintendo release date in U.S., 1991-08-23
Test, Future date, 2040-05-01
Test, Invalid data, 2040-A-01
Test, Invalid data, 2040-A-01, Extra column!

```

## Running

Run using `stack run -- path_to_your_file.csv`

Sample output:

```
This file has 4 line(s) and 216 character(s).
Test   Super Nintendo release date in U.S.   1991-08-23      21,537
Test   Future date                           2040-05-01      -5,940
There were 2 parse error(s):
* Error parsing date " 2040-A-01" in line with category "Test" and summary " Invalid data": `Prelude.read: no parse`.
* Error parsing malformed line: Test, Invalid data, 2040-A-01, Extra column!
```
