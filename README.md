# Days Away

This is a small Haskell command line application that lists how many days away dates in a CSV file are from today.

## Requirements

- Haskell
- A CSV file in which each line contains a category, summary, and date in YYYY-MM-DD format

Sample CSV input:

```
# NOTES:
# Lines beginning with `#` and empty lines are ignored.
# Headers are not supported.
# Spaces around commas will be ignored.
# Data elements must not contain commas themselves.

# Category, summary, date
Test, Super Nintendo release date in U.S., 1991-08-23
Test,Future date,2040-05-01
Test, Invalid data, 2040-A-01
Test, Invalid data, 2073-01-01, Extra column!
```

## Running

Run using `stack run -- path_to_your_file.csv`

Sample output:

```
This file has 184 total character(s) and 4 data line(s).
Test   Super Nintendo release date in U.S.   1991-08-23      12,742
Test   Future date                           2040-05-01      -5,042
There were 2 parse error(s):
* Error parsing date "2040-A-01" in line with category "Test" and summary "Invalid data": `Prelude.read: no parse`.
* Error parsing malformed line: Test, Invalid data, 2073-01-01, Extra column!
```
