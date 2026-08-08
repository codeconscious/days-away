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
File summary: 371 characters, 4 data lines.
Test   Future date                           2040-05-01      -5,015
Test   Super Nintendo release date in U.S.   1991-08-23      12,769
2 parse error(s):
* Invalid date "2040-A-01" in line with category "Test" and summary "Invalid data".
* Too many fields: Test, Invalid data, 2073-01-01, Extra column!
```
