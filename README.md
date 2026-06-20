# days-away

A small command line application that listsg how many days away dates in a CSV file are from today. This is a small Haskell practice project.

## Requirements

- Haskell
- A CSV file in which each line contains a category, title, and date in YYYY-MM-DD format

Sample input:

```csv
Test, Super Nintendo release date in U.S., 1991-08-23
Test, Future date, 2040-05-01
Test, Invalid data, 2040-A-01
Test, Invalid data, 2040-A-01, Extra column!
```

## Running

Run using `stack run -- dates.csv`

Sample output:

```
Test | Super Nintendo release date in U.S. | 1991-08-23 | 12720
Test | Future date | 2040-05-01 | -5064
There were 2 parse error(s).
* Error parsing date " 2040-A-01" in line with category "Test" and summary " Invalid data": `Prelude.read: no parse`.
* Error parsing malformed line: Test, Invalid data, 2040-A-01, Extra column!
```
