# Reading all x3p scans belonging to a folder

Read all scans of a specified format from a folder. This operation is
recursive, i.e. also reads scans from the folder of a folder.

## Usage

``` r
read_dir(
  path,
  extension = "x3p",
  hierarchy = c("land", "bullet", "barrel", "set")
)
```

## Arguments

- path:

  character describing the path to a folder

- extension:

  character value, consisting of the extension(s) describing the file
  format the scans are in

- hierarchy:

  vector of characters describing the folder structure, starting from
  lowest level to highest.

## Value

data frame of x3p files with appropriate meta information
