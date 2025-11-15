# Reading all x3p scans belonging to a single bullet from a folder

Read all scans of a specified format from a folder. This operation is
recursive, i.e. also reads scans from the folder of a folder.

## Usage

``` r
read_bullet(folder = NULL, ext = ".x3p$", urllist = NULL, size = NA)
```

## Arguments

- folder:

  character describing the path to a folder

- ext:

  character value, consisting of the extension(s) describing the file
  format the scans are in

- urllist:

  list of URLs pointing to x3p files

- size:

  specify size for reading binary file of surface matrix in x3p format

## Value

data frame with two variables, source and x3p, containing the path to
the file and the corresponding x3p file

## Examples

``` r
if (FALSE) { # \dontrun{
dir.create("data")
x3ptools::NRBTDsample_download("data")
b1 <- read_bullet("data/Bullet1", "x3p")
b2 <- read_bullet("data/Bullet2", "x3p")
on.exit(unlink("data", recursive = T))

b1 <- read_bullet(urllist = hamby252demo[[1]])
b2 <- read_bullet(urllist = hamby252demo[[2]])
} # }
```
