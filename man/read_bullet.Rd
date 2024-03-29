% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_bullet.R
\name{read_bullet}
\alias{read_bullet}
\title{Reading all x3p scans belonging to a single bullet from a folder}
\usage{
read_bullet(folder = NULL, ext = ".x3p$", urllist = NULL, size = NA)
}
\arguments{
\item{folder}{character describing the path to a folder}

\item{ext}{character value, consisting of the extension(s) describing the
file format the scans are in}

\item{urllist}{list of URLs pointing to x3p files}

\item{size}{specify size for reading binary file of surface matrix in x3p format}
}
\value{
data frame with two variables, source and x3p, containing the path
to the file and the corresponding x3p file
}
\description{
Read all scans of a specified format from a folder. This operation is recursive, i.e. also reads scans from the folder of a folder.
}
\examples{
\dontrun{
dir.create("data")
x3ptools::NRBTDsample_download("data")
b1 <- read_bullet("data/Bullet1", "x3p")
b2 <- read_bullet("data/Bullet2", "x3p")
on.exit(unlink("data", recursive = T))

b1 <- read_bullet(urllist = hamby252demo[[1]])
b2 <- read_bullet(urllist = hamby252demo[[2]])
}
}
