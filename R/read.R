#' Reading all scans from a folder
#'
#' some more description
#' @param folder character describing the path to a folder
#' @param ext character value, consisting of the extension(s) describing the file format the scans are in
#' @export
#' @examples
#'  b1 <- read_bullet("/Volumes/Ocean/bullet-scans/LAPD/FAU 609/Bullet A", "x3p")
#'  b2 <- read_bullet("/Volumes/Ocean/bullet-scans/LAPD/FAU 609/Bullet B", "x3p")
read_bullet <- function(folder, ext="x3p") {
  set = dir(folder, pattern=ext, recursive = TRUE, full.names = TRUE)
  basenames <- basenames(set)
  message(paste("The following files were found: ", basenames, collapse="\n"))
}
