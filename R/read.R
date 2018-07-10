#' Reading all scans from a folder
#'
#' some more description
#' @param folder character describing the path to a folder
#' @param ext character value, consisting of the extension(s) describing the file format the scans are in
#' @return data frame with two variables, source and x3p, containing the path to the file and the corresponding x3p file
#' @export
#' @importFrom x3ptools read_x3p
#' @importFrom dplyr as.tbl
#' @examples
#'  b1 <- read_bullet("data/Bullet1", "x3p")
#'  b2 <- read_bullet("data/Bullet2", "x3p")
read_bullet <- function(folder, ext="x3p") {
  set = dir(folder, pattern=ext, recursive = TRUE, full.names = TRUE)
  message(sprintf("%d files found. Reading ...", length(set)))

  if (ext == "x3p") {
    scans <- lapply(set, FUN = read_x3p)
  }
  as.tbl(data.frame(source = set, x3p = I(scans)))
}
