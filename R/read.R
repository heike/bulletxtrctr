#' Reading all scans from a folder
#'
#' some more description
#' @param folder character describing the path to a folder
#' @param ext character value, consisting of the extension(s) describing the
#'          file format the scans are in
#' @param urllist list of URLs pointing to x3p files
#' @return data frame with two variables, source and x3p, containing the path
#'           to the file and the corresponding x3p file
#' @export
#' @import assertthat
#' @importFrom x3ptools read_x3p
#' @importFrom dplyr as.tbl
#' @examples
#' \dontrun{
#' dir.create("data")
#' x3ptools::NRBTDsample_download("data")
#' b1 <- read_bullet("data/Bullet1", "x3p")
#' b2 <- read_bullet("data/Bullet2", "x3p")
#' on.exit(unlink("data", recursive = T))
#'
#' b1 <- read_bullet(urllist = hamby252demo[[1]])
#' b2 <- read_bullet(urllist = hamby252demo[[2]])
#' }
read_bullet <- function(folder = NULL, ext = ".x3p$", urllist = NULL) {
  assert_that(!is.null(folder) | !is.null(urllist))

  if (!is.null(folder) & !is.null(urllist)) {
    message("folder and urllist both provided. Reading x3p files from folder.")
  }

  if (!is.null(folder)) {
    lapply(folder, function(x) assert_that(is.dir(x)))
    set <- dir(folder, pattern = ext, recursive = TRUE, full.names = TRUE)
    message(sprintf("%d files found. Reading ...", length(set)))
  } else {
    lapply(unlist(urllist), function(x) {
      assert_that(grepl("^(http|www)", x))
    })
    set <- unlist(urllist)
  }
  if (length(set) == 0) stop("No files found. Check path/URL.")

  if (ext == ".x3p$" | ext == "x3p") {
    scans <- lapply(set, FUN = x3ptools::read_x3p)
  }
  as.tbl(data.frame(source = set, x3p = I(scans), stringsAsFactors = F))
}
