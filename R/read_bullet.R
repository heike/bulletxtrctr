#' Reading all x3p scans belonging to a single bullet from a folder
#'
#' Read all scans of a specified format from a folder. This operation is recursive, i.e. also reads scans from the folder of a folder.
#' @param folder character describing the path to a folder
#' @param ext character value, consisting of the extension(s) describing the
#'          file format the scans are in
#' @param urllist list of URLs pointing to x3p files
#' @param size specify size for reading binary file of surface matrix in x3p format
#' @return data frame with two variables, source and x3p, containing the path
#'           to the file and the corresponding x3p file
#' @export
#' @import assertthat
#' @importFrom x3ptools read_x3p
#' @importFrom dplyr as_tibble
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
read_bullet <- function(folder = NULL, ext = ".x3p$", urllist = NULL, size = NA) {
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
    scans <- lapply(set, FUN = x3ptools::read_x3p, size = size)
  }
  as_tibble(data.frame(source = set, x3p = I(scans), stringsAsFactors = F))
}


#' Reading all x3p scans belonging to a folder
#'
#' Read all scans of a specified format from a folder. This operation is recursive, i.e. also reads scans from the folder of a folder.
#' @param path character describing the path to a folder
#' @param extension character value, consisting of the extension(s) describing the
#'          file format the scans are in
#' @param hierarchy vector of characters describing the folder structure, starting from lowest level to highest.
#' @export
#' @importFrom tidyr separate
#' @importFrom purrr map2
#' @return data frame of x3p files with appropriate meta information
read_dir <- function(path, extension = "x3p", hierarchy = c("land", "bullet", "barrel", "set")) {
  files <- NULL # to make R CMD CHECK happy
  get_meta_dir <- function(path, extension) {
    files <- dir(path, pattern = extension, recursive = TRUE)
    if (length(files) == 0) stop(sprintf("No files found at %s", path))
    meta <- data.frame(files = files, path = path, stringsAsFactors = FALSE)
    depth <- sapply(strsplit(files, split = "/"), FUN = length)
    meta$depth <- depth
    meta
  }

  meta <- get_meta_dir(path, extension)
  if (min(meta$depth) != max(meta$depth)) {
    stop("Directory has files at different levels")
  }
  # we know that we have files at only one level
  depth <- meta$depth[1]

  if (depth > length(hierarchy)) {
    warning(sprintf("Identify hierarchy to depth of %d, only have %d levels", max(meta$depth), length(hierarchy)))
    # we add levels now

    cat("we should add some levels here")
  }
  meta <- separate(meta, files, sep = "/", into = hierarchy[depth:1], remove = FALSE)

  if (nrow(meta) > 100) {
    cat()
  }
  meta <- meta %>% mutate(
    x3p = purrr::map2(.x = path, .y = files, .f = function(x, y) read_x3p(file.path(x, y)))
  )
  meta
}
