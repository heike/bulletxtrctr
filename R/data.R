
#' 3d topological surface measurements for one land of a bullet from the Hamby study
#'
#' A 3d topographic surface map of Land 1 on Bullet 1 of Barrel 4 (Hamby study 252).
#' The file is stored in x3p format (ISO standard for forensic measurements).
#' @format an x3p object (list) with 5 elements
#' \describe{
#'   \item{header.info}{list consisting of scan dimensions and scan resolution (in microns per pixel)}
#'   \item{surface.matrix}{2d numeric matrix of the scan heights.}
#'   \item{feature.info}{list containing scanning details and x3p formatting}
#'   \item{general.info}{list containing instrumentation details}
#'   \item{matrix.info}{list of technical details for the matrix encoding}
#' }
"br411"

#' randomforest
#'
#' This randomforest was fitted to predict known matches and non-matches from the scans of land engraved areas of the Hamby study (see Hare et al ).
#' @format a random forest object fitted by the randomforest function from the package of the same name
"rtrees"

#' hamby252demo
#'
#' Links to barrel 1, bullets 1 and 2, with all 6 lands.
#' @format a list containing lists of links for the 6 lands of bullet 1 and 2
"hamby252demo"


#' hamby252demo_github
#'
#' Links to barrel 1, bullets 1 and 2, with all 6 lands, mirrored on github in case NBTRD is down
#' @format a list containing lists of links for the 6 lands of bullet 1 and 2
"hamby252demo_github"


