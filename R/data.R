
#' 3d topological surface measurements for one land of a bullet from the Hamby study
#'
#' Some more info - not sure at the moment which bullet this is. Describe structure.
#' @format a list
"br411"

#' randomforest
#'
#' this randomforest was fitted to predict known matches and non-matches from the scans of land engraved areas of the Hamby study.
#' @format a random forest object fitted by the randomforest function from the package of the same name
"rtrees"

#' hamby252demo
#'
#' Links to barrel 1, bullets 1 and 2, with all 6 lands.
#' @format a list containing lists of links for the 6 lands of bullet 1 and 2
"hamby252demo"

# hamby252demo <- list(
#   bullet1 =
#     c("https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/43567404-1611-4b40-ae74-a1e440e79f6a",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/a9f59fe1-f64b-487b-9f73-322ea0133a74",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/2ea4efe4-beeb-4291-993d-ae7726c624f4",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/6bb13db8-01ca-4cd4-ba5d-1c5670f1c204",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/2110e6c2-f801-458f-941a-9740804aa162",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/eaa73b31-8f9c-4b7f-a1c8-48e4da3ff9e0"),
#   bullet2 =
#     c("https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/979bf3f5-2bf4-43ab-aa14-66e79e0cbc99",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/b2b25004-364c-4468-b835-fd563b190a27",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/554c40d8-8857-4b1c-a28f-fda9b347999b",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/da019fc2-3a19-4da5-b1d7-ec059cd095f2",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/d6dfaef6-f066-4b76-bf42-f0e8c06d6241",
#       "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/a172932e-121c-4bee-9477-ae2454f0b513")
# )
# save(hamby252demo, file = here::here("data/hamby252demo.rda"))

#' hamby252demo_github
#'
#' Links to barrel 1, bullets 1 and 2, with all 6 lands, mirrored on github in case NBTRD is down
#' @format a list containing lists of links for the 6 lands of bullet 1 and 2
"hamby252demo_github"

# hamby252demo_github <- list(
#   bullet1 = c("https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet1_Land1.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet1_Land2.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet1_Land3.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet1_Land4.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet1_Land5.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet1_Land6.x3p?raw=true"),
#   bullet2 = c("https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet2_Land1.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet2_Land2.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet2_Land3.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet2_Land4.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet2_Land5.x3p?raw=true",
#               "https://github.com/heike/bulletxtrctr/blob/master/extra/Hamby252_Barrel1_Bullet2_Land6.x3p?raw=true")
# )
# usethis::use_data(hamby252demo_github)
