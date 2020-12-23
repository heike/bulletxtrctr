context("pipeline")

if (!requireNamespace("here", quietly = TRUE) |
  !requireNamespace("purrr", quietly = TRUE)) {
  skipall <- T
}

testthat::setup({
  # Download data if it is not present
  if (!dir.exists(here::here("tests/Bullet1")) |
    !dir.exists(here::here("tests/Bullet2"))) {
    dir.create(here::here("tests/Bullet1"))
    dir.create(here::here("tests/Bullet2"))
  }
  hambyb1l2 <- here::here("tests/Bullet1/Hamby252_Barrel1_Bullet1_Land2.x3p")
  hambyb2l4 <- here::here("tests/Bullet2/Hamby252_Barrel1_Bullet2_Land4.x3p")
  if (!file.exists(hambyb1l2)) {
    download.file(hamby252demo[[1]][2],
      destfile = hambyb1l2, quiet = T
    )
  }
  if (!file.exists(hambyb2l4)) {
    download.file(hamby252demo[[2]][4],
      destfile = hambyb2l4, quiet = T
    )
  }
})
# testthat::teardown({
#   file.remove(here::here("tests/Bullet1/Hamby252_Barrel1_Bullet1_Land3.x3p"))
#   unlink(here::here("tests/Bullet1"), recursive = T)
#   file.remove(here::here("tests/Bullet2/Hamby252_Barrel1_Bullet2_Land5.x3p"))
#   unlink(here::here("tests/Bullet2"), recursive = T)
# })

cleanfun <- function(x) x %>%
    x3ptools::x3p_scale_unit(scale_by = 10^6) %>%
    x3ptools::rotate_x3p(angle = -90) %>%
    x3ptools::y_flip_x3p()

test_that("bullet_pipeline works as expected", {
  tmp <- bullet_pipeline(here::here("tests"), stop_at_step = "read")
  expect_equal(names(tmp), c("source", "bullet", "x3p"))
  expect_equal(tmp$bullet, c("Bullet1", "Bullet2"))

  tmp <- bullet_pipeline(
    list(
      Bullet1 = c(hamby252demo$bullet1[2]),
      Bullet2 = c(hamby252demo$bullet2[4])
    ),
    stop_at_step = "read"
  )
  expect_equal(names(tmp), c("source", "bullet", "x3p"))
  expect_equal(tmp$bullet, c("Bullet1", "Bullet2"))

  tmp <- bullet_pipeline(here::here("tests"),
    stop_at_step = "clean",
    x3p_clean = cleanfun
  )
  expect_equal(names(tmp), c("source", "bullet", "x3p"))
  expect_equal(tmp$x3p[[1]]$header.info$incrementY, 1.5625)
  expect_equal(tmp$x3p[[1]]$header.info$sizeY, 500)

  tmp <- bullet_pipeline(here::here("tests"),
    stop_at_step = "crosscut",
    x3p_clean = cleanfun, ylimits = c(200, NA)
  )
  tmp2 <- bullet_pipeline(here::here("tests"),
    stop_at_step = "crosscut",
    x3p_clean = cleanfun
  )
  expect_equal(names(tmp), c("source", "bullet", "x3p", "crosscut", "ccdata"))
  expect_error(expect_equivalent(tmp$crosscut, tmp2$crosscut))

  tmp <- bullet_pipeline(here::here("tests"),
    stop_at_step = "grooves",
    x3p_clean = cleanfun
  )
  tmp2 <- bullet_pipeline(here::here("tests"),
    stop_at_step = "grooves",
    x3p_clean = cleanfun, method = "quadratic"
  )
  expect_equal(names(tmp), c(
    "source", "bullet", "x3p",
    "crosscut", "ccdata", "grooves"
  ))
  expect_error(expect_equivalent(tmp$grooves, tmp2$grooves))

  tmp <- bullet_pipeline(here::here("tests"),
    stop_at_step = "signatures",
    x3p_clean = cleanfun
  )
  expect_equal(names(tmp), c(
    "source", "bullet", "x3p",
    "crosscut", "ccdata", "grooves", "sigs"
  ))
})
