context("read")

load(here::here("tests/bullet1_only.Rdata"))

o1 <- capture.output(b2 <- read_bullet(here::here("tests/Bullet1"), "x3p"), split = T)
b3 <- b2 %>%
  # turn the scans such that (0,0) is bottom left
  dplyr::mutate(
    x3p = x3p %>% purrr::map(.f = function(x) x %>%
                               x3ptools::rotate_x3p(angle=-90) %>%
                               x3ptools::y_flip_x3p())
  ) %>% dplyr::mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
      # make sure all measurements are in microns
      x$surface.matrix <- x$surface.matrix*10^6
      x$header.info$incrementY <- x$header.info$incrementY*10^6
      x$header.info$incrementX <- x$header.info$incrementX*10^6
      x
    })
  )

test_that("read works as expected", {
  expect_length(o1, 0)
  expect_s3_class(b2, "tbl_df")
  expect_s3_class(b2, "tbl")
  expect_s3_class(b2, "data.frame")
  expect_equal(b1_l3_x3p$x3p, b3$x3p)
})

