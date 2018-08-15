# If on CRAN or TRAVIS, remove all .Rdata files generated during testing

if (identical(Sys.getenv("NOT_CRAN"), "true") & !identical(Sys.getenv("TRAVIS"), "true")) {

} else {
  data.file.list <- list.files(here::here("tests/"), "*.Rdata", full.names = T)
  data.file.list <- data.file.list[!data.file.list %in% okfiles]
  file.remove(data.file.list)
}

if (dir.exists(here::here("tests/Bullet1")) |
    dir.exists(here::here("tests/Bullet2"))) {
  unlink(here::here("tests/Bullet1"), recursive = T)
  unlink(here::here("tests/Bullet2"), recursive = T)
}
