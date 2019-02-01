library(tidyverse)

hamby252dl <- tibble(barrel = 1, bullet = rep(1:2, each = 6),
                     land = rep(1:6, times = 2),
                     link = unlist(hamby252demo)) %>%
  mutate(filename = sprintf("extra/Hamby252_Barrel%d_Bullet%d_Land%d.x3p",
                            barrel, bullet, land)) %>%
  mutate(dl = map2(link, filename, download.file))
