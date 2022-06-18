## code to prepare `DATASET` dataset goes here

zysno47 <- matrix(
  c(0, 0, 1, 0,
    0, 0, 0, 1,
    1, 0, 1, 0,
    0, 0, 1, 1,
    1, 0, 2, 0,
    1, 0, 2, 1,
    1, 1, 2, 0,
    1, 1, 2, 0,
    1, 1, 3, 0,
    1, 1, 3, 0,
    1, 1, 2, 1,
    1, 1, 2, 1,
    1, 1, 2, 1,
    1, 2, 3, 0,
    1, 2, 3, 0,
    1, 1, 3, 1,
    1, 1, 3, 1,
    2, 2, 3, 0,
    2, 2, 3, 1,
    2, 2, 4, 1),
  ncol = 4,
  byrow=TRUE
)

usethis::use_data(zysno47, overwrite = TRUE)

library(tidyverse)
d <- foreign::read.spss("data-raw/Datenmatrix_CFT1R_Studie_3_FINAL_komplett.sav", to.data.frame = TRUE)
d2 <- foreign::read.spss("data-raw/Datenmatrix_CFT1R_Studie_2_FINAL_komplett.sav", to.data.frame = TRUE)
d3 <- foreign::read.spss("data-raw/Datenmatrix_CFT1R_Studie_1_FINAL_komplett.sav", to.data.frame = TRUE)

d <- d %>%
  select(starts_with("Subtest"), -ends_with("post")) %>%
  mutate_at(vars(starts_with("Subtest")), function(x) ifelse(is.na(x), 0, 1))
d2 <- d2 %>%
  select(starts_with("Subtest"), -ends_with("post")) %>%
  mutate_at(vars(starts_with("Subtest")), function(x) ifelse(is.na(x), 0, 1))
d3 <- d3 %>%
  select(starts_with("Subtest"), -ends_with("post")) %>%
  mutate_at(vars(starts_with("Subtest")), function(x) ifelse(is.na(x), 0, 1))
laura_post <- bind_rows(d, d2, d3)

usethis::use_data(laura_post, overwrite = TRUE)
