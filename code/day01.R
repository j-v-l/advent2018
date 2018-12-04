library(pacman)
p_load(tidyverse)

dat <- read.table("data/day01_input.txt")


# Day 1 part 1 --------------------------------------------------------------------------------
sum(dat$V1)


# Day 1 part 2 --------------------------------------------------------------------------------
dat2 <- data.frame(x = rep(dat$V1, 1000)) %>%
  mutate(freq = cumsum(x),
         duped = duplicated(freq))
dat2 %>%
  filter(duped) %>%
  slice(1) %>%
  pull(freq)
