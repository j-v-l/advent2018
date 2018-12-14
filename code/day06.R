library(pacman)
p_load(tidyverse, lubridate, janitor)

coords <- read.delim("data/day06_input.txt", header = F, as.is = T, sep = ",") %>%
  rename(x = V1, y = V2)


# Day 6 part 1 --------------------------------------------------------------------------------
find_closest_point <- function(x, y, df) {
  md <- abs(x - df$x) + abs(y - df$y)
  closest_point <- which.min(md)
  if (length(md[md == min(md)]) > 1) closest_point <- NA
  
  closest_point
}

grid <- expand.grid(x = min(coords$x):max(coords$x), y = min(coords$y):max(coords$y))

result <- grid %>%
  mutate(on_edge = x %in% c(min(x), max(x)) | y %in% c(min(y), max(y))) %>%
  rowwise() %>%
  mutate(closest = find_closest_point(x, y, coords)) %>%
  ungroup()

result %>%
  group_by(closest) %>%
  summarise(infinite = any(on_edge),
            area = n()) %>%
  filter(!infinite) %>%
  arrange(desc(area)) %>%
  slice(1) %>%
  pull(area)


# Day 6 part 1 --------------------------------------------------------------------------------
result2 <- grid %>%
  rowwise() %>%
  mutate(total_dist = sum(abs(x - coords$x)) + sum(abs(y - coords$y)))

result2 %>%
  filter(total_dist < 10000) %>%
  nrow()
