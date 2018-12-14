library(pacman)
p_load(tidyverse, lubridate, janitor)

dat <- read.delim("data/day05_input.txt", header = F, as.is = T)
input <- dat$V1


# Day 5 part 1 --------------------------------------------------------------------------------
collapse_polymer <- function(polymer, pattern = "([A-Za-z])(?!\\1)(?i:\\1)") {
  any_reactions <- str_detect(polymer, pattern)
  while (any_reactions) {
    polymer <- str_replace_all(polymer, pattern, "")
    any_reactions <- str_detect(polymer, pattern)
  }
  polymer
}

input %>%
  collapse_polymer() %>%
  nchar()


# Day 5 part 2 --------------------------------------------------------------------------------
remove_unit <- function(polymer, unit_type) {
  str_replace_all(polymer, paste0("(?i)", unit_type), "")
}

result <- map_int(letters, function(x) nchar(collapse_polymer(remove_unit(input, x))))
min(result)