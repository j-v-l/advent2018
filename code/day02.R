library(pacman)
p_load(tidyverse)

dat <- read.table("data/day02_input.txt", as.is = T)
codes <- dat$V1


# Day 2 part 1 --------------------------------------------------------------------------------
codes_split <- str_split(codes, pattern = "")
twos <- map_int(codes_split, function(x) any(table(x) == 2))
threes <- map_int(codes_split, function(x) any(table(x) == 3))

sum(twos) * sum(threes)


# Day 2 part 2 --------------------------------------------------------------------------------
for (i in 1:nchar(codes[1])) {
  codes_short <- map_chr(codes_split, function(x) paste(x[-i], collapse = ""))
  if (any(duplicated(codes_short))) {
    print(codes_short[duplicated(codes_short)])
  }
}