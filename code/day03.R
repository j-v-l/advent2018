library(pacman)
p_load(tidyverse)

dat <- read_table("data/day03_input.txt", col_names = F)


# Day 3 part 1 --------------------------------------------------------------------------------
claims <- dat %>%
  mutate(X1 = str_squish(gsub("\\D", " ", X1))) %>%
  separate(X1, c("id", "xstart", "ystart", "xsize", "ysize")) %>%
  mutate_at(-1, as.numeric)

xmax <- max(claims$xstart + claims$xsize)
ymax <- max(claims$ystart + claims$ysize)

fabric <- matrix(0, ncol = xmax + 1, nrow = ymax + 1)

add_claim <- function(input, x1, xs, y1, ys) {
  input[y1:(y1 + ys - 1), x1:(x1 + xs - 1)] <- input[y1:(y1 + ys - 1), x1:(x1 + xs - 1)] + 1
  input
}

for (i in 1:nrow(claims)) {
  fabric <- add_claim(fabric, row$xstart[i], row$xsize[i], row$ystart[i], row$ysize[i])
}

sum(fabric > 1)


# Day 3 part 2 --------------------------------------------------------------------------------
for (i in 1:nrow(claims)) {
  row <- claims[i, ]
  x1 <- row$xstart
  xs <- row$xsize
  y1 <- row$ystart
  ys <- row$ysize
  end_state <- fabric[y1:(y1 + ys - 1), x1:(x1 + xs - 1)]
  if (all(end_state == 1)) {
    print(row)
  }
}