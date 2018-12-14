library(pacman)
p_load(tidyverse, lubridate, janitor)

dat <- read.delim("data/day07_input.txt", header = F, as.is = T)


# Day 7 part 1 --------------------------------------------------------------------------------
prereqs <- dat %>%
  extract(V1, c("prereq", "step"), "Step ([A-Z]).*step ([A-Z])")

result <- vector("character", 26)
prereqs_left <- prereqs
for (i in 1:26) {
  next_step <- LETTERS[!LETTERS %in% prereqs_left$step & !LETTERS %in% result][1]
  prereqs_left <- prereqs_left %>%
    filter(prereq != next_step)
  result[i] <- next_step
}

paste(result, collapse = "")


# Day 7 part 2 --------------------------------------------------------------------------------
total_time_left <- setNames(60 + 1:26, LETTERS)
total_time_spent <- 0
prereqs_left <- prereqs
completed_steps <- ""

while (any(total_time_left > 0)) {
  # what steps to do and how long the shortest one takes
  to_do <- LETTERS[!LETTERS %in% c(prereqs_left$step, completed_steps)]
  if (length(to_do) > 5) to_do <- to_do[1:5]
  time_to_spend <- min(total_time_left[to_do])
  
  # passage of time
  total_time_left[to_do] <- total_time_left[to_do] - time_to_spend
  total_time_spent <- total_time_spent + time_to_spend
  
  # complete steps and remove them
  completed_steps <- names(which(total_time_left == 0))
  prereqs_left <- prereqs_left %>%
    filter(!prereq %in% completed_steps)
}