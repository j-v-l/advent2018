library(pacman)
p_load(tidyverse, lubridate, janitor)

dat <- read.delim("data/day04_input.txt", header = F, as.is = T)


## Day 4 part 1 -------------------------------------------------------------------------------

# Guards for each day
guard_days <- dat %>%
  filter(grepl("begins shift", V1)) %>%
  extract(V1, c("shift_start", "guard"), "\\[(.*)\\] Guard #(\\d*)") %>%
  mutate(
    shift_start = ymd_hm(shift_start),
    shift_date = case_when(
      hour(shift_start) == 23 ~ as_date(shift_start) + days(1),
      T ~ as_date(shift_start)
    )
  )

# Sleeping/waking events
sleep_events <- dat %>%
  filter(!grepl("begins shift", V1)) %>%
  extract(V1, c("date", "minute", "event"), "\\[(\\S*)\\s(.*)]\\s(.*)") %>%
  mutate(
    date = ymd(date),
    minute = as.numeric(word(minute, 2, sep = ":"))
  ) %>%
  arrange(date, minute)

# Merge into one data frame and clean
df <- sleep_events %>%
  left_join(guard_days, by = c("date" = "shift_date")) %>%
  select(-shift_start) %>%
  mutate(nap_id = rep(1:(nrow(.)/2), each = 2)) %>%
  spread(event, minute) %>%
  clean_names() %>%
  mutate(time_asleep = wakes_up - falls_asleep)

# Find the ID of the guard who's asleep the most
sleepiest_guard_id <- df %>%
  group_by(guard) %>%
  summarise(total_time_asleep = sum(time_asleep)) %>%
  arrange(desc(total_time_asleep)) %>%
  slice(1) %>%
  pull(guard)

# Figure out which minute that guard is asleep the most
sleepiest_guard_df <- df %>%
  filter(guard == sleepiest_guard_id)

result <- set_names(integer(60), 0:59)
for (i in seq_along(sleepiest_guard_df$nap_id)) {
  min_start <- sleepiest_guard_df$falls_asleep[i]
  min_end <- sleepiest_guard_df$wakes_up[i]
  index_to_increment <- (min_start + 1):min_end
  result[index_to_increment] <- result[index_to_increment] + 1
}
sleepiest_guard_minute <- names(result)[which.max(result)]

# Final answer
as.numeric(sleepiest_guard_id) * as.numeric(sleepiest_guard_minute)


## Day 4 part 2 --------------------------------------------------------------------------------

guard_ids <- unique(df$guard)
result_all <- set_names(vector("list", length(guard_ids)), guard_ids)
for (i in seq_along(result_all)) {
  current_guard_id <- guard_ids[i]
  current_guard_df <- filter(df, guard == current_guard_id)
  result <- set_names(integer(60), 0:59)
  for (j in seq_along(current_guard_df$nap_id)) {
    min_start <- current_guard_df$falls_asleep[j]
    min_end <- current_guard_df$wakes_up[j]
    index_to_increment <- (min_start + 1):min_end
    result[index_to_increment] <- result[index_to_increment] + 1
  }
  result_all[[i]] <- result
}

# incomplete
maxes <- map_int(result_all, function(x) max(as.integer(x)))
result_all[[which.max(maxes)]]
