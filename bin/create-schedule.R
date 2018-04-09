#' ---
#' title: create weeks
#' ---
#'
#' Create week files in the schedule.
#'
library("tibble")
library("tidyverse")
library("glue")

START_DATE <- as.Date("2018-03-26")
END_DATE <- as.Date("2018-06-01")
OUTDIR <- here::here("content", "schedule")

dates <- tibble(date = seq(START_DATE, END_DATE, by = 1),
                week = as.integer(date - START_DATE) %/% 7L + 1L,
                weekday = weekdays(date),
                tags = list(list())) %>%
  filter(weekday %in% c("Monday", "Wednesday", "Friday")) %>%
  mutate(lab = weekday == "Friday") %>%
  arrange(date) %>%
  group_by(week) %>%
  split(.$week)

for (i in seq_along(dates)) {
  x <- dates[[i]]
  outfile <- file.path(OUTDIR, str_c("week-", i, ".Rmd"))
  print(outfile)

  if (i > 1) {

  list(title = glue("Week {i}"),
       date = format(min(x[['date']]), "%Y-%m-%d"),
       tags = list(),
       week = i,
       meetings = unname(split(mutate(x, date = format(date, "%Y-%m-%d")),
                               seq_len(nrow(x))))) %>%
    yaml::as.yaml() %>% str_c("---\n", ., "---\n\n", sep = "") %>%
    cat(file = outfile)
  }
}

