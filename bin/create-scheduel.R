library("tidyverse")
library("lubridate")

classdays <-
seq(as.Date("2018-03-26"), as.Date("2018-06-01"), by = 1) %>%
  keep(~ weekdays(.x) %in% c("Monday", "Wednesday", "Friday"))

outpath <- function(x) {
  here::here("content", "schedule", str_c(format(x, "%Y-%m-%d"), ".Rmd"))
}

courseweek <- function(x) {
  as.integer(x - as.Date("2018-03-26")) %/% 7L + 1L
}

skeleton <- function(x) {
  courseweek_ <- courseweek(x)

  frontmatter <-
    str_c("---",
        str_c("lab: ", if (base::weekdays(x) == "Friday") "true" else "false"),
        "draft: false",
        str_c("date: ", format(x, "%Y-%m-%d")),
        "readings: []",
        "optional: []",
        str_c("course_week: ", courseweek(x)),
        "description: \"\"",
        "---",
        "",
        sep = "\n")

  cat(frontmatter, file = outpath(x))

}

walk(classdays, skeleton)

