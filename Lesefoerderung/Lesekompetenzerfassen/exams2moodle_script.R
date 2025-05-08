library(exams)

exams2moodle(
  c(here::here("Lesefoerderung/Studienleistung/Studienleistung_exams.Rmd")),
  name = "Teilstudienleistung_Merk",
  mchoice = list(eval = exams_eval(rule = "true")),
  schoice = list(eval = exams_eval(rule = "none")),
  points = 10,
  n = 30,
  converter = "pandoc-mathjax"
)


