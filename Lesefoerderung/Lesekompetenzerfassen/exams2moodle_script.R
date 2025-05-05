library(exams)

exams2moodle(
  c("regressiontothemean_lesemeister/regressiontothemean_lesemeister.Rmd",
    "wissenschaftsverstaendnis/wissenschaftsverstaendnis.Rmd"),
  name = "xml_export_uebung_mit_flo",
  mchoice = list(eval = exams_eval(rule = "true")),
  schoice = list(eval = exams_eval(rule = "none")),
  points = 10,
  n = 10,
  converter = "pandoc-mathjax"
)


