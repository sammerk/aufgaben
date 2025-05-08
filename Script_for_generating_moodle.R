# Script generating a .xml file for moodle
library(exams)
library(exams2forms)
library(here)

# Methodologie ----
## Teilklausur ----
exams2moodle(
  c("regressiontothemean_lesemeister/regressiontothemean_lesemeister.Rmd",
    "methodologie/steigerung-interne-validitaet/exams/steigerung-interne-validitaet.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_r.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_r.Rmd",
    "methodologie/steigerung-externe-validitaet/exams/steigerung-externe-validitaet.Rmd"
    ),
  name = "Teilklausur_methodologie",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")),
  # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 10,
  converter = "pandoc-mathjax"
)

