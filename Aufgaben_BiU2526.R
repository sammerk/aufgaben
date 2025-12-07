library(exams)

exams2moodle(
  list(
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_01.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_02.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_03.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_04.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_05.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_06.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_07.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_08.Rmd",
    "methodologie/erkenntnisinteresse/exams/Deskriptive_Explorative_Explanative_Studien_09.Rmd"),
  n = 1,
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  converter = "pandoc-mathjax",
  iname = T,
  name = "Erkenntnisinteresse"
)


exams2moodle(
  list(
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_01.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_02.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_03.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_04.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_05.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_06.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_07.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_08.Rmd",
    "methodologie/experimentelles-design/exams/Exp_Quasiexp_Nichtexp_Studien_09.Rmd"),
  n = 1,
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  converter = "pandoc-mathjax",
  iname = T,
  name = "Experimentelles Design"
)
