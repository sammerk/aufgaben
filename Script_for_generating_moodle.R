# Script for generating a .xml file for the integration in moodle
library(exams)
library(exams2forms)
library(here)


# Messtheorie ----
## Teilklausur ----
exams2moodle(
  c("messtheorie/av-uv-mov-identifizieren-dag/exams/av-uv-mov-identifizieren-dag.Rmd",
    "messtheorie/bezugsnormen-erkennen/exams/bezugsnormen-erkennen.Rmd",
    "messtheorie/reverse-causality/exams/reverse-causality.Rmd",
    "verteilungen/Streuung_in_Kompetenzstufen_erkennen/exams/Streuung_in_Kompetenzstufen_erkennen.Rmd",
    "verteilungen/Mean_median_Q1_IQR_sd_aus_grafik_abschaetzen/exams/Mean_median_Q1_IQR_sd_aus_grafik_abschaetzen.Rmd"
  ),
  name = NULL,
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 10,
  converter = "pandoc-mathjax"
)
