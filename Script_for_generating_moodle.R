# Script for generating a .xml file for the integration in moodle
library(exams)
library(exams2forms)
library(here)


# Messtheorie ----
## Teilklausur ----
exams2moodle(
  c("messtheorie/skalenniveau-erkennen/exams/skalenniveau-erkennen.Rmd",
  ),
  name = "teilklausur_skalenniveau-erkennen",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("messtheorie/av-uv-mov-identifizieren-dag/exams/av-uv-mov-identifizieren-dag.Rmd"
  ),
  name = "teilklausur_av-uv-mov-identifizieren-dag",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("messtheorie/guetekriterien-unterschied-erkennen/Unterschied_Guetekrit_erkennen_r.Rmd"
  ),
  name = "teilklausur_unterschiede-guetekriterien-erkennen",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("messtheorie/bezugsnorm-unterschiede-erkennen/bezugsnorm-unterschiede-erkennen.Rmd"
  ),
  name = "teilklausur_bezugsnormen-unterschiede-erkennen",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("verteilungen/Mean-Median-berechnen/Mean_Median_berechnen.Rmd"
  ),
  name = "teilklausur_mean-median-berechnen",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)


# Effektstärken ----
## Teilklausur ----
exams2moodle(
  c("korrelationen/Visual_Guessing_r_Partial_Credit/exams/Visual_Guessing_r_Partial_Credit_new.Rmd"
  ),
  name = "teilklausur_visual_guessing_r",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("korrelationen/taub_berechnen/taub_berechnen.Rmd"
  ),
  name = "teilklausur_taub_berechnen",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("gruppenunterschiede/definitions-U1-d-d-taub-r/definitions-U1-d-d-taub-r.Rmd"
  ),
  name = "teilklausur_definitions-U1-d-d-taub-r",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

exams2moodle(
  c("gruppenunterschiede/guessing-cohen-d/exams/guessing-cohen-d.Rmd"
  ),
  name = "teilklausur_guessing-cohen-d",
  dir = "_Moodle-Import-Files",
  mchoice = list(eval = exams_eval(rule = "true")),
  #"true" uses 1/ncorrect (so that each wrong selection cancels one correct selection);
  schoice = list(eval = exams_eval(rule = "none")), # sets minimum for schoice to zero
  points = 10, # points for each exercise?
  n = 9,
  converter = "pandoc-mathjax"
)

