# Task Name: dem-design-interne-validitaet-zuordnen #######################
library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(tidyr)
library(learnr)
library(shinycssloaders)

## UI #########################################################################
ui <- page_fixed(
  useShinyjs(),
  card(
  #  verbatimTextOutput("debug"),
  card(
      card_header(
          "Aufgabe: Bringen Sie die drei Studien in eine Rangfolge bzgl. ihrer internen Validität",
          class = "bg-dark"),
      card_body(
      uiOutput("prompt_task")
      )
    ),
  
  # komplett kopierbar für SC Aufgaben
  shinyjs::hidden(card(id = "feedbackpanel_task",
      card_header(
          "Feedback",
          class = "bg-dark"),
      card_body(
          withSpinner(
                                htmlOutput("feedback_task"),
                                proxy.height = "50px",
                                color = "#8cd000"))
          )
      ),
    card(
      uiOutput("ui_answers_task"),
      actionButton("show_feedback_task", 
                   "Prüfe meine Lösung!",
                   icon = icon("microscope")),
      actionButton("reshuffle_task", 
                   "Diese Aufgabe wiederholen",
                   icon = icon("repeat")),
      actionButton("new_task", 
                   "Neue Aufgabe derselben Art",
                   icon = icon("plus-circle"))
    )      
))



server <- function(input, output, session) {
  

  pausen_nonexp <- "Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden die Studierenden eines Seminars während ihrer Lernzeit beobachtet. Es wird registriert, wie viel Zeit sie für das Lernen der Inhalte aufwenden und wie viele Lernpausen sie machen. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet."
  
  pausen_quasiexp <- "Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden Studentinnen und Studenten eines Seminars miteinander verglichen. Die Studentinnen werden aufgefordert nach jeweils 30 Minuten Lernzeit eine Pause von 5 Minuten einzulegen. Die Studenten sollen die Zeit ohne Unterbrechung zum Lernen nutzen. Zusätzlich wird die Lernzeit erfasst. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet."
  
  pausen_exp <- "Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden die Studierenden in einem Seminar zufällig zwei Gruppen zugeteilt. Die Gesamtlernzeit wird für beide Gruppen auf 3 h begrenzt. Gruppe 1 soll nach jeweils 30 Minuten eine Pause von 5 Minuten machen, Gruppe 2 soll die Zeit ohne Unterbrechung zum Lernen nutzen. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet."
  
  anker_nonexp <- "Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Dazu werden einer repräsentativen Stichprobe von Deutschlehrkräften die gleichen fünf Deutschaufsätze samt Erstkorrektur mit der Bitte um Zweitkorrektur vorgelegt. Anschließend beantworten die Lehrkräfte in einem standardisierten Fragebogen, ob sie bei ihrer Beurteilung von der Erstkorrektur beeinflusst wurden."
  
  anker_exp <- "Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Dazu werden Lehrer:innen zufällig in zwei Experimentalgruppen eingeteilt. Eine Gruppe erhält Deutschaufsätze mit einer angeblich guten Erstkorrekturnote (Note 2). Die andere Gruppe erhält dieselben Deutschaufsätze jedoch unter Angabe einer schlechten Erstkorrekturnote (Note 5). Es wird untersucht, ob eine bessere Note für Arbeiten mit der angeblichen guten Erstkorrekturnote als mit der angeblich schlechten Erstkorrekturnote vergeben werden."
  
  anker_quasiexp <- "Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Der Einfachheit halber bilden die Lehrkräfte an Schule A die eine Experimentalgruppe und die Lehrkräfte an Schule B die zweite Experimentalgruppe. Eine Gruppe erhält Deutschaufsätze mit einer angeblich guten Erstkorrekturnote (Note 2). Die andere Gruppe erhält dieselben Deutschaufsätze jedoch unter Angabe einer schlechten Erstkorrekturnote (Note 5). Es wird untersucht, ob eine bessere Note für Arbeiten mit der angeblichen guten Erstkorrekturnote als mit der angeblich schlechten Erstkorrekturnote vergeben werden."
  
  stress_nonexp <- "Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu werden umfangreiche Fragebögen zu Persönlichkeitsmerkmalen, Wissen zu Stressbewältigungsstrategien sowie eigenem Stressempfinden bei Lehrkräften mit und ohne vorherige Burnout-Diagnose miteinander verglichen."
  
  stress_exp <- "Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu werden aus einem Pool interessierter Lehrkräfte zufällig 30 Personen ausgewählt und zu einem Stressbewältigungstraining eingeladen. Die nicht eingeladenen Lehrkräfte wurden als Kontrollgruppe genutzt. Im Training wird umfangreiches Wissen zur Entstehung von Stress und zu Stressbewältigungsstrategien angeboten und in kleineren Gruppenübungen vertieft. Längsschnittlich werden die Teilnehmenden zu ihrem eigenen Stressempfinden im Beruf befragt und mit der Kontrollstichprobe verglichen."
  
  stress_quasiexp <- "Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu wird auf einer Fortbildung zum Thema Burnout für alle Teilnehmenden ein Stressbewältigungstraining in Kleingruppen durchgeführt. Die Kontrollstichprobe wird auf einer Fortbildung zum Thema Inklusion für die Studie ausgewählt. Längsschnittlich werden die Teilnehmenden anschließend zu ihrem eigenen Stressempfinden im Beruf befragt und mit der Kontrollstichprobe verglichen."
  
pausen_nonexp <- "Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden die Studierenden eines Seminars während ihrer Lernzeit beobachtet. Es wird registriert, wie viel Zeit sie für das Lernen der Inhalte aufwenden und wie viele Lernpausen sie machen. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet."

pausen_quasiexp <- "Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden Studentinnen und Studenten eines Seminars miteinander verglichen. Die Studentinnen werden aufgefordert nach jeweils 30 Minuten Lernzeit eine Pause von 5 Minuten einzulegen. Die Studenten sollen die Zeit ohne Unterbrechung zum Lernen nutzen. Zusätzlich wird die Lernzeit erfasst. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet."

pausen_exp <- "Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden die Studierenden in einem Seminar zufällig zwei Gruppen zugeteilt. Die Gesamtlernzeit wird für beide Gruppen auf 3 h begrenzt. Gruppe 1 soll nach jeweils 30 Minuten eine Pause von 5 Minuten machen, Gruppe 2 soll die Zeit ohne Unterbrechung zum Lernen nutzen. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet."

anker_nonexp <- "Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Dazu werden einer repräsentativen Stichprobe von Deutschlehrkräften die gleichen fünf Deutschaufsätze samt Erstkorrektur mit der Bitte um Zweitkorrektur vorgelegt. Anschließend beantworten die Lehrkräfte in einem standardisierten Fragebogen, ob sie bei ihrer Beurteilung von der Erstkorrektur beeinflusst wurden."

anker_exp <- "Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Dazu werden Lehrer:innen zufällig in zwei Experimentalgruppen eingeteilt. Eine Gruppe erhält Deutschaufsätze mit einer angeblich guten Erstkorrekturnote (Note 2). Die andere Gruppe erhält dieselben Deutschaufsätze jedoch unter Angabe einer schlechten Erstkorrekturnote (Note 5). Es wird untersucht, ob eine bessere Note für Arbeiten mit der angeblichen guten Erstkorrekturnote als mit der angeblich schlechten Erstkorrekturnote vergeben werden."

anker_quasiexp <- "Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Der Einfachheit halber bilden die Lehrkräfte an Schule A die eine Experimentalgruppe und die Lehrkräfte an Schule B die zweite Experimentalgruppe. Eine Gruppe erhält Deutschaufsätze mit einer angeblich guten Erstkorrekturnote (Note 2). Die andere Gruppe erhält dieselben Deutschaufsätze jedoch unter Angabe einer schlechten Erstkorrekturnote (Note 5). Es wird untersucht, ob eine bessere Note für Arbeiten mit der angeblichen guten Erstkorrekturnote als mit der angeblich schlechten Erstkorrekturnote vergeben werden."

stress_nonexp <- "Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu werden umfangreiche Fragebögen zu Persönlichkeitsmerkmalen, Wissen zu Stressbewältigungsstrategien sowie eigenem Stressempfinden bei Lehrkräften mit und ohne vorherige Burnout-Diagnose miteinander verglichen."

stress_exp <- "Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu werden aus einem Pool interessierter Lehrkräfte zufällig 30 Personen ausgewählt und zu einem Stressbewältigungstraining eingeladen. Die nicht eingeladenen Lehrkräfte wurden als Kontrollgruppe genutzt. Im Training wird umfangreiches Wissen zur Entstehung von Stress und zu Stressbewältigungsstrategien angeboten und in kleineren Gruppenübungen vertieft. Längsschnittlich werden die Teilnehmenden zu ihrem eigenen Stressempfinden im Beruf befragt und mit der Kontrollstichprobe verglichen."

stress_quasiexp <- "Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu wird auf einer Fortbildung zum Thema Burnout für alle Teilnehmenden ein Stressbewältigungstraining in Kleingruppen durchgeführt. Die Kontrollstichprobe wird auf einer Fortbildung zum Thema Inklusion für die Studie ausgewählt. Längsschnittlich werden die Teilnehmenden anschließend zu ihrem eigenen Stressempfinden im Beruf befragt und mit der Kontrollstichprobe verglichen."

## shuffle topic
topic_order <- sample(1:3, 3)

data <- 
  tibble(design = c("exp", "quasi-exp", "non-exp"),
         pausen = c(pausen_exp, pausen_quasiexp, pausen_nonexp),
         anker = c(anker_exp, anker_quasiexp, anker_nonexp),
         stress = c(stress_exp, stress_quasiexp, stress_nonexp)) %>% 
  pivot_longer(names_to = "topic", values_to = "abstracts",
               cols = -design) %>% 
  group_by(topic) %>% 
  # randomize order of designs
  sample_frac(1) %>% 
  ungroup() %>% 
  # shuffel topic
  mutate(topic_numeric = case_when(topic == "pausen" ~ topic_order[1],
                                   topic == "stress" ~ topic_order[2],
                                   topic == "anker" ~ topic_order[3]))
  
   ## Select task 
  nth_task <- reactive({
    # the inputs start with 0 but the topic numering also
    modulo <- as.numeric(input$new_task) %% (nrow(data)/3)
    return(modulo + 1)
                         
  })
  
  
  ## Render UI for Answers ###
  answers <- c("Studie A = höchste interne Validität & Studie B = niedrigste interne Validität",
               "Studie A = höchste interne Validität & Studie C = niedrigste interne Validität",
               "Studie B = höchste interne Validität & Studie A = niedrigste interne Validität",
               "Studie B = höchste interne Validität & Studie C = niedrigste interne Validität",
               "Studie C = höchste interne Validität & Studie A = niedrigste interne Validität",
               "Studie C = höchste interne Validität & Studie B = niedrigste interne Validität")
  
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    radioButtons(
      "answers_task",
      "Bitte ankreuzen",
      answers,
      selected = character(0),
      width = "100%"
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderUI({
    HTML(
      paste0(
      "<b>Studie A:</b>",
      data %>% filter(topic_numeric == nth_task()) %>% pull(abstracts) %>% .[1],
      "<b>Studie B:</b> ",
      data %>% filter(topic_numeric == nth_task()) %>% pull(abstracts) %>% .[2],
      "<b>Studie C:</b> ",
      data %>% filter(topic_numeric == nth_task()) %>% pull(abstracts) %>% .[3],
      "<br><b>Welche Studie hat die höchste, welche die niedrigste interne Validität?</b>",
      sep = "")
    )
  }) 
  
  ## Correct answers ###
  correct_answers_task <- reactive({
    correct_answers_task <- 
      case_when(identical(data %>% 
                            filter(topic_numeric == nth_task()) %>% 
                            pull(design),
                          c("exp", "non-exp", "quasi-exp")) ~ 
                  c(T,F,F,F,F,F),
                identical(data %>% 
                            filter(topic_numeric == nth_task()) %>% 
                            pull(design),
                          c("exp", "quasi-exp", "non-exp")) ~ 
                  c(F,T,F,F,F,F),
                identical(data %>% 
                            filter(topic_numeric == nth_task()) %>% 
                            pull(design),
                          c("non-exp", "exp", "quasi-exp")) ~ 
                  c(F,F,T,F,F,F),
                identical(data %>% 
                            filter(topic_numeric == nth_task()) %>% 
                            pull(design),
                          c("quasi-exp", "exp", "non-exp")) ~ 
                  c(F,F,F,T,F,F),
                identical(data %>% 
                            filter(topic_numeric == nth_task()) %>% 
                            pull(design),
                          c("non-exp", "quasi-exp", "exp")) ~ 
                  c(F,F,F,F,T,F),
                identical(data %>% 
                            filter(topic_numeric == nth_task()) %>% 
                            pull(design),
                          c("quasi-exp", "non-exp", "exp")) ~ 
                  c(F,F,F,F,F,T))
    
    return(correct_answers_task)
  })
  
  ## Feedback task  ####
  output$feedback_task <- renderText({   
    
    if(is.null(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                 paste(correct_answers_task(), collapse = ", <br>✓ "),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(identical(
           correct_answers_task(),
           input$answers_task == answers)){
        paste("Richtig!", learnr::random_praise())}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                     answers[correct_answers_task()],
                     "<br><i>",
                     learnr::random_encouragement(),
                     "</i>"))
        }
    }
  })
  
  ## Show and Hide Feedback ####################################################
  ## Show feedback on button click 
  observeEvent(input$show_feedback_task, {
    showElement(id = "feedbackpanel_task")
  })
  
  ## Hide feedback on solution change or new plot type
  observeEvent(c(input$answers_task, input$new_task), {
    hideElement(id = "feedbackpanel_task")
  })
  
  ## Reset answer on new plot type
  observeEvent(c(input$reshuffle_task, input$new_task), {
    reset(id = "answers_task")
  })
  
  ## Debugging ###################
 output$debug <- renderText( nth_task())
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
