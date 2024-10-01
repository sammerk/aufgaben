# Task Name: Experiment_Quasiexperiment_Nichtexperiment ######################

library(shiny)
library(shinyjs)
library(tidyverse)
library(learnr)
library(bslib)
library(shinycssloaders)



## UI #########################################################################
ui <- page_fixed(
  useShinyjs(),
  card(
    card(
      card_header(
        "Aufgabe: Untersuchungsdesign in Studienbeschreibung erkennen",
        class = "bg-dark"),
    card_body(
      htmlOutput("prompt_task")
    )
    ),
    
    
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

  ##############################################################################
  # Backend for task  ##########################################################
  ##############################################################################
  
  # The global logic is to create a tibble containing  
  #      - answers & distractors (column 1)
  #      - questions (headers of columns 2 - m)
  #      - correct solutions (columns 2 - m without headers)
  #
  # Then 
  #     - reshuffle columns (without the first) to randomize order of questions
  #     - reshuffle rows to randomize order of answers & distractors
  
  
  ## Parameter solution matrix for task  ####
  q_a_matrix_qshuffeled <- 
    tibble(
      Answers_and_Distractors = c("Eine experimentelle Studie",
                                  "Eine quasi-experimentelle Studie",
                                  "Eine nicht-experimentelle Studie"),
      `Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden die Studierenden eines Seminars während ihrer Lernzeit beobachtet. Es wird registriert, wie viel Zeit sie für das Lernen der Inhalte aufwenden und wie viele Lernpausen sie machen. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet.` =  c(F,F,T), # adjust line 132 if coded 0/1 #1
      `Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden Studentinnen und Studenten eines Seminars miteinander verglichen. Die Studentinnen werden aufgefordert nach jeweils 30 Minuten Lernzeit eine Pause von 5 Minuten einzulegen. Die Studenten sollen die Zeit ohne Unterbrechung zum Lernen nutzen. Zusätzlich wird die Lernzeit erfasst. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet.` =  c(F,T,F), #2
      `Es soll untersucht werden, welchen Zusammenhang es zwischen Pausen beim Lernen und der Lernleistung gibt. Dazu werden die Studierenden in einem Seminar zufällig in zwei Gruppen zugeteilt. Die Gesamtlernzeit wird für beide Gruppen auf 3 h begrenzt. Gruppe 1 soll nach jeweils 30 Minuten eine Pause von 5 Minuten machen, Gruppe 2 soll die Zeit ohne Unterbrechung zum Lernen nutzen. Die Lernleistung wird anhand der erreichten Punktzahl in einer Abschlussklausur bewertet.` =  c(T,F,F),#3
      `Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Dazu werden in einer repräsentative Stichprobe von Deutschlehrkräften die gleichen fünf Deutschaufsätze samt Erstkorrektur mit der Bitte um Zweitkorrektur vorgelegt. Anschließend beantworten die Lehrkräfte in einem standardisierten Fragebogen, ob sie bei der Beurteilung von der Erstkorrektur beeinflusst wurden.` =  c(F,F,T), #4
      `Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Dazu werden Lehrer*innen zufällig in zwei Experimentalgruppen eingeteilt. Eine Gruppe erhält Deutschaufsätze mit einer angeblich guten Erstkorrekturnote (Note 2). Die andere Gruppe erhält dieselben Deutschaufsätze jedoch unter Angabe einer schlechten Erstkorrekturnote (Note 5). Es wird untersucht, ob eine bessere Note für Arbeiten mit der angeblichen guten Erstkorrekturnote als mit der angeblich schlechten Erstkorrekturnote vergeben werden.` =  c(T,F,F),#5
      `Der Ankereffekt tritt ein, wenn Menschen mit neuen Informationen (z. B. Zahlenwerten) konfrontiert werden, die sie unbewusst als Anker nutzen, um daran ihre eigene Einschätzung zu orientieren. Es soll untersucht werden, ob die Zweitkorrektur von Abituraufsätzen im Fach Deutsch durch den Ankereffekt verfälscht wird, wenn die Erstkorrekturnote bekannt ist. Der Einfachheit halber bilden die Lehrkräfte an Schule A die eine Experimentalgruppe und die Lehrkräfte an Schule B die zweite Experimentalgruppe. Eine Gruppe erhält Deutschaufsätze mit einer angeblich guten Erstkorrekturnote (Note 2). Die andere Gruppe erhält dieselben Deutschaufsätze jedoch unter Angabe einer schlechten Erstkorrekturnote (Note 5). Es wird untersucht, ob eine bessere Note für Arbeiten mit der angeblichen guten Erstkorrekturnote als mit der angeblich schlechten Erstkorrekturnote vergeben werden.` =  c(F,T,F),#6
      `Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu werden umfangreiche Fragebögen zu Persönlichkeitsmerkmalen, Wissen zu Stressbewältigungsstrategien sowie eigenem Stressempfinden bei Lehrkräften mit und ohne vorherige Burnout-Diagnose miteinander verglichen.` =  c(F,F,T),#7
      `Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu werden aus einem Pool interessierter Lehrkräfte zufällig 30 Personen ausgewählt und zu einem Stressbewältigungstraining eingeladen. Die nicht eingeladenen Lehrkräfte wurden als Kontrollgruppe genutzt. Beim Training wird umfangreiches Wissen zur Entstehung von Stress und zu Stressbewältigungsstrategien angeboten und in kleineren Gruppenübungen vertieft. Längsschnittlich werden die Teilnehmenden zu ihrem eigenen Stressempfinden im Beruf befragt und mit der Kontrollstichprobe verglichen.` =  c(T,F,F),#8
      `Es sollen genauere Erkenntnisse darüber gewonnen werden, wie sich die Stressbewältigung bei Lehrkräften verbessern lässt. Dazu wird auf einer Fortbildung zum Thema Burnout für alle Teilnehmenden ein Stressbewältigungstraining in Kleingruppen durchgeführt. Die Kontrollstichprobe wurde auf einer Fortbildung für Inklusion für die Studie ausgewählt. Längsschnittlich werden die Teilnehmenden anschließend zu ihrem eigenen Stressempfinden im Beruf befragt und mit der Kontrollstichprobe verglichen.` =  c(F,T,F)) %>% #9
    # shuffle order of questions
    relocate(1, 2, sample(3:ncol(.), ncol(.) - 2))
  
  ## Number of 
  
  ## Shuffle answers
  q_a_matrix_qashuffeled <- 
    reactive({
      input$reshuffle_task # answer shuffelling induced by both buttons
      input$new_task
      
      q_a_matrix_qshuffeled %>%
        sample_frac(., size = 1)
    })
  
  ## Select task 
  nth_task <- reactive({
    # starting with 1
    (as.numeric(input$new_task) %% (ncol(q_a_matrix_qshuffeled) - 1)) + 1
  })
  
  
  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    radioButtons(
      "answers_task",
      "Bitte ankreuzen",
      q_a_matrix_qashuffeled() %>% 
        select(1, nth_task() + 1) %>% 
        na.omit(.) %>% 
        pull(Answers_and_Distractors),
      selected = character(0)
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste(names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()],
          "<br><b>Welches Untersuchungsdesign liegt vor?</b>")
  }) 
  
  ## Correct answers ###
  correct_answers_task <- reactive({
    
    vector_of_correct_answers <- 
      q_a_matrix_qashuffeled() %>% 
      pull(nth_task() + 1)
    
    correct_answers_task <- 
      q_a_matrix_qashuffeled() %>%
      filter(vector_of_correct_answers == T) %>%
      pull(Answers_and_Distractors)
    
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
      if(setequal(correct_answers_task(), input$answers_task)){
        paste("Richtig!", learnr::random_praise())}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                     paste(correct_answers_task(), collapse = ", <br>✓ "),
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

}



# Create Shiny object
shinyApp(ui = ui, server = server)
