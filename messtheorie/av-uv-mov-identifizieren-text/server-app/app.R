# Task Name: av-uv-mov-identifizieren-text ######################
library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(learnr)
library(shinycssloaders)

## UI #########################################################################
ui <- page_fixed(
  useShinyjs(),
  card(
    card(
      card_header(
        "Aufgabe: Kausalzusammenhang",
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
  
  # Global functions ###########################################################
  ## round2 rounds .5 upwards
  round2 = function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^n
    z*posneg
  }

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
  variables <- reactive({
    tibble::tribble(
    ~Topic,                               ~MV,                             ~IV,                               ~DV,
    1L,           "Kognitive Fähigkeiten",            "Gewissenhaftigkeit", "Schulische Leistungsentwicklung",
    2L,                           "Alter",             "Menge an Freizeit",         "Nutzung sozialer Medien",
    3L,     "Motivation der Studierenden",       "Anspruch eines Seminars",          "Bewertung des Seminars",
    4L,                           "Alter",           "Verbale Intelligenz",              "Gedächtnisleistung",
    5L,                     "Straßenlärm", "Dosierung eines Schlafmittels",                     "Schlafdauer",
    6L, "Einsatz von Entspannungstechnik",         "Aufgewendete Lernzeit",                      "Lernerfolg",
    7L,                   "Alkoholkonsum",         "Aufgewendete Lernzeit",                      "Lernerfolg",
    8L,           "Soziale Unterstützung",     "Individueller Stresslevel",                  "Lebensqualität"#,
    #9L,                      "Geschlecht",              "Arbeitserfahrung",                          "Gehalt"
  )[c(1, sample(2:8,7)),] # reshuffle tasks except the first
  })
  
  # manually set the topic to be used in the task
  topic_manually_set <- reactive({
    (as.numeric(input$new_task) %% (9 - 1)) + 1
  })
  
  # set modality of solution manually
  solution_mode_manually_set <- "verbally"
  
  # (random) sequence of the mentioning of the three variables within the topic 
  mv_iv_dv_seq <- 1:3 #sample(1:3,3)
  
  # generate the question string to alternate wordings 
  question_string_part1 <- 
    reactive({
    paste("Eine Forscherin interessiert sich für die Variablen »",
          variables() %>% 
            slice(., topic_manually_set()) %>% 
            pull(1 + mv_iv_dv_seq[1]),
          " (",
          substr(
            variables() %>% 
              slice(., topic_manually_set()) %>% 
              pull(1 + mv_iv_dv_seq[1]), 
            1,2),
          ")«, »",
          variables() %>% 
            slice(., topic_manually_set()) %>% 
            pull(1 + mv_iv_dv_seq[2]),
          " (",
          substr(
            variables() %>% 
              slice(., topic_manually_set()) %>% 
              pull(1 + mv_iv_dv_seq[2]), 
            1,2),
          ")« und »",
          variables() %>% 
            slice(., topic_manually_set()) %>% 
            pull(1 + mv_iv_dv_seq[3]),
          " (",
          substr(
            variables() %>% 
              slice(., topic_manually_set()) %>% 
              pull(1 + mv_iv_dv_seq[3]), 
            1,2),
          ")«. Sie erhebt die entsprechenden Daten bei einer repräsentativen Stichprobe von ",
          sample(501:7654, 1),
          " SchülerInnen.", 
          sep = ""
    )
      })
  
  question_string_part2 <- reactive({
    sample(c(
      paste(
        " Sie vermutet dabei folgendes: Je niedriger die Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(MV),
        "« ausgeprägt ist, desto größer ist der Effekt der Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(IV),
        "« auf die Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(DV),
        "«.",
        sep = ""
      ),
      paste(
        " Sie vermutet dabei, dass der Effekt der Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(IV),
        "« auf die Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(DV),
        "« umso kleiner ist, je kleiner die Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(MV),
        "« ausgeprägt ist.",
        sep = ""
      ),
      paste(
        " Sie vermutet dabei, dass der Effekt der Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(IV),
        "« auf die Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(DV),
        "« von der Ausprägung der Variable »",
        variables() %>% slice(.,topic_manually_set()) %>% pull(MV),
        "« beinflusst wird.",
        sep = ""
      )),
      1
    )
  })
  
  ## generating verbal solution
  
  list_of_answers <- reactive({
    c(
      paste(
        "Abhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(DV),
        "|| Unabhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(IV),
        "|| Moderatorvariable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(MV)
      ),
      paste(
        "Abhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(DV),
        "|| Unabhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(MV),
        "|| Moderatorvariable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(IV)
      ),
      paste(
        "Abhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(IV),
        "|| Unabhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(DV),
        "|| Moderatorvariable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(MV)
      ),
      paste(
        "Abhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(IV),
        "|| Unabhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(MV),
        "|| Moderatorvariable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(DV)
      ),
      paste(
        "Abhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(MV),
        "|| Unabhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(IV),
        "|| Moderatorvariable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(DV)
      ),
      paste(
        "Abhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(MV),
        "|| Unabhängige Variable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(DV),
        "|| Moderatorvariable = ",
        variables() %>% slice(., topic_manually_set()) %>% pull(IV)
      )
    )
  })
  
  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    radioButtons(
      "answers_task",
      "Bitte ankreuzen",
      list_of_answers()[answersequence()],
      selected = character(0),
      width = "100%"
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste(question_string_part1(), 
          question_string_part2(),
          "<br>Welche der folgenden Zuordnungen entspricht dem vermuteten Kausalzusammenhang?</b><br>")
  }) 
  
  ## Correct answers ###
  
  # String to resort answers
  
  answersequence <- reactive({
    input$new_task
    sample(1:6, 6)
  })
  
  correct_answers_task <- reactive({
    
    list_of_answers()[1]
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

  
  
  ## Debug #############################################################
  
  output$debug <- renderPrint({
    class(question_string_part1())
  })
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
