# Task Name: Messung_als_Homomorphismus ######################
library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(learnr)
library(shinycssloaders)
library(babynames)

## UI #########################################################################
ui <- page_fixed(
  useShinyjs(),
    card(
      card(
        card_header(
          "Aufgabe: Homomorphismus",
          class = "bg-dark"),
        card_body(
          htmlOutput("prompt_task"),
          tableOutput("table")
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
  

  ##############################################################################
  # Backend for task  ##########################################################
  ##############################################################################
  
  av <- reactive({
    input$new_task
    sample(c("des Geschlechts", 
             "der Grundschulempfehlung"#, 
            # "der Klausurleistung"
             ), 
           1)
  })
  
  homomorphism_or_not <- reactive({
    input$new_task
    sample(c("homomorphism", "not_a_homomorphism"), 1)
    })
  
  output$table <- renderTable({
    
       ## Code for av() == "der Grundschulempfehlung"
    if(av() == "der Grundschulempfehlung"){
      list_of_distinct_codes <- sample(c(-100:100, 
                                         c(-100:100 + runif(201))), 
                                       prob = c(rep(1,201), rep(.1,201)),
                                       size =1000,
                                       replace = T) %>% 
        round(., 1) %>% 
        unique(.)
      code_Werkrealschule <- sample(list_of_distinct_codes[list_of_distinct_codes < 75], 1) 
      code_Realschule <- code_Werkrealschule + sample(1:12, 1)
      code_Gymnasium <- code_Realschule + sample(1:12, 1)
      code_falsch <- sample(list_of_distinct_codes[list_of_distinct_codes > code_Realschule], 1) #später wird werkrea falsch
      
      data <- 
        babynames %>% 
        filter(prop >.01) %>% 
        slice_sample(n= 4) %>% 
        select(name, sex) %>% 
        mutate(`SchülerIn` = name,
               Grundschulempfehlung = sample(c("Werkrealschule", "Realschule", "Gymnasium"),
                                             4, replace = T)) %>% 
        left_join(.,
                  tibble(Grundschulempfehlung = c("Werkrealschule", "Realschule", "Gymnasium"),
                         Messwert = c(code_Werkrealschule, code_Realschule, code_Gymnasium))
        )%>% 
        select(SchülerIn, Grundschulempfehlung, Messwert)
      
      if(homomorphism_or_not() == "not_a_homomorphism"){
        data$Messwert[1] <- code_falsch
        data <- 
          data %>% 
          slice_sample(n= 4)
      }
      
    }
    
    ## Code for av() == "der Klausurleistung"
    if(av() == "der Klausurleistung"){
      vorkommende_messwerte <- sample(1:15, 4, replace = T)
      potentielle_falsche_Messwerte <- -15:min(vorkommende_messwerte)
      
      data <- 
        babynames %>% 
        filter(prop >.01) %>% 
        slice_sample(n= 4) %>% 
        select(name, sex) %>% 
        mutate(`SchülerIn` = name,
               `Notenpunkte (=Messwert)` = sample(1:15, 4, replace = T),
               Verrechnungspunkte = `Notenpunkte (=Messwert)`* sample(2:5, 1))%>%
        select(SchülerIn, Verrechnungspunkte, `Notenpunkte (=Messwert)`)
      
      if(homomorphism_or_not() == "not_a_homomorphism"){
        data$`Notenpunkte (=Messwert)`[1] <- sample(potentielle_falsche_Messwerte, 1)
        data <- 
          data %>% 
          slice_sample(n= 4)
      }
    }
    
    ## Code for av() == "des Geschlechts"
    if(av() == "des Geschlechts"){
      list_of_distinct_codes <- sample(c(-100:100, 
                                         c(-100:100 + runif(201))), 
                                       prob = c(rep(1,201), rep(.1,201)),
                                       size =1000,
                                       replace = T) %>% 
        round(., 1) %>% 
        unique(.)
      code_m <- sample(list_of_distinct_codes, 1) 
      code_w <- sample(setdiff(list_of_distinct_codes, code_m), 1)
      code_d <- sample(setdiff(list_of_distinct_codes, c(code_m, code_w)), 1)
      
      data <- 
        babynames %>% 
        filter(prop >.01 & sex == "F") %>% 
        slice_sample(n = 2) %>% 
        full_join(babynames %>% 
                    filter(prop >.01 & sex == "M") %>% 
                    slice_sample(n = 2)) |> 
        select(name, sex) %>% 
        mutate(`SchülerIn` = name,
               Geschlecht = ifelse(sex == "F", "weiblich", "männlich")) %>% 
        left_join(.,
                  tibble(Geschlecht = c("weiblich", "männlich"),
                         Messwert = c(code_w, code_m))) %>% 
       select(SchülerIn, Geschlecht, Messwert)
      
      if(homomorphism_or_not() == "not_a_homomorphism"){
        data$Messwert[1] <- code_d
        data <- 
          data %>% 
          slice_sample(n= 4)
      }
      
    }
    
    return(data)
  })
  
  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    radioButtons(
      "answers_task",
      "Bitte ankreuzen",
      c("ja", "nein"),
      selected = character(0)
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste("<b>Stellt die folgende Tabelle eine Messung im Sinne eines Homomorphismus dar?</b>")
  }) 
  
  ## Correct answers ###
  
  
  correct_answers_task <- reactive({
    ifelse(homomorphism_or_not() == "homomorphism", "ja", "nein")
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
   
  })
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
