library(shiny)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(shinyanimate)
library(devtools)
library(shinycssloaders)
library(shinydlplot)
library(periscope)
library(shinyjs)
library(gridExtra)
library(ggplot2)
library(stringr)
library("rlist")
#source("plotSol.R")
library(ASCAT.sc)

path <- getwd()
load(paste0(path, "/www/result_object_projectGSE89648.Rda"))
options(spinner.color="#f06313", spinner.color.background="#ffffff", spinner.size=1)


getIndex <- function(sample){
  
  index <- which(names(res$allTracks.processed)==sample)
  return (index)
  
}
getSamples <- function() {
  
  #load(paste0(path, "/www/result_object_projectGSE206842.Rda"))
  return (names(res$allTracks.processed))
  
}

ui <- navbarPage(id="nav_page",
 
  title="ASCAT.scFit",  theme = shinytheme("united"),
                 
                 
                 tabPanel("Welcome",
                          busy_start_up(
                            loader = tags$img(
                              src = "Research.gif",
                              width = 400
                            ),
                            text = "Loading ...",
                            color = "#ba4a00",
                            timeout = 2500,
                            background = "white",
                            mode = "auto"
                          ),
                          tags$head(
                            tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin: auto;
                width: 1600px;
                height: 450px;
                font-size:14px;
                color: #833e03;
                line-height:5px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#fae6d4;
                border:4px solid #833e03;
                border-radius:4px; 
            }"))),
                 div(style = "height:70px"), h1(strong("ASCAT.sc Ploidy Modifier",style={'color: #ba4a00; font-family: Georgia ,serif; text-shadow:
  # # 0 0 7px #fff,
  # # 0 0 10px #fff,
  # # 0 0 21px #fff,
  # # 0 0 42px ##ba4a00,
  # # 0 0 82px ##ba4a00;  font-size: 70px'}), align="center"), br(),  fluidRow(column(6, offset=3, align="center", img(src = "edit.png", height = 50, width = 50))), br(),
                 
                 code(h2(strong("Manual refitting for ASCAT.sc profiles"), align="center", style={'color: #833e03; font-family: Georgia'}), 
                      h4(strong("Please choose the sample that you would like to view and modify. Two types of modifications are possible.", style={'font-family: Arial'}), align="center"),
                      h4(strong("You can either change the ploidy of the whole sample (decrease/increase by 1), or modify the copy number of 2 different segments and refit the whole profile based on those.", style={'font-family: Arial'}),align="center"),
                      h4(strong("If you are happy with the result, you can save the new profile. Otherwise, you can discard it and start again.", style={'font-family: Arial'}), align="center")),
                     br(), fluidRow(column(6, align="center", offset = 3,
                                 actionButton("start",
                                              label="Start",
                                              style="color: #FFFFFF ; background-color: #833e03; border-color: #833e03; padding:40px; font-size:100%; border-width: 3px")))),
                 tabPanel("Modifier", fluidRow(column(width=2, selectInput(
                   "samples",
                   "Choose sample",
                   getSamples(),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = FALSE,
                 )),
                 column(width=2, offset=1, selectInput(
                   "Chr1",
                   "Choose first chromosome",
                   c(1:22, "X", "Y"),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = FALSE,
                
                 )), column(width=2,  textInput("cn1", "Choose first copy number", value = "", width = NULL, placeholder = NULL)),
                 column(width = 2, selectInput(
                   "Chr2",
                   "Choose second chromosome",
                   c(1:22, "X", "Y"),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = FALSE,
                 )),
                 column(width = 2, textInput("cn2", "Choose second copy number", value = "", width = NULL, placeholder = NULL),
                 ),
                 column(width = 1, radioButtons("ploidy", "OR Shift sample ploidy by:",
                                                c("-1" = -1, "1"= 1)),
                 )
                 ),
                 fluidRow(column(width=2, actionButton("view", label = "View", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")
                 ),
                 column(width = 2, offset = 4, actionButton("modify", label = "Modify copy number", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")),
                 column(width = 2,  offset=2, actionButton("shift", label = "Shift ploidy", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"))),
  
                 fluidRow(
                   withSpinner(plotOutput("profile", brush = "plot_brush", click = "plot_click"),type=3),
                   div(id = "image-container", style = "display:flexbox"),
                   verbatimTextOutput("info")
                 ),
                 useShinyjs(),
                 fluidRow(withSpinner(plotOutput("profile2"),type=3)),
                 fluidRow(column(width = 6, offset=3,actionButton("discard", label = "Discard", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")),
                          column(width = 3, actionButton("save", label = "Save", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")))))
                 
server <- function(input, output, session) {
  vals <- reactiveVal()
  sampleName <- reactive({
    input$samples
  })
  
  shiftv <- reactive({
    input$ploidy
  })
  
  chrs <- reactive({
    list(input$Chr1,input$Chr2, input$cn1, input$cn2)
  })
  output$profile <- renderPlot(NULL)
  output$profile2 <- renderPlot(NULL)
  
  observeEvent(input$start, {
    updateNavbarPage(session=session,
                     inputId="nav_page",
                     selected="Modifier")
  })
  
  observeEvent(input$discard, {
    
    output$profile2 <- renderPlot(NULL)
    
  })
  
  observeEvent(input$save, {
    
    output$profile2 <- renderPlot(NULL)
    output$profile <- renderPlot(NULL)
    
  })
  
   # include useShinyjs() somewhere in UI
  #server
  observe({ toggle(id="discard", condition=(input$modify>=1)||(input$shift>=1))})
  observe({ toggle(id="save", condition=(input$modify>=1)||(input$shift>=1))})
  
  observeEvent(input$modify,{
    vals <- as.numeric(chrs())
    
    #load(paste0(path, "/www/result_object_projectGSE206842.Rda"))
    if(! is.null(chrs())){
      index <- getIndex(sampleName())
      
      res <- run_any_refitProfile(res,
                                  sample_indice=index,
                                  chr1=vals[[1]],
                                  ind1=NA,
                                  n1=vals[[3]],
                                  chr2=vals[[2]],
                                  ind2=NA,
                                  n2=vals[[4]],
                                  CHRS=c(1:22,"X","Y"),
                                  outdir="./www",
                                  gridpur=seq(-.05,.05,.01),
                                  gridpl=seq(-.1,.2,.01))
      
      output$profile2 <- renderPlot({isolate(plotSolution(res$allTracks.processed[[index]],
                                                          purity=res$allSolutions.refitted.manual[[index]]$purity,
                                                          ploidy=res$allSolutions.refitted.manual[[index]]$ploidy,
                                                          gamma=.55))})
      
    }
    else{
      return (NULL)
    }
    
  })
  
  
  
  observeEvent(input$shift,{
    vals <- as.numeric(chrs())
    
    #load(paste0(path, "/www/result_object_projectGSE206842.Rda"))
    if(! is.null(chrs())){
      index <- getIndex(sampleName())
      shiftp <- as.numeric(shiftv())
      print(shiftp)
      res <- run_any_refitProfile_shift(res,
                                        sample_indice=index,
                                        shift=shiftp,
                                        CHRS=c(1:22,"X","Y"),
                                        outdir="./www",
                                        gridpur=seq(-.05,.05,.01),
                                        gridpl=seq(-.1,.2,.01))
      
      output$profile2 <- renderPlot({isolate(plotSolution(res$allTracks.processed[[index]],
                                                          purity=res$allSolutions.refitted.manual[[index]]$purity,
                                                          ploidy=res$allSolutions.refitted.manual[[index]]$ploidy,
                                                          gamma=.55))})
      
    }
    else{
      return (NULL)
    }
    
  })
  
  observeEvent(input$view,{
    vals(input$samples)
    
    if(! is.null(sampleName())){
      index <- getIndex(sampleName())
      output$profile <- renderPlot({isolate(plotSolution(res$allTracks.processed[[index]],
                                                         purity=res$allSolutions.refitted.auto[[index]]$purity,
                                                         ploidy=res$allSolutions.refitted.auto[[index]]$ploidy,
                                                         ismale=if(!is.null(res$sex)) res$sex[[index]]=="male" else "female",
                                                         gamma=1,
                                                         sol=res$allSolutions[[index]]))})
      
    }
    else{
      return (NULL)
    }
    
  })
  
  
  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) {return("NULL\n")}
  #     else {paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")}
  #     
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) {return("NULL\n")}
  #     else {paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
  #            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))}
  #   }
  #   
  #   paste0(
  #     "click: ", xy_str(input$plot_click),
  #     "brush: ", xy_range_str(input$plot_brush)
  #   )
  #   
  #   
  # })
  
} 

shinyApp(ui = ui, server = server)
