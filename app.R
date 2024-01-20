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
library(shinyalert)
library(shinydashboard)
library(shinyFiles)
library("rlist")
library(spsComps)
library(ASCAT.sc)

path <- getwd()
result <- "result_manualfitting_projectGSE89648.Rda"
res <- NULL
resnew <- NULL
pathrdata <- NULL
rdata <- NULL
coords <- NULL

#load(paste0(path, "/www/result_object_projectGSE89648.Rda"))
options(spinner.color="#f06313", spinner.color.background="#ffffff", spinner.size=1)

if (!is.null(pathrdata)){load(pathrdata)}

getIndex <- function(sample){
  
  index <- which(names(res$allTracks.processed)==sample)
  return (index)
  
}

getSamples <- function() {
  
  if( !is.null(res)){
  #return (names(res$allTracks.processed)[-c(6, 9, 10)])}
    return (names(res$allTracks.processed))}
  
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
                            timeout = 3000,
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
            }",
                "pre {
                  display:block;
                  padding:9.5px;
                  margin: auto;
                  width: 350px;
                  height: 100px;
                  font-size:14px;
                  color: #833e03;
                    line-height:5px;
                  word-break:break-all;
                  word-wrap:break-word;
                  white-space:pre-wrap;
                  background-color:#fae6d4;
                    border:4px solid #833e03;
                  border-radius:4px; 
                }",
                "em {
                  display:block;
                  padding:9.5px;
                  margin: auto;
                  width: 800px;
                  height: 95px;
                  font-size:14px;
                  color: #833e03;
                    line-height:5px;
                  word-break:break-all;
                  word-wrap:break-word;
                  white-space:pre-wrap;
                  background-color:#fae6d4;
                    border:4px solid #833e03;
                  border-radius:4px; 
                }"
                                            ))),
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
                 
  
  ######################### MODIFIER TAB ################################
  #######################################################################
  
  tabPanel("Modifier",  shinyWidgets::useShinydashboard(),
                fluidRow(box(width=12, title="Original", status="warning", solidHeader=TRUE, 
                              column(width=8,withSpinner(plotOutput("profile"),type=3)),
                              column(width=4,plotOutput("sunrise1")))
                  
                   #div(id = "image-container", style = "display:flexbox"),
                   #verbatimTextOutput("info")
                 ),
                 useShinyjs(),
                 useShinyalert(),
           fluidRow(column(width=2, 
                   dropdownButton(
                     tags$h3("Choose Sample"),
                     br(),
                  selectInput(
                   "samples",
                   label= NULL,
                   choices= getSamples(),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = FALSE
                 ), size= "lg", circle= FALSE, status = "info", icon = icon("list",verify_fa = FALSE), width = "450px",
                label= "Select sample",
                 actionButton("view", label = "View", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 2px")
                 
                   )
                 ),
                 column(width=2,
                        dropdownButton(tags$h3("Modify the copy number of 2 segments"),br(),
                        selectInput(
                   "Chr1",
                   "Choose first chromosome",
                   c(1:22, "X", "Y"),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = FALSE,
                   width = "75%"
                   
                 ),
                 textInput("cn1", "Choose first copy number", value = "", placeholder = NULL, width = "75%"),
                 selectInput(
                   "Chr2",
                   "Choose second chromosome",
                   c(1:22, "X", "Y"),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = FALSE,
                   width = "75%"
                 ),
                 textInput("cn2", "Choose second copy number", value = "",  placeholder = NULL, width = "75%"),
                 actionButton("modify", label = "Apply", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"),
                 size= "lg", circle= FALSE, status = "info", label = "Modify copy number", width = "450px") ),
                 
                 column(width = 2, 
                        dropdownButton(
                        sliderInput("ploidy", "Shift ploidy by:", -3, 4, 1, step = 1, round = FALSE,
                                               ticks = TRUE, animate = FALSE,
                                               width = NULL, sep = ",", pre = NULL, post = NULL),
                        actionButton("shift", label = "Apply", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"),
                        size= "lg", circle= FALSE, status = "info", label = "Shift sample ploidy", width = "450px"
                        )
                        #radioButtons("ploidy", "Shift ploidy by:",c("-1" = -1, "1"= 1), selected = 1)
                 ),
                
                column(width = 2, 
                       dropdownButton(tags$h3("Choose ploidy and purity on the sunrise graph"),
                                      h4("To change the ploidy and purity of the whole sample directly on the Working station profile, please click the point on the sunrise plot corresponding to the desired ploidy/purity values, then click on 'Apply'"),
                                      actionButton("sunrise", label = "Apply", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"),
                                      size= "lg", circle= FALSE, status = "info", label = "Modify purity & ploidy", width = "450px"
                       )
                       #radioButtons("ploidy", "Shift ploidy by:",c("-1" = -1, "1"= 1), selected = 1)
                ),
                
                column(width = 2, 
                       dropdownButton(tags$h3("Shift ploidy on graph"),
                                      h4("To shift the ploidy of the whole sample directly on the Working station profile, please click on a segment, then on its desired y axis position, then click on 'Apply'"),
                                      actionButton("shift_graph", label = "Apply", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"),
                                      size= "lg", circle= FALSE, status = "info", label = "Shift ploidy on graph", width = "450px"
                       )
                       #radioButtons("ploidy", "Shift ploidy by:",c("-1" = -1, "1"= 1), selected = 1)
                ),
                
                column(width = 2,
                       dropdownButton(tags$h3("Modify segment on graph"),
                          h4("To modify the copy number of 2 different segments directly on the Working station profile, please click on the segment you wish to modify, then on its desired position. Repeat for the second segment, then click on 'Apply'"),
                         actionButton("refit", label = "Apply", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"),
                         size= "lg", circle= FALSE, status = "info", right= TRUE, label = "Refit segment on graph", width = "450px"
                       )
                       #radioButtons("ploidy", "Shift ploidy by:",c("-1" = -1, "1"= 1), selected = 1)
                )
                
                
                ), br(),
            
    
                fluidRow(box(width=12, title="Modifier Working Station", status="warning", solidHeader=TRUE, column(width=8, withSpinner(plotOutput("profile2", click="profile2_click"),type=3)), column(width = 4, plotOutput("sunrise2", click = "sunrise2_click")))),
                 fluidRow(column(width = 3,  offset=1, actionButton("discard", label = "Reset",  icon = icon("arrows-rotate",verify_fa = FALSE), style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")),
                          #column(width = 2, actionButton("sunrise", label = "Sunrise", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")),
                          
                          #column(width = 3,  actionButton("refit", label = "Refit", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")),
                          column(width = 3, offset=1,  downloadButton("savetxt", label = "Save txt", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px")),
                                                    column(width = 3, offset=1, downloadButton("save", label = "Save .Rda", style="color: #FFFFFF ; background-color: #ba4a00; border-color: #ba4a00; font-size:100%; border-width: 3px"))), br(), br()))
 
                  
server <- function(input, output, session) {
  vals <- reactiveVal()
  volumes = getVolumes()
  coords <- reactiveValues(x=NULL,y=NULL)
  
  observeEvent(input$profile2_click, {
    coords$x <- c(coords$x,input$profile2_click$x)
    coords$y <- c(coords$y,input$profile2_click$y)
  })
  
  observeEvent(input$sunrise2_click, {
    coords$x <- input$sunrise2_click$x
    coords$y <- input$sunrise2_click$y
  })
  
  sampleName <- reactive({
    input$samples
  })
  
  result <- reactive({ res })
  
  shiftv <- reactive({
    input$ploidy
  })
  
  chrs <- reactive({
    list(input$Chr1,input$Chr2, input$cn1, input$cn2)
  })
  output$profile <- renderPlot(NULL)
  output$profile2 <- renderPlot(NULL)
  
  
  # observe({  
  #   shinyFileChoose(input, "get_file", roots = volumes, session = session, fileTypes=c('Rda', 'rda'))
  #   
  #   if(!is.null(input$get_file)){
  #     # browser()
  #     file_selected<-parseFilePaths(volumes, input$get_file)
  #     #output$txt_file <- renderText(as.character(file_selected$datapath))
  #   }
  # })
  # 
  ###################### INPUT MODAL ############################################
  ###############################################################################
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      
      # shinyFilesButton("get_file", "Choose the ASCAT.sc rdata object to load" ,
      #                  title = "Please select a file:", multiple = FALSE,
      #                  buttonType = "default", class = NULL),
      # 
      
      
      textInput("rdata", "Choose ASCAT.sc rdata object to load",
                placeholder = 'Please provide the absolute path'),

      if (failed)
        {div(tags$b("Invalid name or path of data object", style = "color: red;"))},
      
      footer = tagList(
        #modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  observeEvent(input$ok, {
    tryCatch({
      
      # ShinyFileChoose(input, "get_file", roots = volumes, session = session)
      # 
      # if(!is.null(input$get_file)){
      #   # browser()
      #   file_selected<-parseFilePaths(volumes, input$get_file)
      #   #output$txt_file <- renderText(as.character(file_selected$datapath))
      # }
      # 
    pathrdata <<- input$rdata
    load(pathrdata)
    res <<- res
    updateSelectInput(session, "samples", label=NULL,
                      choices=getSamples())
    
    output$profile <- renderPlot({isolate(plot1 <- plotSolution(res$allTracks.processed[[1]],
                                                                purity=res$allSolutions.refitted.auto[[1]]$purity,
                                                                ploidy=res$allSolutions.refitted.auto[[1]]$ploidy,
                                                                ismale=if(!is.null(res$sex)) res$sex[[1]]=="male" else "female",
                                                                gamma=1,
                                                                sol=res$allSolutions[[1]]))
    })
    
    output$profile2 <- renderPlot({isolate(plot2 <- plotSolution(res$allTracks.processed[[1]],
                                                                 purity=res$allSolutions.refitted.auto[[1]]$purity,
                                                                 ploidy=res$allSolutions.refitted.auto[[1]]$ploidy,
                                                                 ismale=if(!is.null(res$sex)) res$sex[[1]]=="male" else "female",
                                                                 gamma=1,
                                                                 sol=res$allSolutions[[1]]))
    })
    
    output$sunrise1 <- renderPlot({isolate(plotSunrise(res$allSolutions.refitted.auto[[1]]))})
    output$sunrise2 <- renderPlot({isolate(plotSunrise(res$allSolutions.refitted.auto[[1]]))})
    
    removeModal()
      },
    error=function(e) {
      showModal(dataModal(failed = TRUE))
    })
    })
  
  observe({
    if (input$nav_page == "Modifier")  {
      showModal(dataModal())
    }
  })
  
  ###################################################################################
  ###################################################################################
  
  observeEvent(input$start, {
     updateNavbarPage(session=session,
                     inputId="nav_page",
                     selected="Modifier")
    # shinyalert("Load Rdata", "Please provide the name of the ASCAT.sc Rdata object on your device that you'd like to modify (with absolute path)", type = "input", inputId = "pathRdata")
    # if (! is.null(input$pathRdata)){
    # load(input$pathRdata)}
  })
  
  observeEvent(input$discard, {
    
    # output$profile2 <- renderPlot(NULL)
    # output$sunrise2 <- renderPlot(NULL)
    # 
    vals(input$samples)
    
    if(! is.null(sampleName())){
      index <- getIndex(sampleName())
     
      output$profile2 <- renderPlot({isolate(plotSolution(res$allTracks.processed[[index]],
                                                          purity=res$allSolutions.refitted.auto[[index]]$purity,
                                                          ploidy=res$allSolutions.refitted.auto[[index]]$ploidy,
                                                          ismale=if(!is.null(res$sex)) res$sex[[index]]=="male" else "female",
                                                          gamma=1,
                                                          sol=res$allSolutions[[index]]))})
      
      output$sunrise2 <- renderPlot({isolate(plotSunrise(res$allSolutions.refitted.auto[[index]]))})
      
    }
    else{
      return (NULL)
    }
    
  })
  
  output$save <- downloadHandler(
    filename = function() {
      "result_manualfitting.Rda"
    },
    content = function(file) {
      #resnew <- result()
      showModal(modalDialog(div(tags$b("Loading...", style = "color: steelblue;")), footer=NULL))
      on.exit(removeModal())
      
      res <- resnew
      save(res, file=file)
      
    }
  )
  
  # observe({ toggle(id="discard", condition=(input$view>=1))})
  # observe({ toggle(id="save", condition=(input$view>=1))})
  # #observe({ toggle(id="refit", condition=(input$view>=1))})
  # observe({ toggle(id="savetxt", condition=(input$view>=1))})
  # # observe({ toggle(id="samples", condition=(input$view>=1))})
  # observe({ toggle(id="Chr1", condition=(input$view>=1))})
  # observe({ toggle(id="Chr2", condition=(input$view>=1))})
  # observe({ toggle(id="cn1", condition=(input$view>=1))})
  # observe({ toggle(id="cn2", condition=(input$view>=1))})
  # observe({ toggle(id="ploidy", condition=(input$view>=1))})
  # observe({ toggle(id="modify", condition=(input$view>=1))})
  # observe({ toggle(id="shift", condition=(input$view>=1))})
  
  observeEvent(input$sunrise,{
    vals <- as.numeric(chrs())
    
    
    if(! is.null(chrs())){
      index <- getIndex(sampleName())
      input$sunrise2_click
      purity <- NULL
      ploidy <- NULL
      
      # print(coords$y)
      # print(coords$x)
      # coords$x <- NULL
      # coords$y <- NULL
      # print(coords$x)
      #seg_index <- which(res$allProfiles.refitted.auto[[index]])
      
      tryCatch(
        {
          
          #x1 <- (coords$x[1] - breaks[chr1]) * 1e+06
          #y1 <- coords$y[1]
          #x2 <- coords$x[2]
          solution <- res$allSolutions[[1]]
          errs <- solution$errs
          errs <- errs-min(errs)
          errs.max <- max(solution$errs[!is.infinite(solution$errs)])
          errs[is.infinite(errs)] <- errs.max
          errs <- errs/errs.max
          #print(errs)
          
          # purity <- round(coords$y, digits=1)
          # ploidy <- round(coords$x, digits=1)
          # 
          purity <- coords$y
          ploidy <- coords$x
          
          #ploidy <- 0.3884719
          #purity <- 0.1318182
          ploidy <- as.numeric(colnames(errs)[as.numeric(ploidy)*ncol(errs)])
          purity <- as.numeric(rownames(errs)[(1-as.numeric(purity))*nrow(errs)])
          
          print(purity)
          print(ploidy)
          # print(which(colnames(errs)==solution$ploidy)/ncol(errs))
          # print(1-which(rownames(errs)==solution$purity)/nrow(errs))
          # 
          # print(which.min(abs(errs-ploidy)))
          # print(errs[which.min(abs(errs-purity)),])
          # print(errs[which.min(abs(errs-ploidy)),])
          # 
          # print(colnames(errs)[ploidy*ncol(errs)])
          # print(rownames(errs)[1-purity*nrow(errs)])
          # print(1-purity*nrow(errs))
          # # print((1-purity)*(nrow(solution$errs)))
          # print(ploidy*(ncol(solution$errs)))
          # print(chr1)
          # print(chr2)
          # print(y2)
          # print(y4)
          resnew <<- res
          resnew$allProfiles.refitted.auto[[index]] <<- getProfile(fitProfile(tracksSingle = resnew$allTracks.processed[[index]], purity, ploidy, ismale=res$sex[index]=="male"))
         
          resnew$allProfiles[[index]] <<- resnew$allProfiles.refitted.auto[[index]]
        
          resnew$allSolutions[[index]]$ploidy <<- ploidy
        
          resnew$allSolutions[[index]]$purity <<- purity
         
          resnew$allSolutions.refitted.auto[[index]]$ploidy <<- ploidy
         
          resnew$allSolutions.refitted.auto[[index]]$purity <<- purity
          
         
          # preds <-  try(predictRefit(res$allProfiles[[index]]))
          # print(preds)
          # if(is.numeric(preds) & length(preds)==1)
          # {
          #   if(preds!=1)
          #   {
          #     resnew$allSolutions.refitted.auto[[index]] <- try(refitProfile_shift(track=resnew$allTracks.processed[[index]],
          #                                                                   solution=resnew$allSolutions[[index]],
          #                                                                   CHRS=res$chr,
          #                                                                   shift=if(preds==0) 1 else -1,
          #                                                                   isPON=resnew$isPON),silent=F)
          #     resnew$allProfiles.refitted.auto[[index]] <- try(getProfile(fitProfile(resnew$allTracks.processed[[index]],
          #                                                                     purity=purity,
          #                                                                     ploidy=ploidy,
          #                                                                     ismale=if(resnew$sex[index]=="male") T else F),
          #                                                          CHRS=resnew$chr),silent=F)
          #     print("here2")
          #   }
          #   if(preds==1)
          #   {
          #     resnew$allSolutions.refitted.auto[[index]] <- resnew$allSolutions[[index]]
          #     resnew$allProfiles.refitted.auto[[index]] <- getProfile(fitProfile(resnew$allTracks.processed[[index]],
          #                                                                     purity=purity,
          #                                                                     ploidy=ploidy,
          #                                                                     ismale=if(resnew$sex[index]=="male") T else F),
          #                                                          CHRS=resnew$chr)
          #     print("here3")
          #   }
          # }
          output$profile2 <- renderPlot({isolate(plotSolution(resnew$allTracks.processed[[index]],
                                                              purity=purity,
                                                              ploidy=ploidy,
                                                              gamma=.55))})
          
          output$sunrise2 <- renderPlot({isolate(plotSunrise(resnew$allSolutions.refitted.auto[[index]]))})
          coords$y <<- NULL
          coords$x <<- NULL
          #shinyCatch({message("New solution is ambiguous: reverted to old one")}, prefix = '')
        },
        error=function(e) {
          message('An Error Occurred')
          print(e)
          shinyalert("Error", "Cannot fit profile: ploidy<0 or purity ∉ [0,1]. Please choose different values", type = "error")
        },
        warning=function(w) {
          message('A Warning Occurred')
          print(w)
          shinyalert("Warning", "New solution is ambiguous: reverted to old one", type = "error")
        })
    }
    else{
      return (NULL)
    }
    
  })
  
  
  observeEvent(input$refit,{
    vals <- as.numeric(chrs())
    
    
    if(! is.null(chrs())){
      index <- getIndex(sampleName())
      input$profile2_click
      chr1 <- NULL
      chr2 <- NULL
      breaks <- c(0, cumsum(sapply(res$allTracks.processed[[1]]$lSegs, function(x) max(x$output$loc.end))/1e+06))
      #print(breaks)
      
      # print(coords$y)
      # print(coords$x)
      # coords$x <- NULL
      # coords$y <- NULL
      # print(coords$x)
      #seg_index <- which(res$allProfiles.refitted.auto[[index]])
      
      tryCatch(
        {
          for (i in 1:length(breaks)){
            if (coords$x[1] < breaks[i]) {
              chr1 <- i-1
              break} 
           
          }
          for (i in 1:length(breaks)){
            
            if (coords$x[3] < breaks[i]) {
              chr2 <- i-1 
              break} 
          }
          #x1 <- (coords$x[1] - breaks[chr1]) * 1e+06
          #y1 <- coords$y[1]
          #x2 <- coords$x[2]
          y2 <- round(coords$y[2], digits=0)
          y4 <- round(coords$y[4], digits=0)
          print(chr1)
          print(chr2)
          print(y2)
          print(y4)
          resnew <<- run_any_refitProfile(res,
                                          sample_indice=index,
                                          chr1=chr1,
                                          ind1=NA,
                                          n1=y2,
                                          chr2=chr2,
                                          ind2=NA,
                                          n2=y4,
                                          CHRS=c(1:22,"X","Y"),
                                          outdir="./www",
                                          gridpur=seq(-.05,.05,.01),
                                          gridpl=seq(-.1,.2,.01))
          
          output$profile2 <- renderPlot({isolate(plotSolution(resnew$allTracks.processed[[index]],
                                                              purity=resnew$allSolutions.refitted.manual[[index]]$purity,
                                                              ploidy=resnew$allSolutions.refitted.manual[[index]]$ploidy,
                                                              gamma=.55))})
          
          output$sunrise2 <- renderPlot({isolate(plotSunrise(resnew$allSolutions.refitted.manual[[index]]))})
          
          #shinyCatch({message("New solution is ambiguous: reverted to old one")}, prefix = '')
        },
        error=function(e) {
          message('An Error Occurred')
          print(e)
          shinyalert("Error", "Cannot fit profile: ploidy<0 or purity ∉ [0,1]. Please choose different values", type = "error")
        },
        warning=function(w) {
          message('A Warning Occurred')
          print(w)
          shinyalert("Warning", "New solution is ambiguous: reverted to old one", type = "error")
        })
    }
    else{
      return (NULL)
    }
    
  })
  
  
  
  observeEvent(input$modify,{
    vals <- as.numeric(chrs())
    
    
    if(! is.null(chrs())){
      index <- getIndex(sampleName())
      tryCatch(
      {
      resnew <<- run_any_refitProfile(res,
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
      
      output$profile2 <- renderPlot({isolate(plotSolution(resnew$allTracks.processed[[index]],
                                                          purity=resnew$allSolutions.refitted.manual[[index]]$purity,
                                                          ploidy=resnew$allSolutions.refitted.manual[[index]]$ploidy,
                                                          gamma=.55))})
      
      output$sunrise2 <- renderPlot({isolate(plotSunrise(resnew$allSolutions.refitted.manual[[index]]))})
      
        },
      error=function(e) {
        message('An Error Occurred')
        print(e)
        shinyalert("Error", "Cannot fit profile: ploidy<0 or purity ∉ [0,1]. Please choose different values", type = "error")
      },
      warning=function(w) {
        message('A Warning Occurred')
        print(w)
        shinyalert("Warning", "New solution is ambiguous: reverted to old one", type = "error")
      })
    }
    else{
      return (NULL)
    }
    
  })
  
  
  
  observeEvent(input$shift,{
    vals <- as.numeric(chrs())
    
   
    if(! is.null(chrs())){
      index <- getIndex(sampleName())
      shiftp <- as.numeric(shiftv())
      print(shiftp)
      
      tryCatch(
        {
          resnew <<- run_any_refitProfile_shift(res,
                                           sample_indice=index,
                                           shift=shiftp,
                                           CHRS=c(1:22,"X","Y"),
                                           outdir="./www",
                                           gridpur=seq(-.05,.05,.01),
                                           gridpl=seq(-.1,.2,.01))
          
          output$profile2 <- renderPlot({isolate(plotSolution(resnew$allTracks.processed[[index]],
                                                              purity=resnew$allSolutions.refitted.manual[[index]]$purity,
                                                              ploidy=resnew$allSolutions.refitted.manual[[index]]$ploidy,
                                                              gamma=.55))})
          
          output$sunrise2 <- renderPlot({isolate(plotSunrise(resnew$allSolutions.refitted.manual[[index]]))})
          #shinyCatch({message("New solution is ambiguous: reverted to old one")}, prefix = '')
          
          #print(res$allSolutions.refitted.manual[[index]])
        },
        error=function(e) {
          message('An Error Occurred')
          print(e)
          shinyalert("Error", "Cannot fit profile. Please choose different values", type = "error")
        },
        warning=function(w) {
          message('A Warning Occurred')
          print(w)
          shinyalert("Warning", "New solution is ambiguous: reverted to old one", type = "error")
        }
      )
      
    }
    else{
      return (NULL)
    }
    
  })
  
  observeEvent(input$view,{
    vals(input$samples)
    
    if(! is.null(sampleName())){
      index <- getIndex(sampleName())
      output$profile <- renderPlot({isolate(plot1 <- plotSolution(res$allTracks.processed[[index]],
                                                         purity=res$allSolutions.refitted.auto[[index]]$purity,
                                                         ploidy=res$allSolutions.refitted.auto[[index]]$ploidy,
                                                         ismale=if(!is.null(res$sex)) res$sex[[index]]=="male" else "female",
                                                         gamma=1,
                                                         sol=res$allSolutions[[index]]))
        })
      
      output$profile2 <- renderPlot({isolate(plot2 <- plotSolution(res$allTracks.processed[[index]],
                                                         purity=res$allSolutions.refitted.auto[[index]]$purity,
                                                         ploidy=res$allSolutions.refitted.auto[[index]]$ploidy,
                                                         ismale=if(!is.null(res$sex)) res$sex[[index]]=="male" else "female",
                                                         gamma=1,
                                                         sol=res$allSolutions[[index]]))
        })
      
      output$sunrise1 <- renderPlot({isolate(plotSunrise(res$allSolutions.refitted.auto[[index]]))})
      output$sunrise2 <- renderPlot({isolate(plotSunrise(res$allSolutions.refitted.auto[[index]]))})
      #shinyCatch({message("New solution is ambiguous: reverted to old one")}, prefix = '')
      
    }
    else{
      return (NULL)
    }
    
  })
  
  
  
} 

shinyApp(ui = ui, server = server)
