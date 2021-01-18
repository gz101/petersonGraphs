#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    p {
      color: purple;
      font-style: italic;
    }
    #statustext {
      color: maroon;
      font-size: 20px;
      font-family: monospace;
    }
    #hamiltontext {
      color: orange;
      font-size: 16px;
      font-family: Georgia
    }
  ')
))

#The user interface
header <- dashboardHeader(title = "Petersen's graph",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),
  fluidRow(stylesheet,
    column(width=2,
      tableOutput("vertex"),
      tableOutput("edge")
    ),
    column(width=6,
      plotOutput("plot1", click="plot_click", height=675),
      uiOutput("statustext"),
      uiOutput("hamiltontext"),
      uiOutput("hamiltonsteps")
    ),
    column(width=4,
      h3("Hamiltonian Walk"),
      p("A Hamiltonian Walk is a walk that includes every vertex of the graph 
        once and only once."),
      p("If you get stuck, click this button again anytime to reset your progress."),
      actionBttn("hamiltonbtn", "Hamiltonian Walk"),
      br(),
      h3("Euler Cycle"),
      p("An Eulerian Cycle is a cycle that includes every edge of the graph
        once and only once."),
      p("If you get stuck, click this button again anytime to reset your progress."),
      actionBttn("cyclebtn", "Euler Cycle"),
      br(),
      h3("Choose Vertex Colors"),
      p("You may choose your own color choices for each vertex provided no edge
        joins two vertices of the same color."),
      actionBttn("colorsbtn", "Choose Vertex Colors"),
      hidden(
        div(id="colors",
            radioButtons(
              "choices",
              "Color Choices",
              choiceNames=c("Gray", "Beige", "Pink"),
              choiceValues=c("grey", "beige", "pink")
            )
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green") #other 


source("hamwalk.R")


server <- function(session, input, output) {
  state <- "NONE"
  PeteDF <- makeVertexDF()
  edgeDF <- makeEdgeDF()
  hamDF <- c()
  cycleRoad <- rep(1, 20)
  steps <- ""
  vertcolor <- "yellow" # Don't forget to change hamwalk.R if this is changed
  first <- 0
  
  # Decide what state to enter the program
  observeEvent(input$hamiltonbtn, {
    hide("colors")
    state <<- "HAMWALK"
    initializor()
    output$statustext <- renderText("You are doing a Hamiltonian Walk.")
    output$hamiltontext <- renderText("Pick your first point.")
  })
  
  observeEvent(input$cyclebtn, {
    hide("colors")
    state <<-"HAMCYCLE"
    initializor.cycle()
    output$statustext <- renderText("You are doing an Eulerian Cycle.")
    output$hamiltontext <- renderText("Pick your first point.")
  })
  
  observeEvent(input$colorsbtn, {
    initializor()
    if (state!="COLORS") {
      show("colors")
      state <<- "COLORS"
      output$statustext <- renderText("You are picking different vertex colors.")
      output$hamiltontext <- renderText("")
    } else {
      hide("colors")
      state <<- "NONE"
      output$statustext <- renderText("")
      output$hamiltontext <- renderText("")
    }
  })
  
  # Handle plot clicks
  observeEvent(input$plot_click, {
    if (is.null(state)) return()
    
    #################
    # HAMILTON WALK #
    #################
    if (state == "HAMWALK") {

      vertex <- findClosestVert(PeteDF, input$plot_click$x, input$plot_click$y)
      prev <- PeteDF[hamDF[length(hamDF)],1]
      
      if (length(hamDF)==0) {
        hamDF <<- c(hamDF, vertex)
        PeteDF[vertex, 4] <<- vertcolor
        output$hamiltontext <- renderText(paste0("Pick point no. [",length(hamDF)+1,"]."))
        steps <<- paste0("Selection [",length(hamDF),"] is: [",PeteDF[vertex,1],"].")
        output$hamiltonsteps <- renderUI(steps)
        showTables()
        showPlot()
        
      } else if (!vertex %in% hamDF && checkRoad(PeteDF, edgeDF, vertex, prev)) {
        hamDF <<- c(hamDF, vertex)
        PeteDF[vertex, 4] <<- vertcolor
        roadidx <- roadLookUp(PeteDF, edgeDF, hamDF[length(hamDF)], hamDF[length(hamDF)-1])
        edgeDF[roadidx,3] <<- vertcolor
        showTables()
        showPlot()
        rawstep <- paste0("Selection [",length(hamDF),"] is: [",PeteDF[vertex,1],"].")
        steps <<- paste(steps,rawstep,sep="<br/>")
        output$hamiltonsteps <- renderUI(HTML(steps))
        
        if (length(hamDF)==nrow(PeteDF)-1) {
          if (resolve(PeteDF, edgeDF, vertex)) {
            output$hamiltontext <- renderText("Pick your final point.")
          } else {
            output$hamiltontext <- renderText("You have no valid moves left, please restart to try again!")
            state <<- "NONE"
          }
        }
        else if (length(hamDF)<nrow(PeteDF)) {
          if (resolve(PeteDF, edgeDF, vertex)) {
            output$hamiltontext <- renderText(paste0("Pick point no. [",length(hamDF)+1,"]."))
          } else {
            output$hamiltontext <- renderText("You have no valid moves left, please restart to try again!")
            state <<- "NONE"
          }
        } else {
          output$hamiltontext <- renderText("You are done.")
          state <<- "NONE"
        }
        
      } else {
        if (is.null(state)) return()
        output$hamiltontext <- renderText(paste0("Choose a valid point no. [",
                                                 length(hamDF)+1,"]."))
      }
    }
    
    ##################
    #   EULER CYCLE  #
    ##################
    if (state=="HAMCYCLE") {

      # Find vertex the user clicked
      vertex <- findClosestVert(PeteDF, input$plot_click$x, input$plot_click$y)

      # The previous point
      prev <- PeteDF[hamDF[length(hamDF)],1]
      
      # Road index
      road.idx <- roadLookUp(PeteDF, edgeDF, vertex, hamDF[length(hamDF)])

      # If this is the first point being clicked by the user
      if (length(hamDF)==0) {

        # Update list of visited vertices
        hamDF <<- c(hamDF, vertex)
        
        # Record the first vertex
        first <<- vertex

        # Change color of vertex
        PeteDF[vertex, 4] <<- vertcolor

        # Update steps and instructions
        output$hamiltontext <- renderText(paste0("Pick point no. [",length(hamDF)+1,"]."))
        steps <<- paste0("Selection [",length(hamDF),"] is: [",PeteDF[vertex,1],"].")
        output$hamiltonsteps <- renderUI(steps)
        showTables()
        showPlot()

      # If selected vertex has not been clicked yet and there are valid roads
      } else if (cycleRoad[road.idx]==1 && checkRoad(PeteDF, edgeDF, vertex, prev)) {
        
        # Update list of visited vertices
        hamDF <<- c(hamDF, vertex)

        # Change color of vertex
        PeteDF[vertex, 4] <<- vertcolor

        # Change edge color
        roadidx <- roadLookUp(PeteDF, edgeDF, hamDF[length(hamDF)], hamDF[length(hamDF)-1])
        cycleRoad[roadidx] <<- 2
        
        showTables()
        showPlot()

        # Update steps and instructions
        rawstep <- paste0("Selection [",length(hamDF),"] is: [",PeteDF[vertex,1],"].")
        steps <<- paste(steps,rawstep,sep="<br/>")
        output$hamiltonsteps <- renderUI(HTML(steps))

        # Below this are conditionals for text outputs
        # If this is the final available point to choose
        if (length(which(cycleRoad==1))==1) {

          # Check if there are valid adjacent points/edges
          if (resolve.cycle(PeteDF, edgeDF, vertex, first)) {
            output$hamiltontext <- renderText("Pick your final point.")

          } else {
            output$hamiltontext <- renderText("You have no valid moves left, please restart to try again!")
            state <<- "NONE"
          }

        # There are >1 points left
        } else if (length(which(cycleRoad==1))>1) {

          # Check if there are valid adjacent points/edges
          if (roadsLeft(PeteDF, edgeDF, cycleRoad, vertex)) {
            output$hamiltontext <- renderText(paste0("Pick point no. [",length(hamDF)+1,"]."))

          } else {
            output$hamiltontext <- renderText("You have no valid moves left, please restart to try again!")
            state <<- "NONE"
          }
        } else {
          # Else if that was the final vertex check if it is the same as the first
          if (first==vertex && length(which(cycleRoad==1))==0) {
            output$hamiltontext <- renderText(paste0("You have successfully ended 
                                                     up at your starting point: ",
                                                     PeteDF[first,1]))
            state <<- "NONE"
          } else if (first!=vertex && length(which(cycleRoad==1))==0) {
            output$hamiltontext <- renderText(paste0("You not end up at your starting 
                                                     point: ",PeteDF[first,1]))
            state <<- "NONE"
          }
        }
      # Selected vertex is invalid
      } else {
        if (is.null(state)) return()
        
        # Else if that was the final vertex check if it is the same as the first
        if (first==vertex && length(which(cycleRoad==1))==0) {
          output$hamiltontext <- renderText(paste0("You have successfully ended 
                                                     up at your starting point: ",
                                                   PeteDF[first,1]))
          state <<- "NONE"
        } else if (first!=vertex && length(which(cycleRoad==1))==0) {
          output$hamiltontext <- renderText(paste0("You not end up at your starting 
                                                     point: ",PeteDF[first,1]))
          state <<- "NONE"
        } else {
          output$hamiltontext <- renderText(paste0("Choose a valid point no. [",
                                                   length(hamDF)+1,"]."))
        }
      }
    }
    
    #################
    # COLOR CHOICES #
    #################
    if (state=="COLORS") {
      vertex <- findClosestVert(PeteDF, input$plot_click$x, input$plot_click$y)
      color.choice <- input$choices
      
      if (checkColor(PeteDF, edgeDF, vertex, color.choice)) {
        PeteDF[vertex,4] <<- color.choice
        output$statustext <- renderText("Great choice of color.")
        showTables()
        showPlot()
      } else {
        output$statustext <- renderText("Invalid color/vertex choice, please try
                                        a different choice.")
      }
      
    }
  })
  
  # Plot vertices
  plotVertices <- function(DF) {
    par (mar = c(0,0,0,0))
    plot(DF$x,DF$y, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), asp = 1, pch = 21, cex = 10, bg = DF$bg )
    text(DF$x,DF$y,DF$V, cex = 1.5)
  }
  
  # Plot edges
  plotEdges <- function(vDF,eDF) {
    for (i in 1:nrow(eDF)){
      v1 <- eDF[i,1]
      v2 <- eDF[i,2]
      color <- eDF[i,3]
      x1 <- vDF[which.max(vDF$V == v1),2]
      y1 <- vDF[which.max(vDF$V == v1),3]
      x2 <- vDF[which.max(vDF$V == v2),2]
      y2 <- vDF[which.max(vDF$V == v2),3]
      segments(x1,y1,x2,y2,col = color, lwd = 2, lty = cycleRoad[i])
    }
  }
  
  # Render table showing vertices and edges
  showTables <- function() {
    output$vertex <- renderTable(PeteDF)
    output$edge <- renderTable(edgeDF)
  }
  
  # Render plot of edges and vertices
  showPlot <- function() {
    output$plot1 <- renderPlot({
      plotVertices(PeteDF)
      plotEdges(PeteDF,edgeDF)
    })
  }
  
  # Create dataframe of vertices and edges and render for Hamilton Walk
  initializor <- function() {
    PeteDF <<- makeVertexDF()
    edgeDF <<- makeEdgeDF()
    hamDF <<- c()
    steps <- ""
    cycleRoad <<- rep(1, 20)
    output$hamiltonsteps <- renderText("")
    showPlot()
    showTables()
  }
  
  initializor.cycle <- function() {
    initializor()
    edgeDF <<- makeCycleEdgeDF()
    cycleRoad <<- rep(1, 20)
    first <<- 0
    showPlot()
    showTables()
  }
  
  # Update plots
  updatePlot <- function() {
    showPlot()
    showTables()
  }
  
  # Initialize values
  showPlot()
  showTables()
}

#Run the app
shinyApp(ui = ui, server = server)