# Load Packages
library(shiny)
library(car)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(DT)
library(shinyalert)
library(shinyWidgets)
library(devtools)
#TicTacToe
GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Variance Inflation Factor" # Xigang, this needs to be the spelled out
APP_DESCP  <<- paste(
  "This app this for letting students know VIF scores",
  "using in Regression and also collinearity problem."
)
# End App Meta Data------------------------------------------------------------

# Xigang, remove any comments that don't pertain to your app, including the
# comments I put into the sample app file you worked from.

# Define UI for App
ui <- list(
  # Xigang, you don't need this V (it's only needed in the ui.r + server.r setup)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    #href = "boast.css") ## This is for Neil's testing purposes
  ),
  # Xigang, you don't need this ^
  ## Create the app page
  dashboardPage(
    skin = "black",
    ### Create the app header
    dashboardHeader(
      title = "Variance Inflation Factor", # Xigang, let's use something more informative
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      # Xigang, add the Comment button
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        # Xigang, Recall my comment about focusing on one page (Explore)
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tabItems(
        #### Set up the Overview Page
        tabItem(
          tabName = "Overview",
          withMathJax(),
          # Xigang, you are over-relying on the initialism VIF. You need to say
          # what that stands for.
          h1("Variance Infation Factor(VIF) & Collinearity Problem for BOAST Apps"), # This should be the full name.
          p("This is a Shiny application for BOAST for VIF & Collineraity Problem."),
          h2("Instructions"),
          # Xigang, you need to watch long code lines
          p("You will directly learn what's VIF and what's causing Collinearity Problem."),
          # Xigang, "In this Chapter"? is a strange thing to say
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            tags$li("Play the game to test how far you've come.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Xigang Zhang",
            br(),
            "We would like to extend a special thanks to the Neil J. Hatfield,
             Robert P. Carey, III.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 10/30/2020 by XGZ.")
          )
        ),
        #### Set up the Prerequisites Page
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          # Xigang, you mentioned that you got the following information from
          # a website? Which one? I didn't notice any in the References. You
          # can't just use material you found online without proper attribution.
          # Failure to give credit is a violation of Academic Integrity.
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Variance Inflation Factor(VIF) is the quotient of the variance in a model with multiple terms by
                    the variance of a model with one term alone. It quantifies the severity
                    of multicollinearity in an ordinary least squares regression analysis. "),
            tags$li("It provides an index that measures how much the variance
                    (the square of the estimate's standard deviation) of an estimated regression coefficient
                    is increased because of collinearity."),
            tags$li("For example, the variance inflation factor for the estimated regression coefficient
                    bj —denoted VIFj —is just the factor by which the variance of bj is inflated by the
                    existence of correlation among the predictor variables in the model."),
            tags$li("How do we interpret the variance inflation factors for a regression model?")
          ),
          p("A VIF of 1 means that there is no correlation among the jth predictor and the remaining predictor variables,
            and hence the variance of bj is not inflated at all."),
          box(
            title = strong("What's Collenarity Problem?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Collenarity also named Multicollinearity exists
            when two or more of the predictors in a regression model are moderately or highly correlated with one another.Unfortunately
            when it exists, it can wreak havoc on our analysis and thereby limit the research conclusions we can draw. As following
            The precision of the estimated regression coefficients decreases as more predictors are added to the model
            The marginal contribution of any one predictor variable in reducing the error sum of squares depends on which other predictors are already in the model.
            Hypothesis tests for βk = 0 may yield different conclusions depending on which predictors are in the model."
          ),
          box(
            title = strong("How to determine the Collenarity Problem?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The general rule of thumb is that VIFs exceeding 4 warrant further investigation,
            while VIFs exceeding 10 are signs of serious multicollinearity requiring correction."

          )
        ),
        # Xigang, see my prior comment about removing comments not realted to your
        # app
        #### Set up an Explore Page
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore the Concept of VIF & Collinearity Problem"),
          tabsetPanel(
            type = "tabs",
            ### VIF Examples tab----
            tabPanel(
              title = "DC bike Sharing Data",
              # Xigang, be sure that you have a paragraph explaining the context
              # of the data collection, otherwise your user won't really know
              # what is going on.
              br(),
              p(
                "This dataset comes from bike rental demand in the Capital Bikeshare 
                program in Washington, D.C. And you will explore whether or not variables 
                would have collineartiy problem. Remember you must select at least two variables"
              ),
              fluidRow(
                column(
                  # Xigang, be explicit: width = 4
                  width = 4,
                  h3("Controls"),
                  checkboxGroupInput(
                    inputId = "bikePredSelect",
                    label = "Select your predictors",
                    choices = list(
                      "temp",
                      "humidity",
                      "windspeed",
                      "atemp"
                    )
                  )
                ),
                column(
                  8,
                  h3("Result"),
                  DT::dataTableOutput("Viftable"),
                  bsPopover(
                    id = "VIFtable",
                    title = "VIF Result!",
                    content = "What happens when VIF is larger than 10?",
                    placement = "top"
                  )
                )
              ),
              br(),
              p(
                tags$em("Note"),
                # Xigang, See prior note about using other's material without
                # attribution
                ": As the name suggests, a variance inflation factor (VIF) quantifies
          how much the variance is inflated. But what variance?
          Recall that we learned previously that the standard errors —
          and hence the variances — of the estimated coefficients are inflated when multicollinearity exists.
          A variance inflation factor exists for each of the predictors in a multiple regression model.
          The variance inflation factor for the estimated regression coefficient bj —denoted
          VIFj —is just the factor by which the variance of bj is inflated by the existence of correlation
          among the predictor variables in the model."
              )
              # Xigang, remove V
              # End of Xigang Zhang's code-----------------------------------------
            ),
            ## Sesame street tab ----
            tabPanel(
              # Xigang, title should be Sesame Street Data
              title = "Sesame.St data",
              br(),
              p(
                "In this portion, you'll explore whether or not two continuous variables
            would have Collinearity problem by selecting different variables.
            You are able to control only one aspects: 1) the type of variables."
              ),
              fluidRow(
                column(
                  4,
                  h3("Controls"),
                  selectInput(
                    inputId = "Sesame",
                    label = "Select your interested variable",
                    choices = c('age','viewcat','site')
                  ),
                  br(),
                  selectInput(
                    inputId = "Sesame",
                    label = "Select your another interested variable",
                    choices = c('prenumb','preform','peabody')
                  )
                ),
                column(
                  8,
                  h3("Result"),
                  DT::dataTableOutput("Viftable1"),
                  bsPopover(
                    id = "VIFtable",
                    title = "VIF Result!",
                    content = "What happens when VIF is larger than 10?",
                    placement = "top"
                  )
                )
              ),
              br(),
              p(
                tags$em("Note"),
                ":
          The variance inflation factor for the estimated regression coefficient bj —denoted
          VIFj —is just the factor by which the variance of bj is inflated by the existence of correlation
          among the predictor variables in the model."
              )
              # End of Xigang Zhang's code-----------------------------------------
            )
            )
          ),
            #game Page
            tabItem(
              tabName = "game",
              withMathJax(),
              useShinyalert(),
              h2("Tic-Tac-Toe"),
              p(
                "To play, click on any one of the buttons that have a question mark.
            A question will appear to the right with possible answers. If you answer
            correctly, you will take the square; if not, the computer will take
            the square. Try your best to win the game!"
              ),
              h3(uiOutput("player")),
              fluidRow(
                div(
                  class = "col-sm-12 col-md-4",
                  h3("game Board"),
                  br(),
                  uiOutput("gameBoard", class = "game-board")
                ),
                div(
                  class = "col-sm-12 col-md-8",
                  h3("Question"),
                  withMathJax(uiOutput("question")),
                  uiOutput("extraOutput"),
                  h3("Answer"),
                  uiOutput("answer"),
                  actionButton(
                    inputId = "submit",
                    label = "Submit",
                    color = "primary",
                    size = "large",
                    style = "bordered",
                    disabled = TRUE
                  ),
                  actionButton(
                    inputId = "reset",
                    label = "Reset game",
                    color = "primary",
                    size = "large",
                    style = "bordered"
                  ),
                  br(),
                  #These two triggers help with MathJax re-rendering
                  uiOutput("trigger1"),
                  uiOutput("trigger2")
                )
              )
            ),
            #### Set up the References Page-REQUIRED
            tabItem(
              tabName = "References",
              withMathJax(),
              h2("References"),
              # Xigang, you need to include all of the data sources as well as
              # where you got any content
              p(
                class = "hangingindent",
                "Attali, D. and Edwards, T. (2018). shinyalert: Easily create pretty
            popup messages (modals) in 'Shiny'. (v1.0). [R package]. Available
            from https://CRAN.R-project.org/package=shinyalert"
              ),
              p(
                class = "hangingindent",
                "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
              ),
              p(
                class = "hangingindent",
                "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package].
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
              ),
              p(
                class = "hangingindent",
                "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
              ),
              p(
                class = "hangingindent",
                "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019).
            shiny: Web application framework for R. (v1.4.0) [R Package]. Available
            from https://CRAN.R-project.org/package=shiny"
              ),
              p(
                class = "hangingindent",
                "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom
            inputs widgets for shiny. (v0.5.0) [R Package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
              )
            )
          )
        )
      )
    )
# Define server logic
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$go1, {
    updateTabItems(session, "pages", "Explore")
  })
  # Variables
  activeBtn <- NA
  activeQuestion <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(
      data = rep.int(0, times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  gameProgress <- FALSE

  #VIF TABLE
  BikeSharing <- read.csv(
    file = "DCbikeSharing.csv",
    stringsAsFactors = FALSE,
    as.is = TRUE
  )
  dc <- eventReactive(
    eventExpr = input$bikePredSelect,
    valueExpr = {
      #Xigang, you'll want to add some sort of error checking here
      lm(
        formula = as.formula(paste("count ~ ", paste(input$bikePredSelect, collapse = "+"))),
        data = DCbikeSharing
      )
    }
  )
  output$Viftable <- DT::renderDataTable({
    expr = data.frame(VIF = round(vif(dc()), digits = 4))
    # Xigang, you'll want to add your options here
  })
  # Xigang, you'll want to take what we did for the bike sharing and duplicate
  # that for the other data sets.
  # Xigang, perhaps instead of using tabs like you currently do, you use a
  # selectInput for the data set and the pages/options update. That would let you
  # leverage your code more efficiently (i.e., you don't have to repeat yourself)
  sesame <-
    read.csv("sesame.csv",
             stringsAsFactors = FALSE,
             as.is = TRUE)
  ss <- lm(improvenumb ~ age + viewcat + site + prenumb + preform + peabody, data = sesame)
  output$Viftable1 <- DT::renderDataTable({
    as.data.frame(vif(ss))
  })

  # Helper Functions
  .tileCoordinates <- function(tile = NULL, index = NULL) {
    row <- -1
    col <- -1

    # if: button tile is given, derive from id
    # else: derive from index
    if (!is.null(tile)) {
      # grid-[row]-[col]
      tile <- strsplit(tile, "-")[[1]]
      tile <- tile[-1] # remove oxo

      row <- strtoi(tile[1])
      col <- strtoi(tile[2])
    } else {
      row <- (index - 1) %/% GRID_SIZE + 1
      col <- index - (GRID_SIZE * (row - 1))
    }

    coordinates <- list("row" = row,
                        "col" = col)

    return(coordinates)
  }

  .tileIndex <- function(tile) {
    coords <- .tileCoordinates(tile)

    index = GRID_SIZE * (coords$row - 1) + coords$col

    return(index)
  }

  .btnReset <- function(index) {
    coords <- .tileCoordinates(index = index)
    id <- paste0("grid-", coords$row, "-", coords$col)
    updateButton(
      session = session,
      inputId = id,
      label = "?",
      disabled = FALSE
    )
  }

  .score <- function(score, tile, value) {
    i <- .tileCoordinates(tile)

    score[i$row, i$col] <- value

    return(score)
  }

  .gameCheck <- function(mat) {
    rows <- rowSums(mat)
    cols <- colSums(mat)

    if (GRID_SIZE > 1) {
      mainD <- sum(diag(mat))
      rotated <- apply(t(mat), 2, rev)
      offD <- sum(diag(rotated))

      if (GRID_SIZE %in% rows ||
          GRID_SIZE %in% cols ||
          mainD == GRID_SIZE || offD == GRID_SIZE) {
        return("win")
      } else if (-GRID_SIZE %in% rows ||
                 -GRID_SIZE %in% cols == 1 ||
                 mainD == -GRID_SIZE || offD == -GRID_SIZE) {
        return("lose")
      } else if (any(mat == 0)) {
        return("continue")
      } else {
        return("draw")
      }
    } else {
      ifelse(rows == 1 && rows != 0, return("win"), return("lose"))
    }
  }

  .boardBtn <- function(tile) {
    index <- .tileIndex(tile)
    activeQuestion <<- gameSet[index, "id"]

    output$question <- renderUI({
      withMathJax()
      return(gameSet[index, "question"])
    })

    output$answer <- .ansFunc(index, gameSet)

    if (gameSet[index, "extraOutput"] != "") {
      output$extraOutput <- renderText({
        gameSet[index, "extraOutput"]
      })
    } else {
      output$extraOutput <- NULL
    }

    #Retrigger MathJax processing
    output$trigger1 <- renderUI({
      withMathJax()
    })
    output$trigger2 <- renderUI({
      withMathJax()
    })

    #Enable Submit Button
    updateButton(session = session,
                 inputId = "submit",
                 disabled = FALSE)
  }

  .ansFunc <- function(index, df) {
    if (df[index, "format"] == "numeric") {
      renderUI({
        numericInput(inputId = "ans",
                     label = df[index, "label"],
                     value = 0)
      })
    } else if (df[index, "format"] == "two") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"]),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textgame",
          direction = "horizontal",
          individual = TRUE
        )
      })
    } else if (df[index, "format"] == "three") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"]),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textgame",
          direction = "vertical"
        )
      })
    } else {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"],
                         df[index, "D"]),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textgame",
          direction = "vertical"
        )
      })
    }
  }

  .gameReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    qSelected <<-
      sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
    gameSet <<- questionBank[qSelected,]

    output$question <-
      renderUI({
        return("Click a button on the game board to get started on your new game.")
      })
    output$answer <- renderUI({
      ""
    })
    output$extraOutput <- renderUI({
      ""
    })
    scoreMatrix <<-
      matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    gameProgress <- FALSE
    activeBtn <- NA

    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  }

  .generateStatement <- function(session, verb = NA, object = NA, description = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", session$input$tabs)
    } else {
      object <- paste0("#", object)
    }

    statement <- rlocker::createStatement(list(
      verb =  verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    ))
    print(statement)
    return(rlocker::store(session, statement))
  }

  .generateAnsweredStatement <- function(session, verb = NA, object = NA, description = NA, interactionType = NA, response = NA, success = NA, completion = FALSE) {
    statement <- rlocker::createStatement(list(
      verb = verb,
      object = list(
        id = paste0(getCurrentAddress(session), "#", object),
        name = paste0(APP_TITLE),
        description = paste0("Question ", activeQuestion, ": ", description),
        interactionType = interactionType
      ),
      result = list(
        success = success,
        response = response,
        completion = completion
        # extensions = list(
        #   ref = "https://shinyapps.science.psu.edu/scoreMatrix", value = paste(as.data.frame(scoreMatrix), collapse = ", ")
        #   )
      )
    )
    )

    # print(statement)
    return(rlocker::store(session, statement))
  }

  # Define navigation buttons
  observeEvent(input$go1, {
    updateTabItems(session,
                   inputId = "tabs",
                   selected = "Explore")
  })

  # Read in data and generate the first subset
  questionBank <-
    read.csv("questionBank.csv",
             stringsAsFactors = FALSE,
             as.is = TRUE)
  qSelected <-
    sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  gameSet <- questionBank[qSelected,]

  # Program the Reset Button
  observeEvent(input$reset, {
    .generateStatement(session, object = "reset", verb = "interacted", description = "game board has been reset.")
    .gameReset()
  })

  # Render game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1

    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)

        board[[index]] <<- tags$li(
          actionButton(
            inputId = paste0("grid-", row, "-", column),
            label = "?",
            color = "primary",
            style = "bordered",
            class = "grid-fill"
          ),
          class = "grid-tile"
        )

        observeEvent(session$input[[id]], {
          activeBtn <<- id
          .boardBtn(id)
          .generateStatement(session, object = activeBtn, verb = "interacted", description = paste0("Tile ", activeBtn, " selected. Rendering question: ", activeQuestion, "."))
        })

        index <<- index + 1
      })
    })

    tags$ol(board, class = paste(
      "grid-board",
      "grid-fill",
      paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
    ))
  })

  # Program Submit Button
  observeEvent(input$submit, {
    index <- .tileIndex(activeBtn)
    answer <- ""

    if (gameSet[index, "format"] == "numeric") {
      answer <- gameSet[index, "answer"]
    } else {
      answer <- gameSet[index, gameSet[index, "answer"]]
    }

    success <- input$ans == answer

    if (success) {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = player,
        disabled = TRUE
      )
      scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
    } else {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = opponent,
        disabled = TRUE
      )
      scoreMatrix <<- .score(scoreMatrix, activeBtn,-1)
    }

    # Check for game over states
    .gameState <- .gameCheck(scoreMatrix)
    completion <- ifelse(.gameState == "continue", FALSE, TRUE)
    interactionType <- ifelse(gameSet[index,]$format == "numeric", "numeric", "choice")

    .generateAnsweredStatement(
      session,
      object = activeBtn,
      verb = "answered",
      description = gameSet[index,]$question,
      response = input$ans,
      interactionType = interactionType,
      success = success,
      completion = completion
    )

    if (.gameState == "win") {
      .generateStatement(session, object = "game", verb = "completed", description = "Player has won the game.")
      confirmSweetAlert(
        session = session,
        inputId = "endgame",
        title = "You Win!",
        text = "You've filled either a row, a column, or a main diagonal. Start over and play a new game.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "lose") {
      .generateStatement(session, object = "game", verb = "completed", description = "Player has lost the game.")
      confirmSweetAlert(
        session = session,
        inputId = "endgame",
        title = "You lose :(",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "draw") {
      .generateStatement(session, object = "game", verb = "completed", description = "game has ended in a draw.")
      confirmSweetAlert(
        session = session,
        inputId = "endgame",
        title = "Draw!",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    }
    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  })

  observeEvent(input$tabs, {
    if (input$tabs == "game") {
      if (!gameProgress) {
        shinyalert(
          title = "Player Select",
          text = "Select whether you want to play as O or X.",
          showConfirmButton = TRUE,
          confirmButtonText = "Play as X",
          showCancelButton = TRUE,
          cancelButtonText = "Play as O"
        )
        gameProgress <<- TRUE
      }
    }
    .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", input$tabs, " tab."))
  }, ignoreInit = TRUE)

  observeEvent(input$endgame, {
    .generateStatement(session, object = "endgame", verb = "interacted", description = paste("game has been reset."))
    .gameReset()
  })

  observeEvent(input$shinyalert, {
    if (input$shinyalert == TRUE) {
      player <<- "X"
      opponent <<- "O"
    }
    if (input$shinyalert == FALSE) {
      player <<- "O"
      opponent <<- "X"
    }

    .generateStatement(session, object = "shinyalert", verb = "interacted", description = paste0("User has selected player: ", player))

    output$player <- renderUI({
      return(paste0("You are playing as ", player, "."))
    })
  })
}
# End of code-------------------------------------------------


# Create Shiny App using BOAST App template
boastApp(ui = ui, server = server)
# shinyApp(ui = ui, server = server) # For testing purposes only