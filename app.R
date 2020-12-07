# Load Packages
library(shiny)
library(car)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

# Load Data, and define Global Functions and Variables----
bikeSharing <- read.csv(
  file = "DCbikeSharing.csv",
  stringsAsFactors = FALSE,
  as.is = TRUE
)
sesameSt <- read.csv(
  file = "sesame.csv",
  stringsAsFactors = FALSE,
  as.is = TRUE
)
amesHousing <- read.csv(
  file = "AmesHousingClean.csv",
  stringsAsFactors = FALSE,
  as.is = TRUE
)

## Drop NAs (Ames) and sample at 50% the two larger data sets (10k+ and 2k+)
bikeSharing <- bikeSharing %>%
  dplyr::slice_sample(prop = 0.5)

amesHousing <- amesHousing %>%
  tidyr::drop_na() %>%
  dplyr::slice_sample(prop = 0.5)

prettyVarNames <- function(vars){
  temp1 <- dplyr::recode(
    .x = vars,
    "temp" = "Temperature",
    "windspeed" = "Windspeed",
    "age" = "Age",
    "humidity" = "Humidity",
    "atemp" = "Air Temp",
    "prenumb" = "Prior Num. Skill",
    "encour" = "Encouraged",
    "viewcat" = "Viewing Freq.",
    "site" = "View Site",
    "preform" = "Prior Form Skill",
    "Lot.Frontage" = "Frontage",
    "Lot.Area" = "Area",
    "Year.Built" = "Year Built",
    "Garage.Area" = "Garg. Area",
    "Gr.Liv.Area" = "Grd Living Area",
    "Total.Bsmt.SF" = "Tot. Bsmt Area",
    "Garage.Yr.Blt" = "Garg. Year",
    "X1st.Flr.SF" = "1st Flr. Area",
    "X2nd.Flr.SF" = "2nd Flr. Area"
    )
  return(temp1)
}

# TicTacToe--Keep for future development
# GRID_SIZE <- 3
# TILE_COUNT <- GRID_SIZE ^ 2

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Collinearity",
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Variance_Inflation_Factor"
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        ### Turning off the game page for now
        # menuItem("game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Set up the Overview Page ----
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Variance Inflation Factor (VIF) & Collinearity"),
          p("In regression, the collinearity problem is a common and sometimes
            serious problem analysts face. One way to detect collinearity
            of predictors is to use the Variance Inflation Factor."),
          h2("Instructions"),
          p("Explore the Variance Inflation Factor and how to detect collinearity
            amongst multiple predictors."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            ## Removing this line for now
            # tags$li("Play the game to test how far you've come.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Xigang Zhang",
            br(),
            "I would like to extend a special thanks to the Neil J. Hatfield",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/04/2020 by XGZ.")
          )
        ),
        ### Set up the Prerequisites Page ----
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following."),
          box(
            title = "What is Collenarity?",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p(
              "Collinearity, also called multicollinearity, occurs when two or
              more predictors in a regression model are moderately or highly
              correlated with one another. Collinearity can distort our analysis
              and thereby limit the conclusions we draw. The precision of the
              estimated regression coefficients decreases as more predictors are
              added to the model The marginal contribution of any one predictor
              in reducing the error sum of squares depends on which other
              predictors are already in the model. Hypothesis tests for
              \\(\\beta_k=0\\) may yield different conclusions depending on
              which predictors are in the model."
            )
          ),
          box(
            title = "Variance Inflaction Factor",
            width = "100%",
            collapsible = TRUE,
            collapsed = TRUE,
            p(
              "The Variance Inflation Factor (VIF) is the quotient of the
              variance of a model with multiple terms and the variance of the
              model with a single term. The VIF quantifies the severity of
              multicollinearity in an ordinary least squares regression analysis.
              This is to say, the VIF provides an index that measures how much
              the variance of an estimated regression coefficient is increased
              due to the collinearity of predictors."
            ),
            p(
              "For example, the VIF for the estimated regression coefficient
              \\(b_j\\), denoted \\(VIF_j\\), is the factor by which the variance
              of \\(b_j\\) is inflated by the existence of correlation among the
              predictor variables in the model. Thus, a \\(VIF_{x1}=8.3\\)
              indicates that variance of \\(b_{x1}\\) is 8.3 times larger in the
              full model than when we only have the \\(x1\\) predictor. A VIF of
              1 means that there is no correlation between predictor \\(j\\) and
              the other predictors in the model. Thus, the variance of \\(b_j\\)
              is not inflated."
            )
          ),
          box(
            title = "Determining Whether There is Collenarity",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              "A general rule of thumb is that VIFs greater than 5 indicate
              additional investigation, while VIFs greater than 10 point to
              serious multicollinearity requiring correction."
            )
          )
        ),
        ### Set up an Explore Page ----
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore the VIFs & Collinearity"),
          p("You can choose different datasets, select various predictors, and
            then explore several tools for looking at collinearity including a
            scatter plot matrix of predictors, the VIF table, and model summeries."
          ),
          ## Removing this line for now.
          # p("It will also be part of Game question."),
          br(),
          selectInput(
            inputId = 'selectedData',
            label = "Select a data set",
            choices = list(
              "DC Bike Sharing Data" = "bikeSharing",
              "Sesame Street" = "sesameSt",
              "Ames Housing Market" = "amesHousing"
            ),
            selected = 'bikeSharing'
          ),
          uiOutput("dataContext"),
          br(),
          fluidRow(
            column(
              width = 4,
              h3("Available Predictors"),
              checkboxGroupInput(
                inputId = "selectedVars",
                label = "Select at least two of the following",
                choices = c("filler")
              )
            ),
            column(
              width = 8,
              tabsetPanel(
                type = "tabs",
                #### Three output tab---
                tabPanel(
                  title = "Scatter Plot Martix",
                  br(),
                  p(
                    "Try using scatter plot Martix to figure out which predictors
                    have strong correlations."
                  ),
                  plotOutput("scatterplots"),
                ),
                tabPanel(
                  title = "VIF Table",
                  br(),
                  p(
                    "Becareful to those VIF scores larger than 5. Select at least
                    two variables."
                  ),
                  DT::dataTableOutput("vifTable"),
                ),
                tabPanel(
                  title = "ANOVA Table",
                  br(),
                  p(
                    "Use the following tables to explore the impacts of including
                    predictors which trigger collinearity problems. What happens
                    when you remove at least one of those predictors?"
                  ),
                  DT::dataTableOutput("anovaTable"),
                  DT::dataTableOutput("coefficients"),
                )
              )
            )
          )
        ),
        ## Game page is being disabled for now
        # #game Page ----
        # tabItem(
        #   tabName = "game",
        #   withMathJax(),
        #   useShinyalert(),
        #   h2("Tic-Tac-Toe"),
        #   p(
        #     "To play, click on any one of the buttons that have a question mark.
        #     A question will appear to the right with possible answers. If you answer
        #     correctly, you will take the square; if not, the computer will take
        #     the square. Try your best to win the game!"
        #   ),
        #   h3(uiOutput("player")),
        #   fluidRow(
        #     div(
        #       class = "col-sm-12 col-md-4",
        #       h3("game Board"),
        #       br(),
        #       uiOutput("gameBoard", class = "game-board")
        #     ),
        #     div(
        #       class = "col-sm-12 col-md-8",
        #       h3("Question"),
        #       withMathJax(uiOutput("question")),
        #       uiOutput("extraOutput"),
        #       h3("Answer"),
        #       uiOutput("answer"),
        #       actionButton(
        #         inputId = "submit",
        #         label = "Submit",
        #         color = "primary",
        #         size = "large",
        #         style = "bordered",
        #         disabled = TRUE
        #       ),
        #       actionButton(
        #         inputId = "reset",
        #         label = "Reset game",
        #         color = "primary",
        #         size = "large",
        #         style = "bordered"
        #       ),
        #       br(),
        #       #These two triggers help with MathJax re-rendering
        #       uiOutput("trigger1"),
        #       uiOutput("trigger2")
        #     )
        #   )
        # ),
        ### References Page ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities.
            (v0.1.10). [R Package]. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2020).
            shiny: Web application framework for R. (v1.5.0) [R Package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Fox, J. and Weisberg, S. (2019). An {R} Companion to Applied
            Regression, Third Edition. Thousand Oaks CA: Sage. URL:
            https://socialsciences.mcmaster.ca/jfox/Books/Companion/"
          ),
          p(
            class = "hangingindent",
            "Schloerke, B., Cook, D., Larmarange, J., Briatte, F., Marbach, M.,
            Thoen, E., Elberg, A., and Crowley, J. (2020).
            GGally: Extension to 'ggplot2'. Rpackage version 2.0.0.
            https://CRAN.R-project.org/package=GGall"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. ggplot2: Elegant Graphics for Data Analysis.
            Springer-Verlag New York, 2016. Available at https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2020). tidyr: Tidy Messy Data.
            R package version 1.1.2. https://CRAN.R-project.org/package=tidyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., and Müller, K.
            (2020). dplyr: A Grammar of Data Manipulation. R package version 1.0.2.
             https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2020). DT: A Wrapper of the
            JavaScript Library 'DataTables'. R package version 0.16.
            https://CRAN.R-project.org/package=DT"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$go1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Explore"
    )
  })

  ## Game code is disabled for now
  # ## Game Variables ----
  # activeBtn <- NA
  # activeQuestion <- NA
  # player <- NA
  # opponent <- NA
  # scoreMatrix <-
  #   matrix(
  #     data = rep.int(0, times = TILE_COUNT),
  #     nrow = GRID_SIZE,
  #     ncol = GRID_SIZE
  #   )
  # gameProgress <- FALSE

  ## Dataset Control----
  observeEvent(
    eventExpr = input$selectedData,
    handlerExpr = {
      ### Data Context
      output$dataContext <- renderUI({
        switch(
          EXPR = input$selectedData,
          "bikeSharing" = p("This dataset comes from bike rental demand in the
                            Capital Bikeshare program in Washington, D.C. And you
                            will explore whether or not variables would have
                            collineartiy problem."),
          "sesameSt" = p("This dataset evaluated the impact of the first year of
                         the Sesame Street television series. Sesame Street was
                         concerned mainly with teaching preschool related skills
                         to children. Both before and after viewing the series the
                         chilren were tested on a variety of cognitive variables,
                         including knowledge of body parts, letters, numbers,
                         etc."),
          "amesHousing" = p("De cock (2011) described data on 82 fields for 2,930
                           properties located in Ames, IA.")
        )
      })

      ### Variable Selections
      updateCheckboxGroupInput(
        session = session,
        inputId = "selectedVars",
        choices = switch(
          EXPR = input$selectedData,
          "bikeSharing" = list(
            "Temperature" = "temp",
            "Humidity" = "humidity",
            "Windspeed" = "windspeed",
            "Air Temperature" = "atemp"
          ),
          "sesameSt" = list(
            "Prior Number Skill Score" = "prenumb",
            "Age" = "age",
            "Encouraged to Watch" = "encour",
            "Viewing Frequency" = "viewcat",
            "Viewing Site" = "site",
            "Prior Form Skill Score" = "preform"
          ),
          "amesHousing" = list(
            "Lot Frontage" = "Lot.Frontage",
            "Lot Area" = "Lot.Area",
            "Year Built" = "Year.Built",
            "Garage Area" = "Garage.Area",
            "Ground living area" = "Gr.Liv.Area",
            "Total Basement Area" = "Total.Bsmt.SF",
            "Garage Year Bulit" = "Garage.Yr.Blt",
            "1st Floor Area" = "X1st.Flr.SF",
            "2nd Floor Area" = "X2nd.Flr.SF"
          )
        )
      )
    },
    ignoreNULL = TRUE
  )

  ## Set the data set ----
  dataSet <- reactiveVal()
  observeEvent(
    eventExpr = input$selectedData,
    handlerExpr = {
      dataSet(
        switch(
          EXPR = input$selectedData,
          "bikeSharing" = bikeSharing,
          "sesameSt" = sesameSt,
          "amesHousing" = amesHousing
        )
      )
    }
  )

  ## Set the response variable ----
  responseVar <- eventReactive(
    eventExpr = input$selectedData,
    valueExpr = {
      switch(
        EXPR = input$selectedData,
        "bikeSharing" = "count",
        "sesameSt" = "improvenumb",
        "amesHousing" = "SalePrice"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Fit the Model ----
  #y not informative "ifelse
  model <- eventReactive(
    eventExpr = input$selectedVars,
    valueExpr = {
      validate(
        need(
          expr = length(input$selectedVars) >= 2,
          message = "Please select at least two predictors."
        )
      )
      lm(
        formula = as.formula(
          paste(
            responseVar(),
            " ~ ",
            paste(input$selectedVars, collapse = "+")
          )
        ),
        data = dataSet() #this is how you would call the data regardless
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  ## Rendering the three Exploration results ----
  observeEvent(
    eventExpr = input$selectedVars,
    handlerExpr = {
      ### Scatter plots----------
      output$scatterplots <- renderCachedPlot({
        validate(
          need(
            expr = length(input$selectedVars) >= 2,
            message = "Please select at least two predictors."
          )
        )
        lowerFn <- function(data = dataSet(), mapping, method = "lm"){
          p <- ggplot(data = dataSet(), mapping = mapping) +
            geom_point(colour = "black", alpha = 0.5) +
            geom_smooth(method = method, formula = y ~ x, se = FALSE, color = "red")
          return(p)
        }
        ggpairs(
          data = dataSet(),
          columns = input$selectedVars,
          upper = list(continuous = wrap(lowerFn, method = "lm")),
          lower = list(continuous = "blank"),
          diag = list(continuous = "blankDiag"),
          axisLabels = "show",
          progress = FALSE,
          columnLabels = prettyVarNames(vars = input$selectedVars)
        ) +
          theme_bw() +
          theme(
            text = element_text(size = 18)
          )
      },
      cacheKeyExpr = {list(input$selectedData, input$selectedVars)}
      )

      ### VIF table ----
      output$vifTable <- DT::renderDataTable(
        expr = {
          validate(
            need(
              expr = length(input$selectedVars) >= 2,
              message = "Please select at least two predictors."
            )
          )
          data.frame(
            VIF = round(vif(model()), digits = 4),
            row.names = prettyVarNames(input$selectedVars)
          )
        },
        options = list(
          responsive = TRUE,
          scrollx = FALSE,
          ordering = FALSE,
          paging = FALSE,
          lengthChange = FALSE,
          pageLeength = 4,
          searching = FALSE,
          info = FALSE
        )
      )
      ### Anova table ----
      anovaOut <- anova(model())
      rowNames <- prettyVarNames(row.names(anovaOut))
      anovaOut <- apply(
        X = anovaOut,
        MARGIN = 2,
        FUN = "prettyNum",
        big.mark = ",",
        digits = 3
      )
      anovaOut <- as.data.frame(anovaOut) %>%
        dplyr::mutate(
          `Pr(>F)` = ifelse(`Pr(>F)` < 0.0001, "< 0.0001", `Pr(>F)`)
        ) %>%
        dplyr::na_if(y = "NA")
      row.names(anovaOut) <- rowNames

      output$anovaTable <- DT::renderDataTable(
        expr = {
          validate(
            need(
              expr = length(input$selectedVars) >= 2,
              message = "Please select at least two predictors."
            )
          )
          anovaOut
        },
        options = list(
          responsive = TRUE,
          scrollx = FALSE,
          ordering = FALSE,
          paging = FALSE,
          lengthChange = FALSE,
          searching = FALSE,
          info = FALSE
        )
      )
      ## Coefficients ----
      coefficientVals <- round(summary(model())$coefficients, digits = 4)
      tableLabels <- attr(coefficientVals, "dimnames")
      coefOut <- apply(
        X = coefficientVals,
        MARGIN = 2,
        FUN = "prettyNum",
        big.mark = ",",
        digits = 3
      )
      coefOut <- as.data.frame(coefOut) %>%
        dplyr::mutate(
          `Pr(>|t|)` = ifelse(`Pr(>|t|)` < 0.0001, "< 0.0001", `Pr(>|t|)`)
        ) %>%
        dplyr::na_if(y = "NA")
      row.names(coefOut) <- prettyVarNames(tableLabels[[1]])

      output$coefficients <- DT::renderDataTable(
        expr = {
          validate(
            need(
              expr = length(input$selectedVars) >= 2,
              message = "Please select at least two predictors."
            )
          )
          coefOut
          },
        options = list(
          responsive = TRUE,
          scrollx = FALSE,
          ordering = FALSE,
          paging = FALSE,
          lengthChange = FALSE,
          searching = FALSE,
          info = FALSE
        )
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  ## Tic-Tac-Toe Section ----
  ## Disabled for now
  #   # Helper Functions
  #   .tileCoordinates <- function(tile = NULL, index = NULL) {
  #     row <- -1
  #     col <- -1
  #
  #     # if: button tile is given, derive from id
  #     # else: derive from index
  #     if (!is.null(tile)) {
  #       # grid-[row]-[col]
  #       tile <- strsplit(tile, "-")[[1]]
  #       tile <- tile[-1] # remove oxo
  #
  #       row <- strtoi(tile[1])
  #       col <- strtoi(tile[2])
  #     } else {
  #       row <- (index - 1) %/% GRID_SIZE + 1
  #       col <- index - (GRID_SIZE * (row - 1))
  #     }
  #
  #     coordinates <- list("row" = row,
  #                         "col" = col)
  #
  #     return(coordinates)
  #   }
  #
  #   .tileIndex <- function(tile) {
  #     coords <- .tileCoordinates(tile)
  #
  #     index = GRID_SIZE * (coords$row - 1) + coords$col
  #
  #     return(index)
  #   }
  #
  #   .btnReset <- function(index) {
  #     coords <- .tileCoordinates(index = index)
  #     id <- paste0("grid-", coords$row, "-", coords$col)
  #     updateButton(
  #       session = session,
  #       inputId = id,
  #       label = "?",
  #       disabled = FALSE
  #     )
  #   }
  #
  #   .score <- function(score, tile, value) {
  #     i <- .tileCoordinates(tile)
  #
  #     score[i$row, i$col] <- value
  #
  #     return(score)
  #   }
  #
  #   .gameCheck <- function(mat) {
  #     rows <- rowSums(mat)
  #     cols <- colSums(mat)
  #
  #     if (GRID_SIZE > 1) {
  #       mainD <- sum(diag(mat))
  #       rotated <- apply(t(mat), 2, rev)
  #       offD <- sum(diag(rotated))
  #
  #       if (GRID_SIZE %in% rows ||
  #           GRID_SIZE %in% cols ||
  #           mainD == GRID_SIZE || offD == GRID_SIZE) {
  #         return("win")
  #       } else if (-GRID_SIZE %in% rows ||
  #                  -GRID_SIZE %in% cols == 1 ||
  #                  mainD == -GRID_SIZE || offD == -GRID_SIZE) {
  #         return("lose")
  #       } else if (any(mat == 0)) {
  #         return("continue")
  #       } else {
  #         return("draw")
  #       }
  #     } else {
  #       ifelse(rows == 1 && rows != 0, return("win"), return("lose"))
  #     }
  #   }
  #
  #   .boardBtn <- function(tile) {
  #     index <- .tileIndex(tile)
  #     activeQuestion <<- gameSet[index, "id"]
  #
  #     output$question <- renderUI({
  #       withMathJax()
  #       return(gameSet[index, "question"])
  #     })
  #
  #     output$answer <- .ansFunc(index, gameSet)
  #
  #     if (gameSet[index, "extraOutput"] != "") {
  #       output$extraOutput <- renderText({
  #         gameSet[index, "extraOutput"]
  #       })
  #     } else {
  #       output$extraOutput <- NULL
  #     }
  #
  #     #Retrigger MathJax processing
  #     output$trigger1 <- renderUI({
  #       withMathJax()
  #     })
  #     output$trigger2 <- renderUI({
  #       withMathJax()
  #     })
  #
  #     #Enable Submit Button
  #     updateButton(session = session,
  #                  inputId = "submit",
  #                  disabled = FALSE)
  #   }
  #
  #   .ansFunc <- function(index, df) {
  #     if (df[index, "format"] == "numeric") {
  #       renderUI({
  #         numericInput(inputId = "ans",
  #                      label = df[index, "label"],
  #                      value = 0)
  #       })
  #     } else if (df[index, "format"] == "two") {
  #       renderUI({
  #         radioGroupButtons(
  #           inputId = "ans",
  #           choices = list(df[index, "A"],
  #                          df[index, "B"]),
  #           checkIcon = list(
  #             yes = icon("check-square"),
  #             no = icon("square-o")
  #           ),
  #           status = "textgame",
  #           direction = "horizontal",
  #           individual = TRUE
  #         )
  #       })
  #     } else if (df[index, "format"] == "three") {
  #       renderUI({
  #         radioGroupButtons(
  #           inputId = "ans",
  #           choices = list(df[index, "A"],
  #                          df[index, "B"],
  #                          df[index, "C"]),
  #           checkIcon = list(
  #             yes = icon("check-square"),
  #             no = icon("square-o")
  #           ),
  #           status = "textgame",
  #           direction = "vertical"
  #         )
  #       })
  #     } else {
  #       renderUI({
  #         radioGroupButtons(
  #           inputId = "ans",
  #           choices = list(df[index, "A"],
  #                          df[index, "B"],
  #                          df[index, "C"],
  #                          df[index, "D"]),
  #           checkIcon = list(
  #             yes = icon("check-square"),
  #             no = icon("square-o")
  #           ),
  #           status = "textgame",
  #           direction = "vertical"
  #         )
  #       })
  #     }
  #   }
  #
  #   .gameReset <- function() {
  #     lapply(1:TILE_COUNT, .btnReset)
  #     qSelected <<-
  #       sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  #     gameSet <<- questionBank[qSelected,]
  #
  #     output$question <-
  #       renderUI({
  #         return("Click a button on the game board to get started on your new game.")
  #       })
  #     output$answer <- renderUI({
  #       ""
  #     })
  #     output$extraOutput <- renderUI({
  #       ""
  #     })
  #     scoreMatrix <<-
  #       matrix(
  #         data = rep.int(0, times = TILE_COUNT),
  #         nrow = GRID_SIZE,
  #         ncol = GRID_SIZE
  #       )
  #     gameProgress <- FALSE
  #     activeBtn <- NA
  #
  #     updateButton(session = session,
  #                  inputId = "submit",
  #                  disabled = TRUE)
  #   }
  #
  #   .generateStatement <- function(session, verb = NA, object = NA, description = NA) {
  #     if(is.na(object)){
  #       object <- paste0("#shiny-tab-", session$input$tabs)
  #     } else {
  #       object <- paste0("#", object)
  #     }
  #
  #     statement <- rlocker::createStatement(list(
  #       verb =  verb,
  #       object = list(
  #         id = paste0(boastUtils::getCurrentAddress(session), object),
  #         name = paste0(APP_TITLE),
  #         description = description
  #       )
  #     ))
  #     print(statement)
  #     return(rlocker::store(session, statement))
  #   }
  #
  #   .generateAnsweredStatement <- function(session, verb = NA, object = NA, description = NA, interactionType = NA, response = NA, success = NA, completion = FALSE) {
  #     statement <- rlocker::createStatement(list(
  #       verb = verb,
  #       object = list(
  #         id = paste0(getCurrentAddress(session), "#", object),
  #         name = paste0(APP_TITLE),
  #         description = paste0("Question ", activeQuestion, ": ", description),
  #         interactionType = interactionType
  #       ),
  #       result = list(
  #         success = success,
  #         response = response,
  #         completion = completion
  #         # extensions = list(
  #         #   ref = "https://shinyapps.science.psu.edu/scoreMatrix",
  #         #value = paste(as.data.frame(scoreMatrix), collapse = ", ")
  #         #   )
  #       )
  #     )
  #     )
  #
  #     # print(statement)
  #     return(rlocker::store(session, statement))
  #   }
  #
  #   # Read in data and generate the first subset
  #   questionBank <-
  #     read.csv("questionBank.csv",
  #              stringsAsFactors = FALSE,
  #              as.is = TRUE)
  #   qSelected <-
  #     sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  #   gameSet <- questionBank[qSelected,]
  #
  #   # Program the Reset Button
  #   observeEvent(input$reset, {
  #     .generateStatement(session, object = "reset", verb = "interacted",
  #                        description = "game board has been reset.")
  #     .gameReset()
  #   })
  #
  #   # Render game Board / Attach Observers
  #   output$gameBoard <- renderUI({
  #     board <- list()
  #     index <- 1
  #
  #     sapply(1:GRID_SIZE, function(row) {
  #       sapply(1:GRID_SIZE, function(column) {
  #         id <- paste0("grid-", row, "-", column)
  #
  #         board[[index]] <<- tags$li(
  #           actionButton(
  #             inputId = paste0("grid-", row, "-", column),
  #             label = "?",
  #             color = "primary",
  #             style = "bordered",
  #             class = "grid-fill"
  #           ),
  #           class = "grid-tile"
  #         )
  #
  #         observeEvent(session$input[[id]], {
  #           activeBtn <<- id
  #           .boardBtn(id)
  #           .generateStatement(session, object = activeBtn, verb = "interacted",
  #                              description = paste0("Tile ", activeBtn,
  #                                                   " selected. Rendering question: ",
  #                                                   activeQuestion, "."))
  #         })
  #
  #         index <<- index + 1
  #       })
  #     })
  #
  #     tags$ol(board, class = paste(
  #       "grid-board",
  #       "grid-fill",
  #       paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
  #     ))
  #   })
  #
  #   # Program Submit Button
  #   observeEvent(input$submit, {
  #     index <- .tileIndex(activeBtn)
  #     answer <- ""
  #
  #     if (gameSet[index, "format"] == "numeric") {
  #       answer <- gameSet[index, "answer"]
  #     } else {
  #       answer <- gameSet[index, gameSet[index, "answer"]]
  #     }
  #
  #     success <- input$ans == answer
  #
  #     if (success) {
  #       updateButton(
  #         session = session,
  #         inputId = activeBtn,
  #         label = player,
  #         disabled = TRUE
  #       )
  #       scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
  #     } else {
  #       updateButton(
  #         session = session,
  #         inputId = activeBtn,
  #         label = opponent,
  #         disabled = TRUE
  #       )
  #       scoreMatrix <<- .score(scoreMatrix, activeBtn,-1)
  #     }
  #
  #     # Check for game over states
  #     .gameState <- .gameCheck(scoreMatrix)
  #     completion <- ifelse(.gameState == "continue", FALSE, TRUE)
  #     interactionType <- ifelse(gameSet[index,]$format == "numeric", "numeric",
  #                               "choice")
  #
  #     .generateAnsweredStatement(
  #       session,
  #       object = activeBtn,
  #       verb = "answered",
  #       description = gameSet[index,]$question,
  #       response = input$ans,
  #       interactionType = interactionType,
  #       success = success,
  #       completion = completion
  #     )
  #
  #     if (.gameState == "win") {
  #       .generateStatement(session, object = "game", verb = "completed",
  #                          description = "Player has won the game.")
  #       confirmSweetAlert(
  #         session = session,
  #         inputId = "endgame",
  #         title = "You Win!",
  #         text = "You've filled either a row, a column, or a main diagonal. Start over and play a new game.",
  #         btn_labels = "Start Over"
  #       )
  #     } else if (.gameState == "lose") {
  #       .generateStatement(session, object = "game", verb = "completed",
  #                          description = "Player has lost the game.")
  #       confirmSweetAlert(
  #         session = session,
  #         inputId = "endgame",
  #         title = "You lose :(",
  #         text = "Take a moment to review the concepts and then try again.",
  #         btn_labels = "Start Over"
  #       )
  #     } else if (.gameState == "draw") {
  #       .generateStatement(session, object = "game", verb = "completed",
  #                          description = "game has ended in a draw.")
  #       confirmSweetAlert(
  #         session = session,
  #         inputId = "endgame",
  #         title = "Draw!",
  #         text = "Take a moment to review the concepts and then try again.",
  #         btn_labels = "Start Over"
  #       )
  #     }
  #     updateButton(session = session,
  #                  inputId = "submit",
  #                  disabled = TRUE)
  #   })
  #
  #   observeEvent(input$tabs, {
  #     if (input$tabs == "game") {
  #       if (!gameProgress) {
  #         shinyalert(
  #           title = "Player Select",
  #           text = "Select whether you want to play as O or X.",
  #           showConfirmButton = TRUE,
  #           confirmButtonText = "Play as X",
  #           showCancelButton = TRUE,
  #           cancelButtonText = "Play as O"
  #         )
  #         gameProgress <<- TRUE
  #       }
  #     }
  #     .generateStatement(session, verb = "experienced",
  #                        description = paste0("Navigated to ", input$tabs, " tab."))
  #   }, ignoreInit = TRUE)
  #
  #   observeEvent(input$endgame, {
  #     .generateStatement(session, object = "endgame", verb = "interacted",
  #                        description = paste("game has been reset."))
  #     .gameReset()
  #   })
  #
  #   observeEvent(input$shinyalert, {
  #     if (input$shinyalert == TRUE) {
  #       player <<- "X"
  #       opponent <<- "O"
  #     }
  #     if (input$shinyalert == FALSE) {
  #       player <<- "O"
  #       opponent <<- "X"
  #     }
  #
  #     .generateStatement(session, object = "shinyalert", verb = "interacted",
  #                        description = paste0("User has selected player: ", player))
  #
  #     output$player <- renderUI({
  #       return(paste0("You are playing as ", player, "."))
  #     })
  #   })
  # }
  # # End of code-------------------------------------------------
}

# Boast app call ----
boastApp(ui = ui, server = server)