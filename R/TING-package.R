#' This function can build jitter plot between independent variables and
#' dependent variables.
#'
#' @param dataset Dataset name
#' @param xname One of independent variables
#' @param targetvariable One of dependent variables
#' @param title Sitle of jitter plot
#' @param subtitle Subtitle of jitter plot
#' @param caption Caption of jitter plot
#'
#' @return Jitter plot
#' @export
#'
#' @examples
#' Ting_graph(mydata, AL, MYOPIC, "Use AL to estimate myopia", "R program",
#' "Produced by Ting Wang")
Ting_graph <- function(dataset, xname, targetvariable, title, subtitle, caption)
{
  ggplot(dataset, aes(x = {{xname}}, y = {{targetvariable}})) +
    geom_jitter(shape = "O", position = position_jitter(height = 0)) +
    labs (title= title,
          subtitle= subtitle,
          caption= caption) +
    theme_bw()
}


#' This function is used to do the pairwise partial correlations for
#'     independent variables and dependent variables. In addition, it gives us
#'     the p value as well as statistic for each correlation.
#'
#' @param dataname The name of dataset
#' @param targetvariable One of independent variable
#'
#' @return p value from pairwise partial correlations
#' @export
#'
#' @examples
#' Ting_correlation(mydata, "MYOPIC")
Ting_correlation <- function(dataname, targetvariable)
{
  newdata <- data.frame(name = colnames(dataname))
  newdata$index <- 1:nrow(newdata)
  selection <- filter(newdata, name == {{targetvariable}})
  number <- selection %>% pull(index)
  correlations <- cor(dataname)
  pcor <- correlations[,number]
  data <- round(pcor,2)
  print(data)
}


#' This function is used to find significant correlations between independent
#'     variables and dependent variables.
#'
#' @param data.name Dataset name (dataset should include p vlaue from correlation
#'     analysis)
#'
#' @return Significant pairwise partial correlations
#' @export
#'
#' @examples
#' Ting_selection(data)
Ting_selection <- function(data.name)
{
  D1 <- data.frame(data.name)
  D1$newvalue <- as.numeric(D1$data.name)
  D1$select <- as.numeric(between(D1$newvalue, -0.05, 0.05))
  D2 <- D1 %>% filter(select == 0)
  D3 <- setNames(cbind(rownames(D2), D2, row.names = NULL),
                 c("Factor", "data", "newvalue", "select"))
  D3
}


#' This function is used to get regression line on scatter plot for independent
#'     variables and dependent variables.
#'
#' @param dataname Name of dataset
#' @param xname One of independent variables
#' @param yname One of dependent variables
#' @param xlab Label of x axis
#' @param ylab Label of y axis
#' @param title Title of plot
#' @param caption Caption of plot
#'
#' @return Scatter plot with regression line
#' @export
#'
#' @examples
#' Ting_regression(mydata, GENDER, MYOPIC, "GENDER", "MYOPIC", "Regression
#'     GENDER versus MYOPIC", "Produced by Ting Wang")
Ting_regression <- function(dataname, xname, yname, xlab, ylab, title, caption)
{
  ggplot(dataname, aes(x = {{xname}}, y = {{yname}})) +
    geom_point() +
    geom_smooth(method = "lm")+
    labs (title=title,
          xlab=xlab, ylab=ylab,
          caption=caption)
}


#' Build shiny APP that shows in each AGE group, the range of ACD that can cause
#'     the occurrence of MYOPIC.
#'
#' @return shiny APP shows that in each AGE group, the range of ACD that can cause
#'     the occurrence of MYOPIC.
#' @export
#'
#' @examples pickAGE()
pickAGE <- function(select, input, output)
{
  ui <- fluidPage(

    sidebarLayout(

      # Inputs
      sidebarPanel(

        # Put input here
        selectInput(inputId = 'AGE',
                    label = 'AGE',
                    choices = unique(mydata$AGE))

      ),

      # Outputs
      mainPanel(
        plotOutput('line'),
        br(), hr(), br(),
        plotOutput('hist')
      )
    )
  )


  # Define server logic required to draw a histogram
  server <- function(input, output) {

    dat <- reactive({
      mydata %>% filter(AGE == input$AGE)
    })

    output$line <- renderPlot({
      ggplot(dat(), aes(ACD, MYOPIC, group = AGE)) +
        geom_line() +
        geom_point() +
        labs(x = 'ACD',
             y = 'MYOPIC',
             title = 'ACD & MYOPIC')
    })

    output$hist <- renderPlot({
      ggplot(dat(), aes(MYOPIC)) +
        geom_bar(binwidth = 15) +
        labs(x = 'MYOPIC',
             title = 'Distribution of MYOPIC')
    })

  }
  # Run the application
  shinyApp(ui = ui, server = server)
}

