library(shiny)
library(shinyjs) # manipulation with UI elements
library(caracas)
library(reticulate)
library(stringr)
library(dplyr)
library(plotly)
library(DT) # interactive HTML tables using data.frames
library(shinythemes)
library(rootSolve)
library(pracma)

withMathJax() # helps display equations




# UI:
source("ui/consumer_ui.R")
source("ui/COC_ui.R")         # (COC = Consumer's Optimal Choice)
source("ui/firm_ui.R")
source("ui/monopoly_ui.R")

# SERVER:
source("server/consumer_server.R")
source("server/COC_server.R")
source("server/firm_server.R")
source("server/monopoly_server.R")

# additional functions:
source("additional_functions/shared_functions.R") # global, for all parts of the app
source("additional_functions/consumer_+_COC.R") # for consumer theory and COC
source("additional_functions/firm_+_monopoly.R") # for firm theory and monopoly

# Modules:
source("modules/modalWindowModule.R") # for modal window needs


# we set the number of digits to 5, so that the results are more readable and not too long:
options(digits = 5)


# top bar:
ui <- navbarPage(
  theme = shinytheme("spacelab"), # design

  position = "fixed-top", # it's always visible, even when scrolling

  header = tags$head( # additional CSS settings
    tags$style(HTML("
      body {
        padding-top: 80px;
        padding-bottom: 80px;
      }

      pre {
        font-size: 16px;
        line-height: 1.5;
        padding: 10px;
      }

      .navbar {
        height: 60px !important;
      }

      .navbar .navbar-brand {
        height: 50px !important;      /* adjusts the logo height */
        padding: 10px 15px !important;
      }

      .navbar-nav > li > a {
        padding-top: 20px !important;  /* vertical alignment */
        padding-bottom: 18px !important;
      }

      .tagsSummary {
        cursor: pointer;
        font-weight: bold;
        color: #575757;
        background-color: none;
        padding: 8px 12px;
        margin-bottom: 5px;
        border-radius: 6px;
        border: 1px solid #bababa;
      }
    ")), # "pre" is for formatting "verbatimTextOutput"

    # MathJax and tippy.js are for displaying equations (internet connection
    # is therefore required, otherwise the equations won't be displayed properly):
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6")
  ),

  # footer:
  footer = tags$footer(
    p("Author: Marek Holub"),
    style = "position: fixed; bottom: 0; width: 100%; background-color: #f2f2f2;
            text-align: center; padding: 8px; font-size: 12px;
            color: #555; border-top: 1px solid #ccc; z-index: 1;"
  ),
  # the title with the logo:
  title = div(
    img(src = "logo.png", height = "40px", style = "margin-right: 20px;"),
    "Microeconomics App"
  ),
  # the individual tabs:
  tabPanel("Consumer Theory", consumer_ui),
  tabPanel("Consumer's Optimal Choice", COC_ui),
  tabPanel("Firm Theory", firm_ui),
  tabPanel("Monopoly", monopoly_ui)
)

server <- function(input, output, session) {
  consumer_server(input, output, session)
  COC_server(input, output, session)
  firm_server(input, output, session)
  monopoly_server(input, output, session)
}


shinyApp(ui, server)
