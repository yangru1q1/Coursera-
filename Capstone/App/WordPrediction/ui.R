#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("App", tabName = "app", icon = icon("medapps")),
        menuItem("Help", tabName = "help", icon = icon("question-circle")),
        menuItem("Info", tabName = "info", icon = icon("info-circle"))
    )
)


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "app",
                #
                fluidRow(column(12,  textInput(inputId = "myText", 
                                               label = h4("Input Text"), width = "100%"))),
                #
                fluidRow(
                    box(width = 4, status = "primary",
                        h5(uiOutput(outputId = "pred1"))),
                    box(width = 4, status = "warning",
                        h5(uiOutput(outputId = "pred2"))),
                    box(width = 4, status = "success",
                        h5(uiOutput(outputId = "pred3")))
                    )
        ),
        tabItem(tabName = "help",
                h2("How to use this app"),
                br(),
                p("Please input the text input in \"Input Text\" box."),
                br(),
                p("Just like Swiftkey Keyboard, this app offers 3 possible next word by given any text input."),
                br(),
                p("These given words are the top 3 predictions with highest possibilities calculated by backend algorithm, with probabilities of words decrease from left to right."),
                br(),
                p("Each words is clickable, the word will be automatically added into text if the user click on the word.")
                ),
        tabItem(tabName = "info",
                h2("Informations"),
                br(),
                p("This shiny app is the Capstone project for Coursera Data Science Specialization offered by John Hopskin University."),
                br(),
                p("The purpose of this app is predicting next word by given text input. The language model build with",
                  strong("Kneser-Ney Smoothing")),
                br(),
                p("For more informations, please check", a("App Slide Deck", href = "http://rpubs.com/yangru1q1/521989"))
                )
    )
)


dashboardPage(
    dashboardHeader(title = "Word Predictions"),
    sidebar,
    body,
    skin = "yellow"
)

