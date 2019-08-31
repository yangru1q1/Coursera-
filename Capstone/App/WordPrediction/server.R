#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(data.table)


function(input, output, session) {
    
    result <- reactive({
        myPrediction(input = input$myText)
    })
    
    output$pred1 <- renderUI({
        actionLink(inputId = "click1", label = result()[1])
    })
    
    output$pred2 <- renderUI({
        actionLink(inputId = "click2", label = result()[2])
    })
    
    output$pred3 <- renderUI({
        actionLink(inputId = "click3", label = result()[3])
    })
    
    # update the input
    updatedInput <- reactiveValues(input = "")
    observeEvent(input$click1, {
        updatedInput$input <- updateInput(input = input$myText, result()[1])
    })
    observeEvent(input$click2, {
        updatedInput$input <- updateInput(input = input$myText, result()[2])
    })
    observeEvent(input$click3, {
        updatedInput$input <- updateInput(input = input$myText, result()[3])
    })
    observe({
        updateTextInput(session, inputId = "myText", value = updatedInput$input)
    })
    
}
