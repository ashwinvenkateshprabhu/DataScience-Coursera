#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
        titlePanel("Simple Interest Calculator"),
        sidebarLayout(
                sidebarPanel(
                        helpText("This app calculates simple interest based on your inputs."),            
                        numericInput("num_principal", label = h6("Enter the principal amount (in $)"), value = 100),
                        sliderInput("slider_intrate", label = h6("Choose the yearly interest rate (in %)"), min = 0, max = 30, value = 2),
                        sliderInput("slider_num", label = h6("Choose the number of time periods"), min = 0, max = 20, value = 5),
                        selectInput("select_time", label = h6(""), choices = list("Years" = 1, "Quarters" = 2), selected = 1),
                        actionButton("action_Calc", label = "Refresh & Calculate")        
                ),
                
                mainPanel(
                        tabsetPanel(
                                tabPanel("Output",
                                         p(h5("Your entered values:")),
                                         textOutput("text_principal"),
                                         textOutput("text_intrate"),
                                         textOutput("text_num"),
                                         textOutput("text_time"),
                                         p(h5("Calculated values:")),
                                         textOutput("text_int"),
                                         textOutput("text_amt")
                                ),
                                tabPanel("Documentation",
                                         p(h4("Simple Interest Calculator:")),
                                         helpText("This application calculates simple interest and total amount, i.e. principal + interest."),
                                         HTML("<u><b>Method for calculation: </b></u>
                                              <br> <br>
                                              <b> Amt = Prpl + Int = Prpl(1 + rate * time) ; R = rate * 100 </b>
                                              <br>
                                              where: <br>
                                              Amt = Total amount (Principal + Interest) <br>
                                              Prpl = Principal amount <br>
                                              Int = Interest amount <br>
                                              rate = Rate of interest per year, in decimal; r=R/100 <br>
                                              time = Time period invested in years/quarters")                
                                )
                        )
                )
        )
))
