library(shiny)
library(MASS)
library(ggplot2)
library(ggExtra)
library(stringr)
library(mediation)

fixedPage(
  tags$head(
    tags$style(HTML("
      table {
      border-collapse: collapse;
      }
                    
      td, th {
      border: 1px solid #ddd;
      padding: 6px;
      }
                    
      th {
      padding-top: 6px;
      padding-bottom: 6px;
      text-align: left;
      background-color: #808080;
      color: white;
      }

      tr {
      padding-top: 6px;
      background-color: black;
      color: white
      }

    "))
  ),   
   # Application title
   titlePanel("Causality and Multiple Regression"),
   HTML('Want to use this app for teaching? Contact <a href="http://www.lrdc.pitt.edu/rottman/" target="_blank">Ben Rottman</a> for teaching materials, homework assignments, and a production version of this app.'),
   hr(),
   
   fixedRow(

      column(width=5,
         wellPanel(
           h4("Generate the Data:"),
           selectInput("RoleOfZ", "Role of Z (a dichotomous variable)", choices=c('Noise / Alternative Cause', 'Confound / Common Cause', 'Alternative Effect', 'Mediator / Mechanism', 'Interaction / Moderator', 'Common Effect')),
           selectInput("N", "Number of Observations", choices=c(10,20,50,100,500,1000), selected=100),
           radioButtons("DataGeneration", "Data Generation",
                        c("Perfect Data" = TRUE,
                          "Sampled (Approximate) Data" = FALSE)),
           
           hr(),
           selectInput("Domain", "Names of Variables", choices=c('Smoking - Heart Disease', 'Perseverence - LSAT', 'Anxiety - Depression')),
           checkboxInput('ChooseNames', 'Edit the variable names'),
           conditionalPanel(
             condition = "input.ChooseNames == true",
             
             textInput("XLabel", "Name of X", value = "Smoking", width = NULL),
             textInput("YLabel", "Name of Y", value = "Heart Disease", width = NULL, placeholder = NULL),
             textInput("ZLabel", "Name of Z", value = "Diet", width = NULL, placeholder = NULL),
             textInput("ZLabelG1", HTML('<span style="color:red">Name of Z Group 1</span>'), value = "Healthy Diet", width = NULL, placeholder = NULL),
             textInput("ZLabelG2", HTML('<span style="color:orange">Name of Z Group 2</span>'), value = "Unhealthy Diet", width = NULL, placeholder = NULL),
             actionButton("SubmitNames", "Submit Names")
           ),
           
           conditionalPanel(
             condition = '"a" == "b"',
             numericInput("hiddeninput", "hiddeninput", 10, min = 1, max = 1000000)
           ),
           
           hr(),
           
           conditionalPanel(
             condition = "input.RoleOfZ == 'Noise / Alternative Cause'",
             sliderInput("N_SlopeXonY", "Influence of X on Y", min = -3, max = 3, value = 1, step=.5),
             sliderInput("N_SlopeZonY", "Influence of Z on Y", min = -3, max = 3, value = 1, step=.5)
           ),
           conditionalPanel(
             condition = "input.RoleOfZ == 'Confound / Common Cause'",
             sliderInput("CC_SlopeXonY", "Influence of X on Y", min = -3, max = 3, value = 1, step=.5),
             sliderInput("CC_SlopeZonX", "Influence of Z on X", min = -3, max = 3, value = 3, step=.5),
             sliderInput("CC_SlopeZonY", "Influence of Z on Y", min = -3, max = 3, value = 3, step=.5)
           ),
           conditionalPanel(
             condition = "input.RoleOfZ == 'Alternative Effect'",
             sliderInput("AE_SlopeXonY", "Influence of X on Y", min = -3, max = 3, value = 1, step=.5),
             sliderInput("AE_SlopeXonZ", "Influence of X on Z", min = -3, max = 3, value = 1, step=.5)
           ),
           conditionalPanel(
             condition = "input.RoleOfZ == 'Mediator / Mechanism'",
             sliderInput("M_SlopeXonY", "Influence of X on Y", min = -3, max = 3, value = 1, step=.5),
             sliderInput("M_SlopeXonZ", "Influence of X on Z", min = -3, max = 3, value = 3, step=.5),
             sliderInput("M_SlopeZonY", "Influence of Z on Y", min = -3, max = 3, value = 3, step=.5)
           ),
           conditionalPanel(
             condition = "input.RoleOfZ == 'Common Effect'",
             sliderInput("CE_SlopeXonY", "Influence of X on Y", min = -3, max = 3, value = 0, step=.5),
             sliderInput("CE_SlopeXonZ", "Influence of X on Z", min = -3, max = 3, value = 3, step=.5),
             sliderInput("CE_SlopeYonZ", "Influence of Y on Z", min = -3, max = 3, value = 3)
           ),
           conditionalPanel(
             condition = "input.RoleOfZ == 'Interaction / Moderator'",
             sliderInput("I_SlopeXonY", "Influence of X on Y", min = -3, max = 3, value = -1, step=.5),
             sliderInput("I_SlopeZonY", "Influence of Z on Y", min = -3, max = 3, value = 0, step=.5),
             sliderInput("I_Interaction", "Interaction X and Z on Y", min = -3, max = 3, value = 2, step=.5)
           ),
           actionButton("ResampleData", "Resample Data")
           ),

         # hr(),
         
         wellPanel(
           h4("Causal Structure Graph:"),
           uiOutput("CSGraph", width = "210px", height = "250px")
         )
         
      ),
      
      
      # Show a plot of the generated distribution
      column(width=7,
         h4("Plot the Data:"),
         plotOutput("distPlot", width = "400px", height = "400px"), #width and height of graph also needs to be set below
         
         fixedRow(
           column(width=4,
                  radioButtons("GroupColors", "Colors for Z:",
                               c("Separate Colors", "All One Color"))),
           column(width=8, 
                  uiOutput("KeyText"))
         ),

         hr(),
         h4("What to Analyze?"),
         wellPanel(checkboxInput('AnalyzeXRegressZ', "[ X~Z ]: Simple Relation between Z and X"), 
                   conditionalPanel(
                     condition = "input.AnalyzeXRegressZ == true",
                     uiOutput("simple_relation_Z_X_HTML")
                     )
                   ),
         wellPanel(checkboxInput('AnalyzeYRegressZ', '[ Y~Z ]: Simple Relation between Z and Y'), 
                   conditionalPanel(
                     condition = "input.AnalyzeYRegressZ == true",
                     # verbatimTextOutput("simple_relation_Z_Y"))
                     uiOutput("simple_relation_Z_Y_HTML")
                   )
         ),
         wellPanel(checkboxInput('AnalyzeYRegressX', '[ Y~X ]: Simple Relation between X and Y'), 
                   conditionalPanel(
                     condition = "input.AnalyzeYRegressX == true",
                     # verbatimTextOutput("current_regression_unconditional")
                     uiOutput("current_regression_unconditional_HTML")
                  )
         ),
         wellPanel(checkboxInput('AnalyzeYRegressXPlusZ', '[ Y~X+Z ]: Regression using X and Z to predict Y'), 
                   conditionalPanel(
                     condition = "input.AnalyzeYRegressXPlusZ == true",
                     # verbatimTextOutput("current_regression_conditional")
                     uiOutput("current_regression_conditional_HTML")
                   )
         ),
         wellPanel(checkboxInput('AnalyzeInteraction', '[ Y~X+Y+X*Z ]: Regression using X and Z and interaction to predict Y'), 
                   conditionalPanel(
                     condition = "input.AnalyzeInteraction == true",
                     # verbatimTextOutput("asdfasfd")
                     uiOutput("current_regression_interaction_HTML")
                   )
         ),
         wellPanel(checkboxInput('AnalyzeMediation', 'Advanced Feature: Mediation Analysis'),
                   conditionalPanel(
                     condition = "input.AnalyzeMediation == true",
                     uiOutput("current_mediation_HTML"),
                     HTML("* The p-value works as expected. The estimate of the indirect effect gives numbers close to 0 when there is no indirect effect, as expected. However, when there is an indirect effect, the numbers are not the product of X->Z and Z->Y; this is a quirk of X being binary and should be ignored.")
                   )
         )
      ),
      fixedRow(
        HTML("<p style='font-size:75%;'>This material is based upon work supported by the National Science Foundation under Grant Number 1651330. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.</p>")
      )
   )
)


# Colors being used in various places
# #9e37eb Purple
# #00FF00 Green
# #FF00FF Magenta
# #2993fc Blue or "dodgerblue"
# #fffa40 Yellow

