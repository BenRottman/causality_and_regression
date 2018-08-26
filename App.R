library(shiny)
library(MASS)
library(ggplot2)
library(ggExtra)
library(stringr)
library(mediation)

ui <- fixedPage(
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  XNAME<-   eventReactive(c(input$SubmitNames, input$hiddeninput), {input$XLabel}, ignoreNULL = FALSE)
  YNAME<-   eventReactive(c(input$SubmitNames, input$hiddeninput), {input$YLabel}, ignoreNULL = FALSE)
  ZNAME<-   eventReactive(c(input$SubmitNames, input$hiddeninput), {input$ZLabel}, ignoreNULL = FALSE)
  ZNAMEG1<- eventReactive(c(input$SubmitNames, input$hiddeninput), {input$ZLabelG1}, ignoreNULL = FALSE)
  ZNAMEG2<- eventReactive(c(input$SubmitNames, input$hiddeninput), {input$ZLabelG2}, ignoreNULL = FALSE)
  
  observe({
    r <- input$RoleOfZ
    s <- input$Domain
    updateNumericInput(session, "hiddeninput", value = runif(1, 0, 1000000))
  })
  
  # http://shiny.rstudio.com/gallery/update-input-demo.html
  observe({
    x <- input$RoleOfZ
    updateCheckboxInput(session, "AnalyzeXRegressZ", label = paste('[ X~Z ]: Simple Relation between Z (', ZNAME(), ') and X (', XNAME(), ')', sep=''))
    updateCheckboxInput(session, "AnalyzeYRegressZ", label = paste('[ Y~Z ]: Simple Relation between Z (', ZNAME(), ') and Y (', YNAME(), ')', sep=''))
    updateCheckboxInput(session, "AnalyzeYRegressX", label = paste('[ Y~X ]: Simple Relation between X (', XNAME(), ') and Y (', YNAME(), ')', sep=''))
    updateCheckboxInput(session, "AnalyzeYRegressXPlusZ", label = paste('[ Y~X+Z ]: Regression using X (', XNAME(), ') and Z (', ZNAME(), ') to predict Y (', YNAME(), ')', sep=''))
    updateCheckboxInput(session, "AnalyzeYRegressInteraction", label = paste('[ Y~X+Z+X*Z ]: Regression using X (', XNAME(), ') and Z (', ZNAME(), ') and interaction to predict Y (', YNAME(), ')', sep=''))
      })
  
  observe({
    if (input$RoleOfZ == 'Noise / Alternative Cause'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Diet", "Healthy Diet", "Unhealthy Diet")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Sleep", "More Sleep", "Less Sleep")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelG1", value = L[4])
      updateTextInput(session, "ZLabelG2", value = L[5])
      

    } else if (input$RoleOfZ == 'Confound / Common Cause'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Stress", "Not Stressed", "Stressed")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Stress", "More Stress", "Less Stress")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelG1", value = L[4])
      updateTextInput(session, "ZLabelG2", value = L[5])
    
    } else if (input$RoleOfZ == 'Mediator / Mechanism'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Endothelium Damage", "Endothelium Not Damaged", "Endothelium Damaged")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Study", "Little Studying", "Lots of Studying")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelG1", value = L[4])
      updateTextInput(session, "ZLabelG2", value = L[5])

    } else if (input$RoleOfZ == 'Alternative Effect'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Teeth Color", "Normal Teeth", "Yellow Teeth")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Excercise Frequency", "Infrequent", "Frequent") #stress could affect perseverance through procrastination
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelG1", value = L[4])
      updateTextInput(session, "ZLabelG2", value = L[5])
      
    } else if (input$RoleOfZ == 'Common Effect'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Death", "Normal Death", "Early Death")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Law School Admission", "Not Admitted", "Admitted") #stress could affect perseverance through procrastination
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelG1", value = L[4])
      updateTextInput(session, "ZLabelG2", value = L[5])
      
    } else if (input$RoleOfZ == 'Interaction / Moderator'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Activity", "Active", "Sedentary")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Practice Tests", "Do Not Take Practice Tests", "Take Practice Tests")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelG1", value = L[4])
      updateTextInput(session, "ZLabelG2", value = L[5])
      
    }
  })
  
  
  currentDF <- reactive({
    input$ResampleData
    N <- as.integer(input$N)

    if(input$RoleOfZ == 'Noise / Alternative Cause'){
      SlopeXonY<-input$N_SlopeXonY
      SlopeZonY<-input$N_SlopeZonY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical= input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      
      Z<-c(rep(-.5,N/2),rep(.5,N/2))*VarNoiseZ
      X<-NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + NoiseY
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 1"
      Z[Z==TRUE]<-"Group 2"
    }
    
    if(input$RoleOfZ == 'Confound / Common Cause'){
      SlopeXonY<-input$CC_SlopeXonY
      SlopeZonX<-input$CC_SlopeZonX
      SlopeZonY<-input$CC_SlopeZonY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      Z<-c(rep(-.5,N/2),rep(.5,N/2))*VarNoiseZ
      X<-SlopeZonX*Z + NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + NoiseY
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 1"
      Z[Z==TRUE]<-"Group 2"
    }
    
    #I used the same model to generate the mechanism as the confound, even though technically it is not generated from causes to effects
    #The reason is that these two models are markov equivalent so the data should work either way 
    #Also, it is confusing to generate a binary variable (Z) when it is not exogenous - would have to designate a cutoff, which essentially adds noise into the system and messes with the parameters
    #Possibley come back to this and instead of having a binary variable as 1/0, actually make it a multiple of X, so it could be something like +2-/2 if X->Z link is 2, or +3/-3 if X->Z link is 3. I think this would fix the mediation problem.
    if(input$RoleOfZ == 'Mediator / Mechanism'){
      SlopeXonY<-input$M_SlopeXonY
      SlopeZonX<-input$M_SlopeXonZ 
      SlopeZonY<-input$M_SlopeZonY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      Z<-c(rep(-.5,N/2),rep(.5,N/2))
      X<-SlopeZonX*Z + NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + NoiseY
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 1"
      Z[Z==TRUE]<-"Group 2"
    }

    if(input$RoleOfZ == 'Alternative Effect'){
      #technically I am modeling this like Z->X->Y because if I do Y<-X->Z, then have to deal with Z being binary and the simple relation between X and Z gets messed up; see the note above for mediation model. The same note applies here as well
      #However, because Z->X->Y and Z<-X->Y are Markov equivalent, the parameters will work out ok.
      SlopeXonY<-input$AE_SlopeXonY
      SlopeZonX<-input$AE_SlopeXonZ #see note above - doing a similar thing for the mechanism / mediator case
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      Z<-c(rep(-.5,N/2),rep(.5,N/2))
      X<-SlopeZonX*Z + NoiseX
      Y<-SlopeXonY*X + NoiseY
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 1"
      Z[Z==TRUE]<-"Group 2"
    }
    
    if(input$RoleOfZ == 'Common Effect'){
      SlopeXonY<-input$CE_SlopeXonY
      SlopeXonZ<-input$CE_SlopeXonZ
      SlopeYonZ<-input$CE_SlopeYonZ
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,9),3,3)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseZ
      sigma[3,3]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N), mu=rep(0,3), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      X<-d[,1]
      NoiseZ<-d[,2]
      NoiseY<-d[,3]
      
      Y<-SlopeXonY*X + NoiseY
      Z<-SlopeXonZ*X + SlopeYonZ*Y + NoiseZ
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 1"
      Z[Z==TRUE]<-"Group 2"
    }
    
    if(input$RoleOfZ == 'Interaction / Moderator'){
      SlopeXonY<-input$I_SlopeXonY
      SlopeZonY<-input$I_SlopeZonY
      Interaction<-input$I_Interaction
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical= input$DataGeneration)
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      Z<-c(rep(0,N/2),rep(1,N/2))*VarNoiseZ
      X<-NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + Interaction*X*Z + NoiseY
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 1"
      Z[Z==TRUE]<-"Group 2"
    }
    
    df<-data.frame(X,Y,Z)
    return(df)
  })
  
   simple_relation_Z_X <- reactive({ summary(lm(X~Z, data=currentDF() )) })
   simple_relation_Z_Y <- reactive({ summary(lm(Y~Z, data=currentDF() )) })
   current_regression_unconditional <- reactive({ summary(lm(Y~X, data=currentDF() )) })  
   current_regression_conditional <- reactive({ summary(lm(Y~X+Z, data=currentDF() )) })
   current_regression_interaction <- reactive({ summary(lm(Y~X*Z, data=currentDF() )) })
   
   # This additional dataframe is necessary for the mediation analysis which does not run if Z is a factor
   currentDFnumeric <- reactive({ 
     df <- currentDF()
     df$Z<-as.numeric(df$Z)
     return(df)
     })
   
   current_mediation.reg1<-reactive({ lm(Z ~ X, data=currentDFnumeric() ) })
   current_mediation.reg2<-reactive({ lm(Y ~ X + Z, data=currentDFnumeric() ) })
   current_mediation <- reactive({ summary(med.out <- mediate(current_mediation.reg1() , current_mediation.reg2(), treat = "X", mediator = "Z", sims = 100))  })
   
   output$distPlot <- renderPlot({
     df<-currentDF()
     
     #unconditional
     regression_unconditional<-current_regression_unconditional()
     intercept_unconditional<-coefficients(regression_unconditional)[1,1]
     XSlope_unconditional<-coefficients(regression_unconditional)[2,1]
     
     #conditional
     regression_conditional<-current_regression_conditional()
     intercept_conditional<-coefficients(regression_conditional)[1,1]
     XSlope_conditional<-coefficients(regression_conditional)[2,1]
     ZTrueEffect_conditional<-coefficients(regression_conditional)[3,1]

     #interaction
     regression_interaction<-current_regression_interaction()
     intercept_I_G1<-        coefficients(regression_interaction)[1,1]
     XSlope_I_G1<-           coefficients(regression_interaction)[2,1]
     intercept_diff_I_G2<-   coefficients(regression_interaction)[3,1]
     interaction_I<-         coefficients(regression_interaction)[4,1]
     
     # Intercepts
     df_intercept_unconditional <-data.frame(X=c(0), Y=c(intercept_unconditional))
     df_intercept_conditional   <-data.frame(X=c(0), Y=c(intercept_conditional))
     df_intercept_interaction   <-data.frame(X=c(0), Y=c(intercept_I_G1))
     
     # CI and Prediction intervals
     # http://www2.stat.duke.edu/~tjl13/s101/slides/unit6lec3H.pdf
     # https://rstudio-pubs-static.s3.amazonaws.com/71339_d0b8346f41314979bc394448c5d60d86.html
     
     # ConfidenceIntervals for Unconditional
     tX<-seq(min(df$X), max(df$X), 0.05)
     CI_Unconditional_Temp <- data.frame(X = tX)
     CI_Unconditional <- predict(lm(Y~X, data=df), CI_Unconditional_Temp, interval="confidence",level = 0.95)
     CI_Unconditional <- cbind(CI_Unconditional, CI_Unconditional_Temp)
     
     # Confidence Intervals for Conditional
     temp1<-subset(df, Z=='Group 1')
     tX<-seq(min(temp1$X), max(temp1$X), 0.05)
     CI_Conditional_Temp_ZFalse <- data.frame(X = tX, Z = rep('Group 1',length(tX)))
     CI_Conditional_ZFalse <- predict(lm(Y~X+Z, data=df), CI_Conditional_Temp_ZFalse, interval="confidence",level = 0.95)
     CI_Conditional_ZFalse <- cbind(CI_Conditional_ZFalse, CI_Conditional_Temp_ZFalse)
     
     temp2<-subset(df, Z=='Group 2')
     tX<-seq(min(temp2$X), max(temp2$X), 0.05)
     CI_Conditional_Temp_ZTrue <- data.frame(X = tX, Z = rep('Group 2',length(tX)))
     CI_Conditional_ZTrue <- predict(lm(Y~X+Z, data=df), CI_Conditional_Temp_ZTrue, interval="confidence",level = 0.95)
     CI_Conditional_ZTrue <- cbind(CI_Conditional_ZTrue, CI_Conditional_Temp_ZTrue)
     
     # Confidence Intervals for Interaction
     temp3<-subset(df, Z=='Group 1')
     tX<-seq(min(temp3$X), max(temp3$X), 0.05)
     CI_Interaction_Temp_ZFalse <- data.frame(X = tX, Z = rep('Group 1',length(tX)))
     CI_Interaction_ZFalse <- predict(lm(Y~X, data=temp3), CI_Interaction_Temp_ZFalse, interval="confidence",level = 0.95)
     CI_Interaction_ZFalse <- cbind(CI_Interaction_ZFalse, CI_Interaction_Temp_ZFalse)
     
     temp4<-subset(df, Z=='Group 2')
     tX<-seq(min(temp4$X), max(temp4$X), 0.05)
     CI_Interaction_Temp_ZTrue <- data.frame(X = tX, Z = rep('Group 2',length(tX)))
     CI_Interaction_ZTrue <- predict(lm(Y~X, data=temp4), CI_Interaction_Temp_ZTrue, interval="confidence",level = 0.95)
     CI_Interaction_ZTrue <- cbind(CI_Interaction_ZTrue, CI_Interaction_Temp_ZTrue)
     
     # dots for the curve for the interaction
     dot_Interaction_G1_X <- 1
     dot_Interaction_G1_Y <- intercept_I_G1 + 1*XSlope_I_G1
     dot_Interaction_G2_X <- 1
     dot_Interaction_G2_Y <- intercept_I_G1 + intercept_diff_I_G2 + 1*(XSlope_I_G1+interaction_I) + .01 #.01 added so points are never exactly the same
     
     df_int_curve<-data.frame(x1=dot_Interaction_G1_X, x2=dot_Interaction_G2_X, y1=dot_Interaction_G1_Y, y2=dot_Interaction_G2_Y)
     
     # otherwise curve for interaction switches on the left vs. right sided curve
     if (dot_Interaction_G1_Y < dot_Interaction_G2_Y) {
       curvature_int <- .3
       show_interaction_curve <- TRUE
     } else if (dot_Interaction_G1_Y > dot_Interaction_G2_Y){
       curvature_int <- -.3
       show_interaction_curve <- TRUE
     } else if (dot_Interaction_G1_Y == dot_Interaction_G2_Y){
       show_interaction_curve <- FALSE
     }
     
     # Conditional Effect of Z in Interaction Model
     dfvert_int <- data.frame(x1 = 0, x2 = 0, Z1 = intercept_I_G1, Z2 = intercept_I_G1 + intercept_diff_I_G2)
     # data frame for two dots for vertical line
     df_vertical_int<-data.frame(X=c(0,0), Y=c(intercept_I_G1, intercept_I_G1 + intercept_diff_I_G2))
          
     
     # Conditional Effect of Z
     # data frame for the vertical line
     dfvert <- data.frame(x1 = 0, x2 = 0, Z1 = intercept_conditional, Z2 = intercept_conditional + ZTrueEffect_conditional)
     # data frame for two dots for vertical line
     df_vertical<-data.frame(X=c(0,0), Y=c(intercept_conditional, intercept_conditional + ZTrueEffect_conditional))

     # help with naming groups
     # https://stackoverflow.com/questions/18060116/adding-legend-to-ggplot-when-lines-were-added-manually
     # note - color needs to be inside aes call for this to work
     
     # data frame for two dots for AnalyzeXRegressZ
     df1AnalyzeXRegressZ<-data.frame(Y=c(6,6), X=c(simple_relation_Z_X()$coefficients[1,1]-.02, simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1]))
     # data frame for the vertical line for AnalyzeYRegressZ
     df2AnalyzeXRegressZ <- data.frame(y1 = 6, y2 = 6, x1 = simple_relation_Z_X()$coefficients[1,1]-.02, x2 = simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1])
     # data frame for two dots for AnalyzeYRegressZ
     # df1AnalyzeYRegressZ<-data.frame(X=c(4,4), Y=c(1, 2))
     df1AnalyzeYRegressZ<-data.frame(X=c(6,6), Y=c(simple_relation_Z_Y()$coefficients[1,1]-.02, simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1]))
     # data frame for the vertical line for AnalyzeYRegressZ
     df2AnalyzeYRegressZ <- data.frame(x1 = 6, x2 = 6, y1 = simple_relation_Z_Y()$coefficients[1,1]-.02, y2 = simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1]) 

     # first plot
     p <- ggplot(df, aes(X, Y))
     p <- p + labs(x = paste('X (', XNAME(), ')', sep=''), y=paste('Y (', YNAME(), ')', sep='') )
     
     # One of two colors for dots
     if (input$GroupColors=="Separate Colors"){
       p <- p + geom_point(aes(color = Z), size=2)
       p <- p + scale_color_manual(values=c("Group 1"="red", "Group 2"="orange", "Overall"="purple", "Effect of Z"="green"),
                                   labels=c('Group 1'=ZNAMEG1(),
                                            'Group 2'=ZNAMEG2(),
                                            "Overall"=paste( "Simple effect of X (", XNAME(), ")", sep=""),
                                            "Effect of Z"=paste("Effect of Z (", ZNAME(), ")", sep="")
                                            ),
                                   name='')
     } else if (input$GroupColors=="All One Color"){
       p <- p + geom_point(data=df, aes(X, Y, color = Z), alpha=0)
       p <- p + geom_point(data=df, aes(X, Y), color="purple", alpha=1)
     }

     p <- p + theme(axis.title=element_text(size=14),
                    legend.position="none",
                    panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    plot.margin = unit(c(.1,.1,.1,.1), "cm")) #reduce spacing around legend

     
     # axes
     p<- p + scale_x_continuous(breaks=c(-6:6), lim=c(-6,6))
     p<- p + scale_y_continuous(breaks=c(-6:6), lim=c(-6,6))

     #Simple X~Z and Y~Z
     if((input$AnalyzeXRegressZ == TRUE) & (input$AnalyzeYRegressZ == TRUE)){
       p <- p + geom_vline(xintercept= simple_relation_Z_X()$coefficients[1,1]-.02, color="red", size=1.5)
       p <- p + geom_vline(xintercept= simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1], color="orange", size=1.5)

       p <- p + geom_hline(yintercept= simple_relation_Z_Y()$coefficients[1,1]-.02, color="red", size=1.5)
       p <- p + geom_hline(yintercept= simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1], color="orange", size=1.5)
       
       p <- p + geom_point(aes(x=X, y=Y), size=3, color = "black", data=df1AnalyzeXRegressZ)
       p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size=1.5, data = df2AnalyzeXRegressZ, color='black', linetype="dashed")
       
       p <- p + geom_point(aes(x=X, y=Y), size=3, color = "black", data=df1AnalyzeYRegressZ)
       p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size=1.5, data = df2AnalyzeYRegressZ, color='black', linetype="dotted")
       
       p <- p + geom_rug(aes(color = Z), sides='rt')

     }
     
     #Simple X~Z
     else if (input$AnalyzeXRegressZ == TRUE){
       p <- p + geom_vline(xintercept= simple_relation_Z_X()$coefficients[1,1]-.02, color="red", size=1.5)
       p <- p + geom_vline(xintercept= simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1], color="orange", size=1.5)
       
       p <- p + geom_point(aes(x=X, y=Y), size=3, color = "black", data=df1AnalyzeXRegressZ)
       p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size=1.5, data = df2AnalyzeXRegressZ, color='black', linetype="dashed")
       
       p <- p + geom_rug(aes(color = Z), sides='t')
     }
     
     #Simple Y~Z
     else if (input$AnalyzeYRegressZ == TRUE){
       p <- p + geom_hline(yintercept= simple_relation_Z_Y()$coefficients[1,1]-.02, color="red", size=1.5)
       p <- p + geom_hline(yintercept= simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1], color="orange", size=1.5)

       p <- p + geom_point(aes(x=X, y=Y), size=3, color = "black", data=df1AnalyzeYRegressZ)
       p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size=1.5, data = df2AnalyzeYRegressZ, color='black', linetype="dotted")
       
       p <- p + geom_rug(aes(color = Z), sides='r') 
     }

     #Unconditional Regression Y~X
     if (input$AnalyzeYRegressX == TRUE){
       p <- p + geom_abline(aes(intercept = intercept_unconditional, slope = XSlope_unconditional, color="Overall"))
       p <- p + geom_ribbon(aes(x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "purple", alpha = 0.4, data=CI_Unconditional)
       p <- p + geom_point(aes(x=X, y=Y), shape=3, size=5, stroke=1.5, color="black", data=df_intercept_unconditional)
     }
     
     #Conditional Regression Y~X+Z
     if (input$AnalyzeYRegressXPlusZ == TRUE){
       #Group1
       p <- p + geom_abline(intercept = intercept_conditional, slope = XSlope_conditional, color='magenta', linetype = "solid")
       p <- p + geom_ribbon(aes(x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "red", alpha = 0.4, data=CI_Conditional_ZFalse)
       #Group2
       p <- p + geom_abline(intercept = intercept_conditional+ZTrueEffect_conditional, slope = XSlope_conditional, color='magenta', linetype = "solid")
       p <- p + geom_ribbon(aes(x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "orange", alpha = 0.4, data=CI_Conditional_ZTrue)
       
       #Vertical Line Effect of Z
       p <- p + geom_point(aes(x=X, y=Y, color = "Effect of Z"), size=3, data=df_vertical)
       p <- p + geom_segment(aes(x = x1, y = Z1, xend = x2, yend = Z2), size=1, data = dfvert, color='green')
       
       #Intercept
       p <- p + geom_point(aes(x=X, y=Y), shape=4, size=5, stroke=1.5, color="black", data=df_intercept_conditional)
     }
     
     #Interaction Regression Y~X*Z
     if (input$AnalyzeInteraction == TRUE){
       #Group1
       p <- p + geom_abline(intercept = intercept_I_G1, slope = XSlope_I_G1, color='red', linetype = "solid")
       p <- p + geom_ribbon(aes(x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "red", alpha = 0.4, data=CI_Interaction_ZFalse)
       #Group2
       p <- p + geom_abline(intercept = (intercept_I_G1+intercept_diff_I_G2), slope = (XSlope_I_G1+interaction_I), color='orange', linetype = "solid")
       p <- p + geom_ribbon(aes(x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "orange", alpha = 0.4, data=CI_Interaction_ZTrue)
       #Interaction Curve
       if (show_interaction_curve == TRUE){
         p <- p + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_int_curve, curvature = curvature_int, color="dodgerblue", linetype = "solid", size=1.1)
       }

       #Vertical Line Effect of Z
       p <- p + geom_point(aes(x=X, y=Y), size=3, data=df_vertical_int, color='yellow')
       p <- p + geom_segment(aes(x = x1, y = Z1, xend = x2, yend = Z2), size=1, data = dfvert_int, color='yellow')
       
       #Intercept
       p <- p + geom_point(aes(x=X, y=Y), shape=5, size=3, stroke=1.5, color="black", data=df_intercept_interaction)
     }
    
     # calls to ggMarginal must come at the very end of the graph, so going through conditional logic again
     if((input$AnalyzeXRegressZ == TRUE) & (input$AnalyzeYRegressZ == TRUE)){
       p <- ggMarginal(p, type = c("density"), groupColour = TRUE, groupFill = TRUE, size=10, margins = c("both"))
     }
     else if (input$AnalyzeXRegressZ == TRUE){
       p <- ggMarginal(p, type = c("density"), groupColour = TRUE, groupFill = TRUE, size=10, margins = c("x"))
     }
     else if (input$AnalyzeYRegressZ == TRUE){
       p <- ggMarginal(p, type = c("density"), groupColour = TRUE, groupFill = TRUE, size=10, margins = c("y"))
     }
     
     #Final return of the graph
     return(p)

   }, width = 400, height = 400  #width and height also need to be set above in "plotOutput" even though that feels redundant
) 
   

#### Below is the html for how all the regression tables are produced.
## First an html table is made
## Then certain key words are replaced dynamically based on regression results and variable names

## Z~X
   
simple_relation_Z_X_HTML.Model<-'
This regression result is essentially the same thing as running a t-test comparing X on the two different groups of Z. The only difference is that B is not standardized between 0 and 1, but the p-value is the same.
<table>
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B: difference between means</th>
    <th>p-value</th></tr>
  <tr>
    <td style="text-align:left">Z (ZNAME)</td>
    <td style="color:white">Z.B <br> Dashed line: - - -</td>
    <td>Z.p</td>
    </tr>
</table>'


output$simple_relation_Z_X_HTML <- 
  renderText({str_replace_all(simple_relation_Z_X_HTML.Model,
                              c("Z.B" = toString(formatC(simple_relation_Z_X()$coefficients[2,1], 
                                                         format = 'f', digits=3)),
                                "Z.p" = toString(formatC(simple_relation_Z_X()$coefficients[2,4], 
                                                         format = 'f', digits=3)),
                                "ZNAME" = ZNAME()
                                ))
    })
   

## Z~Y

simple_relation_Z_Y_HTML.Model<-'
This regression result is essentially the same thing as running a t-test comparing Y on the two different groups of Z.
<table>
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B: difference between means</th>
    <th>p-value</th>
  </tr>
  <tr>
    <td style="text-align:left">Z (ZNAME)</td>
    <td style="color:white" bgcolor="black">Z.B <br>Dotted line: &#8901&#8901&#8901&#8901&#8901</td>
    <td>Z.p</td>
  </tr>
</table>'

output$simple_relation_Z_Y_HTML <- 
  renderText({str_replace_all(simple_relation_Z_Y_HTML.Model,
                              c("Z.B" = toString(formatC(simple_relation_Z_Y()$coefficients[2,1], 
                                                         format = 'f', digits=3)),
                                "Z.p" = toString(formatC(simple_relation_Z_Y()$coefficients[2,4], 
                                                         format = 'f', digits=3)),
                                "ZNAME" = ZNAME()
                                ))
    })

## Y~X

current_regression_unconditional_HTML.Model<-'
This regression result is essentially the same thing as running a correlation between X and Y.
<table>
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B</th>
    <th>p-value</th>
  </tr>
  <tr>
    <td style="text-align:left">Intercept</td>
    <td>I.B &#xff0b</td>
    <td>I.p</td>
  </tr>
  <tr>
    <td style="text-align:left">X (XNAME)</td>
    <td style="color:#9e37eb" bgcolor="black">X.B</td>
    <td>X.p</td>
  </tr>
</table>'

output$current_regression_unconditional_HTML <- 
  renderText({str_replace_all(current_regression_unconditional_HTML.Model,
                              c("I.B" = toString(formatC(current_regression_unconditional()$coefficients[1,1], 
                                                         format = 'f', digits=3)),
                                "I.p" = toString(formatC(current_regression_unconditional()$coefficients[1,4], 
                                                         format = 'f', digits=3)),
                                "X.B" = toString(formatC(current_regression_unconditional()$coefficients[2,1], 
                                                         format = 'f', digits=3)),
                                "X.p" = toString(formatC(current_regression_unconditional()$coefficients[2,4], 
                                                         format = 'f', digits=3)),
                                "XNAME" = XNAME()
                                ))
    })


## Y~X+Z

current_regression_conditional_HTML.Model<-'
<table>
<tr><th style="text-align:left">Predictor</th><th>B</th><th>p-value</th></tr>
<tr>
  <td style="text-align:left">Intercept</td>
  <td>I.B &#x2715</td>
  <td>I.p</td>
</tr>
<tr>
  <td style="text-align:left">X (XNAME)</td>
  <td style="color:#FF00FF" bgcolor="black">X.B</td>
  <td>X.p</td>
</tr>
<tr>
  <td style="text-align:left">Z (ZNAME)</td>
  <td style="color:#00FF00" bgcolor="black">Z.B</td>
  <td>Z.p</td>
</tr>
</table>'

output$current_regression_conditional_HTML <- 
  renderText({str_replace_all(current_regression_conditional_HTML.Model,
                              c("I.B" = toString(formatC(current_regression_conditional()$coefficients[1,1], 
                                                         format = 'f', digits=3)),
                                "I.p" = toString(formatC(current_regression_conditional()$coefficients[1,4], 
                                                         format = 'f', digits=3)),
                                "X.B" = toString(formatC(current_regression_conditional()$coefficients[2,1], 
                                                           format = 'f', digits=3)),
                                "X.p" = toString(formatC(current_regression_conditional()$coefficients[2,4], 
                                                           format = 'f', digits=3)),
                                "Z.B" = toString(formatC(current_regression_conditional()$coefficients[3,1], 
                                                           format = 'f', digits=3)),
                                "Z.p" = toString(formatC(current_regression_conditional()$coefficients[3,4], 
                                                           format = 'f', digits=3)),
                                "XNAME" = XNAME(),
                                "ZNAME" = ZNAME()
                              ))
    })

## Y~X*Z

current_regression_interaction_HTML.Model<-'
<table>
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B</th>
    <th>p-value</th>
  </tr>
  <tr>
    <td style="text-align:left">Intercept</td>
    <td>I.B &#x25c7;</td>
    <td>I.p</td>
  </tr>
  <tr>
    <td style="text-align:left">X (XNAME)</td>
    <td style="color:red" bgcolor="black">X.B</td>
    <td>X.p</td>
  </tr>
  <tr>
    <td style="text-align:left">Z (ZNAME)</td>
    <td style="color:yellow" bgcolor="black">Z.B</td>
    <td>Z.p</td>
  </tr>
  <tr>
    <td style="text-align:left">Interaction X*Z</td>
    <td style="color:dodgerblue" bgcolor="black">Int.B</td>
    <td>Int.p</td>
  </tr>
</table>'

output$current_regression_interaction_HTML <- 
  renderText({str_replace_all(current_regression_interaction_HTML.Model,
                              c("I.B" = toString(formatC(current_regression_interaction()$coefficients[1,1], 
                                                         format = 'f', digits=3)),
                                "I.p" = toString(formatC(current_regression_interaction()$coefficients[1,4], 
                                                         format = 'f', digits=3)),
                                "X.B" = toString(formatC(current_regression_interaction()$coefficients[2,1], 
                                                         format = 'f', digits=3)),
                                "X.p" = toString(formatC(current_regression_interaction()$coefficients[2,4], 
                                                         format = 'f', digits=3)),
                                "Z.B" = toString(formatC(current_regression_interaction()$coefficients[3,1], 
                                                         format = 'f', digits=3)),
                                "Z.p" = toString(formatC(current_regression_interaction()$coefficients[3,4], 
                                                         format = 'f', digits=3)),
                                "Int.B" = toString(formatC(current_regression_interaction()$coefficients[4,1], 
                                                         format = 'f', digits=3)),
                                "Int.p" = toString(formatC(current_regression_interaction()$coefficients[4,4], 
                                                         format = 'f', digits=3)),
                                "XNAME" = XNAME(),
                                "ZNAME" = ZNAME()
                              ))
  })

#Mediation Analysis

current_mediation_HTML.Model<-'
<table>
  <tr>
    <th>Estimate of Indirect Effect X&#8594Z&#8594Y</th>
    <th>p-value</th>
  </tr>
  <tr>
    <td bgcolor="black">Mediation.Indirect.B *</td>
    <td>Mediation.Indirect.p</td>
  </tr>
</table>'

#d is the mediation effect, z is the direct effect, not sure why - see mediation package
output$current_mediation_HTML <- 
  renderText({str_replace_all(current_mediation_HTML.Model,
                              c("Mediation.Indirect.B" = toString(formatC(current_mediation()$d1, 
                                                         format = 'f', digits=3)),
                                "Mediation.Indirect.p" = toString(formatC(current_mediation()$d1.p, 
                                                         format = 'f', digits=3))
                              ))
  })

## The Key/Legend for the graph is made with text, not an image, and changes based on whether one or two colors are chosen

output$KeyText <-renderText({
  if (input$GroupColors=="Separate Colors"){
    KT<-('<b>Key for Z:</b><br>
<table style="padding: 0px;">
  <tr>
    <td style="border:0px solid white; background-color:white; color:red">&#9679</td>
    <td style="border:0px solid white; background-color:white; color:black">Z Group 1 (ZNAMEG1)</td>
  </tr>
  <tr>
    <td style="border:0px solid white; background-color:white; color:orange">&#9679</td>
    <td style="border:0px solid white; background-color:white; color:black">Z Group 2 (ZNAMEG2)</td>
  </tr>
</table>
'
         )
  } else if (input$GroupColors=="All One Color"){
    KT<-('<b>Key for Z:</b><br>
<table style="padding: 0px;">
  <tr>
    <td style="border:0px solid white; background-color:white; color:#9e37eb">&#9679</td>
    <td style="border:0px solid white; background-color:white; color:black">Z</td>
  </tr>
</table>
'
         )
  }
  return(str_replace_all(KT,
                         c("ZNAMEG1" = ZNAMEG1(),
                           "ZNAMEG2" = ZNAMEG2()
                           ))
         )
  })

#### The Causal Structure Graph is made with SVG
##   First a bunch of stuff is added.
##   After, depending on the structure and the sliders, things are hidden and colors changed
##   The text inside <!-- --> are comments - don't delete it!
   
CSGraph<-'
<svg width="210px" height="250px">
  <defs>
<marker id="arrow" markerWidth="10" markerHeight="10" refX="0" refY="3" orient="auto" markerUnits="strokeWidth">
<path d="M0,0 L0,6 L9,3 z" fill="white" />
</marker>
</defs>
<!-- The Background -->
<rect width="100%" height="100%" fill="black"  stroke="#000" stroke-width="1"/>

<!-- Coloring the lines is easy but Im not sure how to also color the triangles at the end  -->
<!-- this looks hard http://bl.ocks.org/kenpenn/8d782030e4be9d832be7  -->

<!-- The Arrows -->
<!-- X->Y -->
<line x1="30" y1="75" x2="30" y2="148" stroke="white" stroke-width="2" opacity="OP.XY" marker-end="url(#arrow)" />

<!-- Z->Y -->
<line x1="150" y1="120" x2="58" y2="162" stroke="white" stroke-width="2" opacity="OP.ZY" marker-end="url(#arrow)" /> 
<!-- Y->Z -->
<line x1="30" y1="180" x2=123 y2="138" stroke="white" stroke-width="2" opacity="OP.YZ" marker-end="url(#arrow)" />

<!-- X->Z -->
<line x1="30" y1="60" x2="123" y2="102" stroke="white" stroke-width="2" opacity="OP.XZ"  marker-end="url(#arrow)" />
<!-- Z->X -->
<line x1="150" y1="120" x2="58" y2="79" stroke="white" stroke-width="2" opacity="OP.ZX" marker-end="url(#arrow)" /> 

<!-- Interaction -->
<!-- M first x first y, then Q pivot point X Y, second X Y -->
<path d="M30 135 Q 65 100 75 155" stroke="ColorInteraction" stroke-width="2" opacity="OP.Int"  /> 


<!-- The Nodes -->
<!-- X -->
<circle cx="30" cy="60" r="15" stroke="black" stroke-width="2" fill="white" />
<text x="30" y="65.5" fill="black" text-anchor="middle">X</text>
<!-- Y -->
<circle cx="30" cy="180" r="15" stroke="black" stroke-width="2" fill="white" />
<text x="30" y="185.5" fill="black" text-anchor="middle">Y</text>
<!-- Z -->
<circle cx="150" cy="120" r="15" stroke="black" stroke-width="2" fill="white" />
<text x="150" y="125.5" fill="black" text-anchor="middle">Z</text>


<!-- The Numbers -->
<!-- XY -->
<circle cx="30" cy="115" r="12" style="fill:black;" />
<text x="30" y="120.5" fill="ColorXY" text-anchor="middle">Num.XY</text>
<!-- ZY -->
<circle cx="95" cy="150" r="12" style="fill:black;" />
<text x="95" y="155" fill="ColorZY" text-anchor="middle">Num.ZY</text>
<!-- XZ -->
<circle cx="95" cy="90" r="12" style="fill:black;" />
<text x="95" y="95" fill="ColorXZ" text-anchor="middle">Num.XZ</text>
<!-- Interaction -->
<circle cx="60" cy="130" r="12" style="fill:black;" />
<text x="63" y="130" fill="ColorInteraction" text-anchor="middle">Num.Interaction</text>


<!-- The Descriptors -->
<!-- X -->
<text x="10" y="20" fill="grey" text-anchor="left" font-style = "italic">Independent Variable:</text>
<text x="10" y="35" fill="grey" text-anchor="left" font-style = "italic">XNAME</text>
<!-- Y -->
<text x="10" y="220" fill="grey" text-anchor="left" font-style = "italic">Dependent Variable:</text>
<text x="10" y="235" fill="grey" text-anchor="left" font-style = "italic">YNAME</text>
<!-- Z -->
<!-- <text x="180" y="125" fill="grey" text-anchor="middle" font-style = "italic" transform="rotate(90 180,125)">Third Variable</text> -->
<!-- <text x="165" y="125" fill="grey" text-anchor="middle" font-style = "italic" transform="rotate(90 165,125)">Type</text> -->
<!-- <text x="150" y="125" fill="grey" text-anchor="middle" font-style = "italic" transform="rotate(90 150,125)">e.g., Diet</text> -->

<text x="190" y="125" fill="grey" text-anchor="middle" font-style = "italic" transform="rotate(90 190,125)">Type</text>
<text x="145" y="95" fill="grey" text-anchor="middle" font-style = "italic" transform="rotate(90 145,125)">ZNAME</text>

</svg>
'

# Edit the SVG for the graph based on the structure and sliders. The changes are called GraphMods

output$CSGraph <- renderText({
  if(input$RoleOfZ == 'Noise / Alternative Cause'){
    if(input$N_SlopeXonY == 0){
      GraphMods <- c("ColorXY" = "#FF00FF",
                     "ColorZY" = "#00FF00",
                     "ColorXZ" = "black",
                     "ColorInteraction" = "black",
                     "OP.XY" = "0",
                     "OP.ZY" = "1",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "0",
                     "OP.Int" = "0",
                     "Num.XY" = input$N_SlopeXonY,
                     "Num.ZY" = input$N_SlopeZonY,
                     "Num.XZ" = "",
                     "Type" = "3rd Variable (Noise):"
                     # "XNAME" = XNAME()
                     )
    }
    else{
      GraphMods <- c("ColorXY" = "#FF00FF",
                     "ColorZY" = "#00FF00",
                     "ColorXZ" = "black",
                     "ColorInteraction" = "black",
                     "OP.Int" = "0",
                     "OP.XY" = "1",
                     "OP.ZY" = "1",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "0",
                     "Num.XY" = input$N_SlopeXonY,
                     "Num.ZY" = input$N_SlopeZonY,
                     "Num.XZ" = "",
                     "Type" = "3rd Variable (Noise):"
                     # "XNAME" = XNAME()
                     )
    }
  }
    
    if(input$RoleOfZ == 'Confound / Common Cause'){
      if(input$CC_SlopeXonY == 0){
        GraphMods <- c("ColorXY" = "#FF00FF",
                       "ColorZY" = "#00FF00",
                       "ColorXZ" = "white",
                       "ColorInteraction" = "black",
                       "OP.Int" = "0",
                       "OP.XY" = "0",
                       "OP.ZY" = "1",
                       "OP.YZ" = "0",
                       "OP.ZX" = "1",
                       "OP.XZ" = "0",
                       "Num.XY" = input$CC_SlopeXonY,
                       "Num.ZY" = input$CC_SlopeZonY,
                       "Num.XZ" = input$CC_SlopeZonX,
                       "Type" = "3rd Variable (Confound)"
        )
      }
      else{
        GraphMods <- c("ColorXY" = "#FF00FF",
                       "ColorZY" = "#00FF00",
                       "ColorXZ" = "white",
                       "ColorInteraction" = "black",
                       "OP.Int" = "0",
                       "OP.XY" = "1",
                       "OP.ZY" = "1",
                       "OP.YZ" = "0",
                       "OP.ZX" = "1",
                       "OP.XZ" = "0",
                       "Num.XY" = input$CC_SlopeXonY,
                       "Num.ZY" = input$CC_SlopeZonY,
                       "Num.XZ" = input$CC_SlopeZonX,
                       "Type" = "3rd Variable (Confound):"
        )
      }
    }

  if(input$RoleOfZ == 'Alternative Effect'){
    if(input$AE_SlopeXonY == 0){
      GraphMods <- c("ColorXY" = "#FF00FF",
                     "ColorZY" = "#00FF00",
                     "ColorXZ" = "white",
                     "ColorInteraction" = "black",
                     "OP.Int" = "0",
                     "OP.XY" = "0",
                     "OP.ZY" = "0",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "1",
                     "Num.XY" = input$AE_SlopeXonY,
                     "Num.ZY" = '',
                     "Num.XZ" = input$AE_SlopeXonZ,
                     "Type" = "3rd Variable (Alt. Effect)"
      )
    }
    else{
      GraphMods <- c("ColorXY" = "#FF00FF",
                     "ColorZY" = "#00FF00",
                     "ColorXZ" = "white",
                     "ColorInteraction" = "black",
                     "OP.Int" = "0",
                     "OP.XY" = "1",
                     "OP.ZY" = "0",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "1",
                     "Num.XY" = input$AE_SlopeXonY,
                     "Num.ZY" = '',
                     "Num.XZ" = input$AE_SlopeXonZ,
                     "Type" = "3rd Variable (Alt Effect):"
      )
    }
  }
  
  if(input$RoleOfZ == 'Mediator / Mechanism'){
    if(input$M_SlopeXonY == 0){
      GraphMods <- c("ColorXY" = "#FF00FF",
                     "ColorZY" = "#00FF00",
                     "ColorXZ" = "white",
                     "ColorInteraction" = "black",
                     "OP.Int" = "0",
                     "OP.XY" = "0",
                     "OP.ZY" = "1",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "1",
                     "Num.XY" = input$M_SlopeXonY,
                     "Num.ZY" = input$M_SlopeZonY,
                     "Num.XZ" = input$M_SlopeXonZ,
                     "Type" = "3rd Variable (Mediator):"
      )
    }
    else{
      GraphMods <- c("ColorXY" = "#FF00FF",
                     "ColorZY" = "#00FF00",
                     "ColorXZ" = "white",
                     "ColorInteraction" = "black",
                     "OP.Int" = "0",
                     "OP.XY" = "1",
                     "OP.ZY" = "1",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "1",
                     "Num.XY" = input$M_SlopeXonY,
                     "Num.ZY" = input$M_SlopeZonY,
                     "Num.XZ" = input$M_SlopeXonZ,
                     "Type" = "3rd Variable (Mediator):"
      )
    }
  }
  
    if(input$RoleOfZ == 'Common Effect'){
      if(input$CE_SlopeXonY == 0){
        GraphMods <- c("ColorXY" = "#9e37eb",
                       "ColorZY" = "white",
                       "ColorXZ" = "white",
                       "ColorInteraction" = "black",
                       "OP.Int" = "0",
                       "OP.XY" = "0",
                       "OP.ZY" = "0",
                       "OP.YZ" = "1",
                       "OP.ZX" = "0",
                       "OP.XZ" = "1",
                       "Num.XY" = input$CE_SlopeXonY,
                       "Num.ZY" = input$CE_SlopeYonZ,
                       "Num.XZ" = input$CE_SlopeXonZ,
                       "Type" = "3rd Variable (Comm. Eff.):"
        )
      }
      else{
        GraphMods <- c("ColorXY" = "#9e37eb",
                       "ColorZY" = "white",
                       "ColorXZ" = "white",
                       "ColorInteraction" = "black",
                       "OP.Int" = "0",
                       "OP.XY" = "1",
                       "OP.ZY" = "0",
                       "OP.YZ" = "1",
                       "OP.ZX" = "0",
                       "OP.XZ" = "1",
                       "Num.XY" = input$CE_SlopeXonY,
                       "Num.ZY" = input$CE_SlopeYonZ,
                       "Num.XZ" = input$CE_SlopeXonZ,
                       "Type" = "3rd Variable (Comm. Eff.):"
        )
      }
    }
  if(input$RoleOfZ == 'Interaction / Moderator'){ #never get rid of X->Y even if 0 because looks weird for interaction
      GraphMods <- c("ColorXY" = "Red",
                     "ColorZY" = "Yellow",
                     "ColorXZ" = "black",
                     "ColorInteraction" = "dodgerblue",
                     "OP.Int" = "1",
                     "OP.XY" = "1",
                     "OP.ZY" = "1",
                     "OP.YZ" = "0",
                     "OP.ZX" = "0",
                     "OP.XZ" = "0",
                     "Num.XY" = input$I_SlopeXonY,
                     "Num.ZY" = input$I_SlopeZonY,
                     "Num.XZ" = "",
                     "Num.Interaction" = input$I_Interaction,
                     "Type" = "3rd Variable (Interaction):"
                     # "XNAME" = XNAME()
      )
  }
  
  #Adding more graphmods that are common to all the graphs regardless of structure or sliders
  GraphMods <- c(GraphMods,
                 "XNAME" = XNAME(),
                 "YNAME" = YNAME(),
                 "ZNAME" = ZNAME(),
                 "ZNAMEG1" = ZNAMEG1(),
                 "ZNAMEG2" = ZNAMEG2()
                 )

  ## GraphMods are actually implemented
  return(str_replace_all(CSGraph, GraphMods))
  }) #end of rendertext for causal structure graph

} #end of server

# Run the application 
shinyApp(ui = ui, server = server)

# Colors being used in various places
# #9e37eb Purple
# #00FF00 Green
# #FF00FF Magenta
# #2993fc Blue or "dodgerblue"
# #fffa40 Yellow

