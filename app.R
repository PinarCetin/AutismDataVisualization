

# Pinar Cetin - 2010064
# Final Project



library(shiny)
library(ggplot2)
library(leaflet)
library(maps)
library(DT)
library(shinythemes)
library(stringr)
library(caret)

# read data
data <- read.csv("train.csv",header = TRUE)

# cleaning
data$ethnicity[data$ethnicity == '?'] <- 'Unknown'
data$ethnicity[data$ethnicity == 'others'] <- 'Others'
data$age_desc <- NULL
data$ID <- NULL
data$relation <- NULL
data$used_app_before <- NULL
library(dplyr)
data <- data %>% mutate_if(is.character, as.factor)
data[,1:10] <- data[,1:10] %>% mutate_all(as.factor)
data$Class.ASD <- as.factor(data$Class.ASD)

# make summary output verbatim to fit the whole screen
options(width = 300)

# data for visualization, add None variable for grouping
df <- data
df$None <- ""


# logistic regression
logreg <- glm(Class.ASD~., family = "binomial", data = data[, -c(13, 16, 17)])
logreg_summary <- summary(logreg)
logreg_summary
preds <- predict(logreg, newdata = data, type = "response")
preds <- ifelse(preds > 0.5, 1, 0)
logreg_performance <- confusionMatrix(as.factor(preds), data$Class.ASD, positive = "1")
logreg_performance

ui <- fluidPage(title = "ASD",
  theme = shinytheme(theme = "paper"),
  navbarPage(title = "Autism Spectrum Disorder Analysis",
             tabPanel(title = "Description",
                        column(
                          6,
                          br(),br(),
                          h4("About Autism Spectrum Disorder"),
                          p(
                            "Autism spectrum disorder (ASD) is a developmental disability caused by 
                            differences in the brain. People with ASD often have problems with social 
                            communication and interaction, and restricted or repetitive behaviors or 
                            interests. People with ASD may also have different ways of learning, moving, 
                            or paying attention."
                          ),
                          br(),br(),
                          h4("Aim of the Project"),
                          p("The project of the StataR Foundation offers an application for individuals who seek 
                            healthcare institutions and hospitals with suspected autism. This application also aims 
                            to provide information about autism and promote a greater understanding of the experiences 
                            and challenges faced by autistic individuals."),
                          p("Additionally, the application can potentially contribute to the early diagnosis of autism. 
                            By providing accessible and accurate information about the early signs and symptoms of 
                            autism to the community, individuals' awareness can be increased. This increased awareness 
                            can lead to early diagnosis and interventions that may result in better outcomes for 
                            individuals with autism and their families."
                          ),
                          br(),br(),
                          h4("Author:"),
                          
                          tags$ul(
                            
                            tags$li("Pinar Cetin"),
                          ),
                        ),
                        column(
                          6,
                          br(),br(),br(),
                          HTML('<iframe width="720" height="405" src="https://www.youtube.com/embed/Lk4qs8jGN4U" title="What is Autism?" frameborder="0" allowfullscreen></iframe>'),
                          
                        )
                      ), 
             tabPanel(
               title = "Data Information",
               id = 'dataset',
               fluidRow(column(width = 5, 
                      h5("About the Dataset"),
                      p(
                        "This data is about the diagnosis of autism in people. 
                            It is collected to distinguish people who have autism and 
                            understand their behaviors. There are 22 columns in the original data. 
                            In this project, 18 columns were used, which are listed below. It contains 16 categorical variables, 
                            14 for which is binary, and 2 numeric variables. There are 800 observations in the data."
                      ),
                      p("A1_Score to A10_Score - Score based on Autism Spectrum Quotient (AQ) 10-item screening tool. 
                            Classified result as 0 or 1."),
                      tags$ul(
                        tags$li("A1: I expect people to communicate with me rather than spontaneously contact others."),
                        tags$li("A2: I need help understanding what to do in social situations."),
                        tags$li("A3: I need help using correct gestures and facial expressions when communicating with others."),
                        tags$li("A4: I need help understanding when to speak and when to shut up in conversations."),
                        tags$li("A5: I can't get comfortable immediately when I encounter a new situation or place."),
                        tags$li("A6: I know how to tell if someone listening to me is getting bored."),
                        tags$li("A7: I find it difficult to understand what other people are thinking or feeling."),
                        tags$li("A8: I engage in long conversations with others about topics I am interested in."),
                        tags$li("A9: When I learn something new, I usually become obsessed with it."),
                        tags$li("A10: I usually focus too much on details when learning something new."),
                        tags$li("age - Age of the patient in years"),
                        tags$li("gender - Gender of the patient"),
                        tags$li("ethnicity - Ethnicity of the patient"),
                        tags$li("jaundice - Whether the patient had jaundice at the time of birth"),
                        tags$li("autism - Whether an immediate family member has been diagnosed with autism"),
                        tags$li("contry_of_res - Country of residence of the patient"),
                        tags$li("result - Score for AQ1-10 screening test"),
                        tags$li("Class/ASD - Classified result as 0 or 1. Here 0 represents No, and one represents Yes. 
                                    This is the target column."),
                      )),
               column(width = 7,
                      DT::DTOutput("mytable1"))),
               fluidRow(h5("Summary"),
                 verbatimTextOutput("summary"))
             ), 
             tabPanel(
               title = "Visualization",
               sidebarPanel(
                 width = 2,
                 tags$b("Relationship Between Variables:"),
                 selectInput(
                   inputId = "x",
                   label = "x-axis:",
                   choices = names(data)[-16],
                   selected = names(data)[18]
                 ),
                 selectInput(
                   inputId = "y",
                   label = "y-axis:",
                   choices = names(data)[-16],
                   selected = names(data)[11]
                 ),
                 uiOutput("group_by"),
                 selectInput(
                   inputId = "var_bar",
                   label = tags$b("Frequencies:"),
                   choices = names(data),
                   selected = "age"
                 ),
                 uiOutput("countries"),
               ),
               mainPanel(width = 10,
                         fluidRow(column(6, plotOutput("xy")),
                                  column(5, plotOutput("barplot"))),
                         fluidRow(h6("Class ASD By Country"),
                                  leafletOutput("map", width = "90%", height = "420px")))
             ),
             tabPanel(
               title = "Analysis",
               fluidRow(
                 column(width = 3,
                        h4("Model Performance:"),
                        h5("Accuracy: 87%"),
                        p("87% of the prediction results are expected to be correct."),
                        h5("Sensitivity: 70%"),
                        p("70% of the prediction results are expected to be correct if patient has autism."),
                        h5("Specificity: 91%"),
                        p("91% of the prediction results are expected to be correct if patient does not have autism."),
                        helpText("Logistic Regression is used for this analysis")),
                 column(width = 4,
                   selectInput(inputId = "A1", label = "A1: I expect people to communicate with me rather than spontaneously contact others.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A2", label = "A2: I need help understanding what to do in social situations.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A3", label = "A3: I need help using correct gestures and facial expressions when communicating with others.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A4", label = "A4: I need help understanding when to speak and when to shut up in conversations.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A5", label = "A5: I can't get comfortable immediately when I encounter a new situation or place.", choices = c("TRUE", "FALSE"), width = "100%"),),
                 column(width = 3,
                   selectInput(inputId = "A6", label = "A6: I know how to tell if someone listening to me is getting bored.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A7", label = "A7: I find it difficult to understand what other people are thinking or feeling.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A8", label = "A8: I make long conversations with others about topics I'm interested in.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A9", label = "A9: When I learn something new, I usually become obsessed with it.", choices = c("TRUE", "FALSE"), width = "100%"),
                   selectInput(inputId = "A10", label = "A10: I usually focus too much on details when learning something new.", choices = c("TRUE", "FALSE"), width = "100%"),),
                 column(width = 2,
                        numericInput(inputId = "age", label = "Age:", min = 0, max = 100, value = 20, width = "100%"),
                        selectInput(inputId = "gender", label = "Gender:", choices = c("Female", "Male"), width = "100%"),
                        selectInput(inputId = "juandice", label = "Had jaundice at the time of birth.", choices = c("yes", "no"), width = "100%"),
                        selectInput(inputId = "autism", label = "An immediate family member has been diagnosed with autism.", choices = c("yes", "no"), width = "100%"),
                        actionButton(inputId = "predict", label = "Predict", width = "100%")),
               ),
               fluidRow(
                 column(width = 5,
                        h5("Findings"),
                        tags$ul(
                          tags$li("According to our model, A4, A6, A8 and A9 scores make significant difference when predicting if a person has autism or not at 95% confidence level.
                           In other words, people with autism tend to answer positively to these statements."),
                          tags$li("Another important note is features like ethnicity, country of residence, age and gender has no meaningful effect on identifying autism."),
                          tags$li("Model suggests A3 and A5 scores might have some impact in identifying autism. All other scores does not seem to make much difference
                          among autistic and non autistic peaople."),),
                        verbatimTextOutput("model_summary")
                        ),
                 column(width = 1),
                 column(width = 5,
                        h5("Prediction Result:"),
                        helpText("Prediction result gives a probability value between 0 and 1. 
                                 A probability of 0 means that the event will not happen.
                                 A probability of 1 means that the event will happen."),
                        textOutput("probability"),
                        textOutput("interpret"))
               )
             ),
             tabPanel(
               title = "References",
               h3("References"),
               tags$ul(
                 tags$li("https://www.kaggle.com/datasets/konikarani/autismdiagnosis"),
                 br(),
                 tags$li("https://embrace-autism.com/autism-spectrum-quotient/"),
                 br(),
                 tags$li("https://static1.squarespace.com/static/5b31928bcef372d3e22e4fec/t/5e1f5d9e9b2f5f1c7d4c650f/1579113887480/autism-spectrum-quotient-aq10-test-pdf-186582493.pdf"),
               )
             )
  )
)



server <- function(input, output) {
  
  # data table
  output$mytable1 <- DT::renderDT({
    DT::datatable(data, options = list(scrollX = TRUE))
  })
  
  # data summary
  output$summary <- renderPrint(summary(data))
  
  
  # first plot: x and y relation
  x <- reactive(input$x)
  y <- reactive(input$y)
  
  # add group by input if one of the variables is numeric
  output$group_by <- renderUI({
    if (is.numeric(data[, x()]) || is.numeric(data[, y()])) {
      selectInput(
        inputId = "group", 
        label = "Group By:", 
        choices = c("None", names(data[,-16] %>% select_if(is.factor))), 
        selected = "None")
    } else {NULL}
  })
  
  group <- reactive(input$group)
  output$xy <- renderPlot({
    if (is.factor(df[, x()]) & is.factor(df[, y()])) {
      ggplot(data = df, aes(x = df[, x()], fill = df[, y()])) +
        geom_bar(position = "fill") +
        ylab("Proportion") + xlab(x()) + labs(fill = y()) +
        ggtitle(paste("Barplot of", x(), "vs", y())) +
        theme_minimal()
    } else if (is.factor(df[, x()])) {
      ggplot(data = df, aes(x = df[, x()], y = df[, y()], fill = df[,group()])) +
        geom_boxplot() + xlab(x()) + ylab(y()) + labs(fill = group()) +
        ggtitle(paste("Boxplot of", x(), "vs", y())) +
        theme_minimal()
    } else if (is.factor(df[, y()])) {
      ggplot(data = df, aes(x = df[, x()], y = df[, y()], fill = df[,group()])) +
        geom_boxplot() + xlab(x()) + ylab(y()) + labs(fill = group()) +
        ggtitle(paste("Boxplot of", x(), "vs", y())) +
        theme_minimal()
    } else {
      ggplot(data = df, aes(x = df[, x()], y = df[, y()], color = df[,group()])) +
        geom_point() + geom_smooth(method = "lm") + xlab(x()) + ylab(y()) + labs(color = group()) +
        ggtitle(paste("Scatter plot of", x(), "vs", y())) +
        theme_minimal()
    }
  })
  
  # add select input if user selects country for second plot
  output$countries <- renderUI({
    if (input$var_bar == "contry_of_res") {
      selectInput(inputId = "c", "Select Countries:", 
                  choices = sort(unique(data$contry_of_res)), multiple = TRUE)
    } else {NULL}
  })
  
  # Second plot: Bar plot and histogram
  freq <- reactive(input$var_bar)
  output$barplot <- renderPlot({
    if (is.numeric(data[, freq()])) {
      ggplot(data = data, aes(x = data[, freq()])) +
        geom_histogram(aes(y = ..density..),fill = "steelblue",size = 1) +
        ylab("Frequency") + xlab(freq()) +
        ggtitle(paste("Histogram of", freq())) +
        theme_minimal()
    } else if (input$var_bar == "contry_of_res") {
      d <- data %>% filter(contry_of_res %in% input$c)
      ggplot(d, aes(x = d[, freq()])) +
        geom_bar(stat = "count", fill = "steelblue") +
        ylab("Frequency") + xlab(freq()) + ggtitle(paste("Bar Plot of", freq())) +
        theme_minimal()
    } else {
      ggplot(data, aes(x = data[, freq()])) +
        geom_bar(stat = "count", fill = "steelblue") +
        ylab("Frequency") + xlab(freq()) + ggtitle(paste("Bar Plot of", freq())) +
        theme_minimal()
    }
  })
  
  # leaflet map autism count per country
  map_df = reactive({
    ulkeler <- data %>% select("contry_of_res","Class.ASD") %>% subset(Class.ASD==1)%>% rename(country=contry_of_res) %>% group_by(country) %>% count
    data(world.cities)
    world.cities %>% dplyr::select(country = country.etc, lat, lng = long) %>% distinct(country,.keep_all=TRUE) %>% left_join(ulkeler,.,by="country")
  })
  output$map = renderLeaflet({
    leaflet(map_df())%>%
      addProviderTiles("Esri")%>%
      addMarkers(label=~n,clusterOptions = markerClusterOptions())
  })
  
  output$model_summary <- renderPrint({
    logreg_summary <- summary(logreg)
    logreg_summary
  })
  
  # make dataset from user input and predict probability
  prob <- reactive({
    test <- data.frame("A1_Score" = as.factor(ifelse(input$A1 == "TRUE", 1, 0)), "A2_Score" = as.factor(ifelse(input$A2 == "TRUE", 1, 0)), 
                       "A3_Score" = as.factor(ifelse(input$A3 == "TRUE", 1, 0)), "A4_Score" = as.factor(ifelse(input$A4 == "TRUE", 1, 0)), 
                       "A5_Score" = as.factor(ifelse(input$A5 == "TRUE", 1, 0)), "A6_Score" = as.factor(ifelse(input$A6 == "TRUE", 1, 0)), 
                       "A7_Score" = as.factor(ifelse(input$A7 == "TRUE", 1, 0)), "A8_Score" = as.factor(ifelse(input$A8 == "TRUE", 1, 0)), 
                       "A9_Score" = as.factor(ifelse(input$A9 == "TRUE", 1, 0)), "A10_Score" = as.factor(ifelse(input$A10 == "TRUE", 1, 0)),
                       "age" = input$age, "gender" = str_to_lower(str_sub(input$gender,start = 1,end = 1)), "jaundice" = input$juandice, "austim" = input$autism)
    predict(logreg, newdata = test, type = "response")
  })
  
  output$probability <- renderText({
    req(input$predict)
    isolate({
      paste("Probability:", round(prob(), 4))
    })
  })
  output$interpret <- renderText({
    req(input$predict)
    isolate({
      paste0("The Logistic Regression Model is ", round(prob(),3)*100, "% ", "certain that this person has autism spectrum disorder.")
    })
  })
  
}

shinyApp(ui, server)
