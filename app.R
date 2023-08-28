library(DT)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shiny)
library(rsample)
library(caret)
library(tidyr)
library(ROCR)
library(formattable)
library(ggridges)
library(glue)
library(partykit)
library(randomForest)
library(shinythemes)
library(cowplot)
library(e1071)
library(hrbrthemes)
library(viridis)
library(glue)
library(scales)
library(yardstick)
library(rsconnect)


# prepare -----------------------------------------------------------------
# Random Forest Model
rf_model_final <- readRDS("rf_model_final.RDS")

# Train and test dataframe

wsdm_model_train_final <- readRDS("wsdm_model_train.RDS")
wsdm_model_test <- readRDS("wsdm_model_test.RDS")

# Variable Importance Random Forest
varmp_final <- readRDS("varmp_final.RDS")
# round to 0
varmp_final$`mean decrease gini` <-
  round(varmp_final$`mean decrease gini`, 0)
# Remove'_'
new_variables = c(
  "automatic subscription",
  "average time per day",
  "plan list price mean",
  "Number of Unique Track",
  "Sum of Unique Song",
  " Sum of Songs at full duration",
  "Number of Songs in full duration per day",
  "The Period of Subscription"
  ,
  "sum of number 25",
  "sum of number 25 per day"
)
varmp_final$new_variables <- new_variables
varmp_df <- varmp_final %>%
  select(-variable)

varmp_df <- varmp_df %>%
  mutate(tooltip = glue("Mean decrease gini: {`mean decrease gini`}"))

# Data Visualization Dataframe
# For Listening hour
forest_viz_ggplot <- readRDS("forest_viz_ggplot.RDS")
# For Registration line plot
wsdm_for_registration <- readRDS("wsdm_for_registration.RDS")
# For auto renew and loyalty
forest_viz <- readRDS("forest_viz.RDS")
# change the name of variable into yes or no
forest_viz$is_auto_renew <-
  ifelse(forest_viz$is_auto_renew == 1, "yes", "no")
# change the type of data is_auto_renew variable
forest_viz$is_auto_renew <- as.factor(forest_viz$is_auto_renew)
# Data wrangling for auto_renew plot
forest_viz_auto_renew <- forest_viz %>%
  select(is_auto_renew, is_churn) %>%
  group_by(is_auto_renew, is_churn) %>%
  count() %>%
  spread(is_churn, n) %>%
  ungroup %>%
  drop_na()

forest_viz_coba <- forest_viz_auto_renew %>%
  pivot_longer(
    -is_auto_renew,
    names_to = "condition",
    names_transform = list(condition = as.factor)
  )

forest_viz_coba <- forest_viz_coba %>%
  mutate(
    `has auto renew feature?` = is_auto_renew,
    `churn or not` = condition,
    `number of subscriber` = value
  )

auto_renew_plot_df <- forest_viz_coba %>%
  select(-is_auto_renew, -condition, -value)

auto_renew_plot_df$`has auto renew feature?` <-
  ifelse(
    auto_renew_plot_df$`has auto renew feature?` == "yes",
    "has auto renew feature",
    "doesn't have auto renew feature"
  )
auto_renew_plot_df$`has auto renew feature?` <-
  as.factor(auto_renew_plot_df$`has auto renew feature?`)

wsdm_model_test$unq_track <- round(wsdm_model_test$unq_track , 0)
wsdm_model_test$num_100_perday <-
  round(wsdm_model_test$num_100_perday , 0)
# Data wrangling for loyalty_range
forest_viz_loyalityplot_df <- forest_viz %>%
  select(loyality_range, is_churn) %>%
  group_by(loyality_range, is_churn) %>%
  count() %>%
  spread(is_churn, n) %>%
  ungroup %>%
  drop_na()

# Data wrangling for listening hour

forest_viz_listeninghourplot_df <- forest_viz %>%
  select(listening_hour, is_churn) %>%
  group_by(listening_hour, is_churn) %>%
  count() %>%
  spread(is_churn, n) %>%
  ungroup %>%
  drop_na()

listening_hour_df <- forest_viz_listeninghourplot_df %>%
  pivot_longer(
    -listening_hour,
    names_to = "churn or stay",
    names_transform = list(`churn or stay` = as.factor)
  )

# for EDA

raw_df <- forest_viz
not_numeric <-
  sapply(names(raw_df), function(x)
    ! is.numeric(raw_df[[x]]))
df <- raw_df


# for confusion matrix

wsdm_forest_split_class_lr <- predict(rf_model_final, wsdm_model_test, type = "class")
wsdm_forest_split_prob_lr <- predict(rf_model_final, wsdm_model_test ,type = "prob")

wsdm_forest_pred <- select(wsdm_model_test, is_churn) %>%
  bind_cols(churn_pred = wsdm_forest_split_class_lr) %>%
  bind_cols(stay_prob =  wsdm_forest_split_prob_lr[, 1]) %>%
  bind_cols(churn_prob = round(wsdm_forest_split_prob_lr [, 2]))

forest_model_evalmat <- wsdm_forest_pred %>%
  summarise(
    accuracy = accuracy_vec(is_churn, churn_pred),
    sensitivity = sens_vec(is_churn, churn_pred),
    specificity = spec_vec(is_churn, churn_pred),
    precision = precision_vec(is_churn, churn_pred)
  )

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = " KKBOXX Churn Prediction Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Home",
        tabName = "home",
        icon = icon("house")
      ),
      menuItem(
        text = "Data Analysis",
        tabName = "da",
        icon = icon("square-poll-vertical")
      ),
      menuItem(
        text = "EDA",
        tabName = "eda",
        icon = icon("eye")
      ),
      menuItem(
        text = "Prediction",
        tabName = "pred",
        icon = icon("volume-high")
      ),
      menuItem(
        text = "Evaluate Model",
        tabName = "EM",
        icon = icon("star")
      ),
      menuItem(
        text = "Database",
        tabName = "dbs",
        icon = icon("database")
      )
      
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "home",
      box(
        solidHeader = TRUE,
        align = "center",
        width = 12,
        imageOutput("home_img", height = "600px")
      )
    ),
    tabItem(tabName = "da",
            fluidRow(
              box(
                "Subscriber Growth ",
                width = 12,
                status = "primary",
                background = "light-blue",
                plotOutput(outputId = "growth_plot")
              ),
              box(
                "Relationship Listening Hour with Churn Rate",
                width = 6,
                background = "navy",
                plotlyOutput(outputId = "listen_plot", height = "600px")
              ),
              box(
                "Relationship Listening Hour with Churn Rate",
                width = 6,
                background = "green",
                plotlyOutput(outputId = "renew_plot", height = "600px")
              )
            )),
    tabItem(tabName = "eda",
            fluidRow(
              column(
                width = 4,
                sliderInput(
                  "sampleSize",
                  "Plot sample size (n)",
                  min = 1,
                  max = nrow(df),
                  value = min(1000, nrow(df)),
                  step = nrow(df) / 50,
                  round = 0
                ),
                radioButtons(
                  "sampleType",
                  "Plot sample type",
                  choices = list("Random n" = "random", "First n" = "first")
                ),
                numericInput("sampleSeed", "Sample seed", value = 1),
                selectInput("x", "X", names(df)),
                selectInput("y", "Y", c("None", names(df)), names(df)[[2]]),
                # only allow non-numeric variables for color
                selectInput("color", "Color", c("None", names(df)[not_numeric])),
                p(
                  "Jitter and smoothing are only available when two numeric variables are selected."
                ),
                checkboxInput("jitter", "Jitter"),
                checkboxInput("smooth", "Smooth")
              ),
              column(
                width = 8,
                tabsetPanel(
                  type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Data Snippet", verbatimTextOutput("snippet")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("str() Output", verbatimTextOutput("str"))
                )
              )
            )),
    tabItem(tabName = "pred",
            fluidRow(
              column(
                width = 5,
                box(
                  background = "light-blue",
                  selectInput(
                    inputId = "automaticsubscription",
                    label = "Has auto subscription feature?",
                    choices =
                      levels(wsdm_model_test$is_auto_renew)
                  )
                ),
                box(
                  sliderInput(
                    inputId = "slidelisten",
                    label = "The Duration of listening hour",
                    min = min(wsdm_model_test$listening_hour),
                    max = max(wsdm_model_test$listening_hour),
                    value = min(wsdm_model_test$listening_hour),
                    step = 200
                  )
                ),
                
                box(
                  selectInput(
                    inputId = "pricerange",
                    label = "How much does the subscriber pay?",
                    choices =
                      levels(wsdm_model_test$price_range)
                  )
                ),
                
                box(
                  sliderInput(
                    inputId = "slideof100",
                    label = "Number of Full Duration Song ",
                    min = min(wsdm_model_test$num_100_perday),
                    max = max(wsdm_model_test$num_100_perday),
                    value = min(wsdm_model_test$num_100_perday),
                    step = 5
                  )
                ),
                box(
                  selectInput(
                    inputId = "loyalinput",
                    label = "How loyal are the subscriber",
                    choices =
                      levels(wsdm_model_test$loyality_range)
                  )
                ),
                box(
                  sliderInput(
                    inputId = "slideuniquetrack",
                    label = "Number of listened song each day",
                    min = min(wsdm_model_test$unq_track),
                    max = max(wsdm_model_test$unq_track),
                    value = min(wsdm_model_test$unq_track),
                    step = 5
                  )
                ),
                box(
                  background = "light-blue",
                  actionButton("pred", "Predict!", icon("paper-plane"),
                               style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              ),
              column(width = 7,
                     fluidRow(infoBoxOutput("predchurn")),
                     fluidRow(infoBoxOutput("predstay")))
            )),
    tabItem(tabName = "EM",
            fluidRow(column(
              width = 12,
              box(
                "Confusion Matrix",
                width = 12,
                status = "primary",
                background = "light-blue",
                tableOutput(outputId = "confusion_matrix")
              )
            )),
            fluidRow(
              column(
                width = 12,
                box(
                  "Variable Importance",
                  width = 12,
                  status = "primary",
                  background = "light-blue",
                  plotlyOutput(outputId = "Varimp", height = "800px")
                )
              )
              #,
              # column(
              #   width = 6,
              #   box(
              #     "Relationship between loyality and churn customer",
              #     width = 12,
              #     status = "primary",
              #     background = "light-blue",
              #     plotlyOutput(outputId = "loyality_plot", height = "800px")
              #  )
              # )
            ))
  ))
)


# server ------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$home_img <- renderImage({
    list(src = "dashboardimage.png",
         width = "1200",
         height = "700")
  }, deleteFile = F)
  
  
  
  output$listen_plot <-
    renderPlotly({
      listening_hour_plot <-
        ggplot(
          listening_hour_df ,
          aes(
            fill = `churn or stay`,
            x = listening_hour,
            y = value,
            text = paste0(
              "Churn or stay: ",
              `churn or stay`,
              "<br>",
              "number of subscriber: ",
              scales::comma(value, 1),
              "<br>"
            )
          )
        ) +
        geom_bar(position = "stack", stat = "identity") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_viridis(discrete = T) +
        labs(y = 'Number of Subscriber',
             x = ' Listening Hours') +
        theme_ipsum()
      ggplotly(listening_hour_plot, tooltip = "text")
      
      
    })
  
  output$loyality_plot <-
    renderPlotly({
      loyality_plot <-
        ggplot(forest_viz_loyalityplot_df,
               aes(
                 x = loyality_range,
                 y = churn,
                 text = paste0(
                   "years :" ,
                   loyality_range,
                   "<br>",
                   "number of churn: ",
                   scales::comma(churn, 1),
                   "<br>",
                   "number of stay: ",
                   scales::comma(stay, 1)
                 )
               )) +
        geom_point(aes(color = loyality_range)) +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma)  +
        ggtitle("churn by listening hour") +
        labs(x = 'year',
             y = ' Number of churn customer',
             title = 'Relationship between loyality and churn customer') +
        theme_ipsum()
      
      ggplotly(loyality_plot, tooltip = "text")
      
      
    })
  
  output$growth_plot <- renderPlot({
    colnames(wsdm_for_registration) [1] = "year registration"
    
    wsdm_for_registration_app <- wsdm_for_registration %>%
      head(14) %>%
      select(-tooltip)
    
    ggplot(wsdm_for_registration_app,
           aes(x = `year registration`, y = `number of subscriber`)) +
      geom_line(color = "#69b3a2") +
      geom_point(color = "#69b3a2", size = 4) +
      scale_y_continuous(labels = scales::comma)  +
      ylab("number of subscriber") +
      theme_ipsum()
    
  })
  
  
  output$renew_plot <-
    renderPlotly({
      auto_renew_plot <-
        ggplot(
          auto_renew_plot_df,
          aes(
            fill = `churn or not`,
            x = `has auto renew feature?`,
            y = `number of subscriber`,
            text = paste0(
              "Churn or stay: ",
              `churn or not`,
              "<br>",
              "number of subscriber: ",
              scales::comma(`number of subscriber`, 1),
              "<br>"
            )
          )
        ) +
        geom_bar(position = "stack",
                 stat = "identity",
                 width = 0.8) +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_viridis(discrete = T) +
        labs(y = 'Number of Subscriber',
             x = ' have auto renew feature or not ?',) +
        theme_ipsum() +
        ylab("Automatic Subscription")
      
      ggplotly(auto_renew_plot, tooltip = "text")
    })
  
  
  rf_model_final <- readRDS("rf_model_final.RDS")
  log_info <- function(var_name, value) {
    cat(file = stderr(), paste("[info]", var_name, "is:", value, "\n"))
  }
  
  datainput <- reactive({
    # Check that the number of rows in each column is 1 before creating the next dataframe.
    # log_info("input$slide100", as.numeric(input$slide100)) # This column causeed ERROR.
    go <- input$go
    churn_df <- data.frame(
      plan_list_price = max(wsdm_model_test$plan_list_price),
      actual_amount_paid = max(wsdm_model_test$actual_amount_paid),
      is_auto_renew = as.factor(input$automaticsubscription),
      year_registration = max(wsdm_model_test$year_registration),
      sum_num_25 = mean(wsdm_model_test$sum_num_25),
      sum_num_50 = mean(wsdm_model_test$sum_num_50),
      sum_num_75 = mean(wsdm_model_test$sum_num_75),
      sum_num_985 = mean(wsdm_model_test$sum_num_985),
      sum_num_100 = mean(wsdm_model_test$sum_num_100),
      num_unq_sum = mean(wsdm_model_test$num_unq_sum),
      # total_secs_sum = mean(wsdm_model_test$total_secs_sum),
      plan_list_price_mean = mean(wsdm_model_test$plan_list_price_mean),
      actual_amount_paid_mean = mean(wsdm_model_test$actual_amount_paid),
      avg_time_perday = mean(wsdm_model_test$avg_time_perday),
      unq_track = input$slideuniquetrack,
      num_25_perday = mean(wsdm_model_test$num_25_perday),
      num_50_perday = mean(wsdm_model_test$num_50_perday),
      num_75_perday = mean(wsdm_model_test$num_75_perday),
      num_985_perday = mean(wsdm_model_test$num_985_perday),
      num_100_perday = as.numeric(input$slideof100),
      loyality_range  = as.factor(input$loyalinput),
      price_day =  mean(wsdm_model_test$price_day),
      price_range = as.factor(input$pricerange),
      listening_hour = as.numeric(input$slidelisten)
    )
    
    levels(churn_df$is_auto_renew) <-
      levels(wsdm_model_train_final$is_auto_renew)
    levels(churn_df$loyality_range) <-
      levels(wsdm_model_train_final$loyality_range)
    levels(churn_df$price_range) <-
      levels(wsdm_model_train_final$price_range)
    print(is.data.frame(churn_df))
    print(churn_df)
    print(str(churn_df))
    
    rfprediction <- predict(rf_model_final, churn_df, type = "prob")
    rfprediction <- as.data.frame(rfprediction)
    print(rfprediction)
    print(is.data.frame(rfprediction))
    rfprediction
  })
  
  output$predchurn <- renderInfoBox(infoBox(
    "Probability Churn",
    paste0(datainput()$`1` * 100, "%"),
    icon = icon("list"),
    color = "blue",
    fill = TRUE
  ))
  
  output$predstay <- renderInfoBox(infoBox(
    "Probability Stay",
    paste0((1 - datainput()$`1`) * 100, "%"),
    icon = icon("list"),
    color = "blue",
    fill = TRUE
  ))
  
  # Random Forest Variable Importance
  output$Varimp <- renderPlotly({
    varimp_forest_plotly <-
      ggplot(varmp_df, aes(
        x = `mean decrease gini`,
        y = reorder(new_variables, `mean decrease gini`)
      )) +
      geom_col(aes(fill = `mean decrease gini`, text = tooltip)) +
      scale_fill_continuous(low = "red", high = "blue") +
      guides(fill = "none") +
      labs(
        title = "Variable importance in Random Forest Model",
        subtitle = "KKBOXX Online Music Streaming",
        x = "variable importance",
        y = NULL
      ) +
      theme_ipsum()
    
    ggplotly(varimp_forest_plotly, tooltip = "text")
  })
  
  # Confusion Matrix
  output$confusion_matrix <- renderTable({
    forest_model_evalmat
  })
  
  # the following codes are for EDA
  # Generate data summaries
  output$summary <- renderPrint({
    summary(raw_df)
  })
  output$str <- renderPrint({
    str(raw_df)
  })
  
  # get new dataset sample for plotting
  idx <- reactive({
    if (input$sampleType == "first") {
      1:input$sampleSize
    } else {
      set.seed(input$sampleSeed)
      sample(nrow(raw_df), input$sampleSize)
    }
  })
  df <- reactive(raw_df[idx(), , drop = FALSE])
  
  # Get head of selected data
  output$snippet <- renderPrint({
    head(df(), n = 15)
  })
  
  # get plot type
  # * 2: both numeric variables
  # * 1: one numeric, one non-numeric variable
  # * 0: both non-numeric variables
  # * -1: only one variable provided
  plot_type <- reactive({
    if (input$y != "None")
      is.numeric(raw_df[[input$x]]) + is.numeric(raw_df[[input$y]])
    else-1
  })
  
  # Create plot
  output$plot <- renderPlot({
    if (plot_type() == 2) {
      # both numeric variables: scatterplot
      # also allow for color, jitter & smoothing
      p <- ggplot(df(), aes_string(x = input$x, y = input$y))
      
      if (input$jitter)
        p <- p + geom_jitter(alpha = 0.5)
      else
        p <- p + geom_point(alpha = 0.5)
      
      if (input$smooth)
        p <- p + geom_smooth()
      
      # color change
      if (input$color != "None")
        p <- p + aes_string(color = input$color)
    } else if (plot_type() == 1) {
      # one numeric var, one character var: boxplot
      # allow color, don't allow jitter or smoothing
      p <-
        p <- ggplot(df(), aes_string(x = input$x, y = input$y)) +
        geom_boxplot()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    } else if (plot_type() == 0) {
      # two character variables: heatmap
      # don't allow color, jitter or smoothing
      temp_df <-
        reactive(df()[, c(input$x, input$y), drop = FALSE] %>%
                   group_by(across()) %>%
                   summarize(count = n()))
      p <- ggplot(temp_df(),
                  mapping = aes_string(
                    x = input$x,
                    y = input$y,
                    fill = "count"
                  )) +
        geom_tile() +
        scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
    } else {
      # only one variable: univariate plot
      # allow color, don't allow jitter or smoothing
      p <- ggplot(df(), aes_string(x = input$x))
      
      if (is.numeric(raw_df[[input$x]]))
        p <- p + geom_histogram()
      else
        p <- p + geom_bar()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    }
    # add title
    if (plot_type() >= 0) {
      p <- p + labs(title = paste(input$y, "vs.", input$x))
    } else {
      p <- p + labs(title = paste("Distribution of", input$x))
    }
    # add styling
    p <- p +
      theme_bw() +
      theme(
        plot.title = element_text(
          size = rel(1.8),
          face = "bold",
          hjust = 0.5
        ),
        axis.title = element_text(size = rel(1.5))
      )
    print(p)
  }, height = 500)
}

# Run the application -----------
shinyApp(ui = ui, server = server)

