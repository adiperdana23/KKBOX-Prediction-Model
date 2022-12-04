#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(plotly)
library(glue)
library(caret)
library(lubridate)
library(GGally)
library(yardstick)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)
library(shinydashboard)
library(shinythemes)
hrbrthemes::import_roboto_condensed()

#copy library
library(tibble)
library(psych)
library(forcats)
library(stargazer)
library(caret)
library(gmodels)
library(BBmisc)
library(partykit)
library(lubridate)
library(psych)
library(rpart)
library(rattle)
library(rpart.plot)

library(tidyverse)
library(plotly)
library(shiny)
library(dplyr)
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
# GLOBAL
# Model Dataset
## Load the dataset for model

wsdm_for_registration_app <-wsdm_for_registration %>% 
  head(14) %>% 
  select(-tooltip)

forest_viz_auto_renew<- forest_viz %>% 
  select(is_auto_renew,is_churn) %>% 
  group_by(is_auto_renew,is_churn) %>% 
  count() %>% 
  spread(is_churn,n) %>% 
  ungroup %>% 
  drop_na()

forest_viz_auto_renew$is_auto_renew <-ifelse(forest_viz_auto_renew$is_auto_renew==1,"has auto renew feature","doesn't have auto renew feature")
forest_viz_auto_renew$is_auto_renew <- as.factor(forest_viz_auto_renew$is_auto_renew)

forest_viz_auto_renew <- forest_viz_auto_renew %>% 
  pivot_longer(-is_auto_renew,names_to = "condition",names_transform = list(condition =as.factor))

forest_viz_auto_renew <- forest_viz_auto_renew %>% 
  mutate(`has auto renew feature?` = is_auto_renew,
         `churn or not` = condition,
         `number of subscriber` = value)

forest_viz_auto_renew_plot <- forest_viz_auto_renew %>% 
  select(-is_auto_renew,-condition,-value)

forest_viz_loyality <-forest_viz %>% 
  select(loyality_range,is_churn) %>% 
  group_by(loyality_range,is_churn) %>% 
  count() %>% 
  spread(is_churn,n) %>% 
  ungroup %>% 
  drop_na()


forest_viz_plot_lh <- forest_viz %>% 
  select(listening_hour,is_churn) %>% 
  group_by(listening_hour,is_churn) %>% 
  count() %>% 
  spread(is_churn,n) %>% 
  ungroup %>% 
  drop_na()

forest_viz_ggplot <-forest_viz_plot_lh

forest_viz_ggplot <- forest_viz_ggplot %>% 
  pivot_longer(-listening_hour,names_to = "condition",names_transform = list(condition =as.factor))

# Random Forest Model

forest_model_lr <-readRDS("forest_model_lr.RDS")

# Random Forest test dataframe

forest_test_lr <- readRDS('forest_test_lr.RDS')

# test dataset wrangling

forest_test_lr$is_auto_renew <-ifelse(forest_test_lr$is_auto_renew==1,"yes","no")
forest_test_lr$is_auto_renew <- as.factor(forest_test_lr$is_auto_renew)
forest_test_lr$unq_track <- round(forest_test_lr$unq_track,0)
forest_test_lr$unq_track <- round(forest_test_lr$unq_track,0)
forest_test_lr$num_100_perday <- round(forest_test_lr$num_100_perday,0)
# For Listening hour
forest_viz_ggplot <- readRDS("forest_viz_ggplot.RDS")
# For Registration line plot
wsdm_for_registration <- readRDS("wsdm_for_registration.RDS")
# For auto renew and loyalty
forest_viz <- readRDS("forest_viz.RDS")
# For Model
forest_model_lr <-readRDS("forest_model_lr.RDS")


# Variable Importance Random Forest 
data_viz_varmp <- readRDS("data_viz_varmp.RDS")
#theme

theme_algoritma <- theme(legend.background = element_rect(color="white", fill="#263238"),
                         plot.subtitle = element_text(size=6, color="white"),
                         panel.background = element_rect(fill="#dddddd"),
                         panel.border = element_rect(fill=NA),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color="darkgrey", linetype=2),
                         panel.grid.minor.y = element_blank(),
                         plot.background = element_rect(fill="#263238"),
                         text = element_text(color="white"),
                         axis.text = element_text(color="white"))


# Varimp plot
varimp_forest_plotly <-ggplot(data_viz_varmp, aes(x = `mean decrease gini`, y = reorder(variable, `mean decrease gini`))) +
  geom_col(aes(fill = `mean decrease gini`, text = tooltip)) +
  scale_fill_continuous(low = "gray", high = "blue") +
  guides(fill = "none") +
  labs(
    title = "Variable importance in Random Forest Model",
    subtitle = "KKBOXX Online Music Streaming",
    x = "variable importance",
    y = NULL
  ) +
  theme_minimal()





# Define UI for application that draws a histogram
ui <- navbarPage(h3("KKBOX Online Music Streaming Churn Dashboard"),
                 theme = shinytheme("spacelab"),
                 tabPanel(title=h3("| Home"),
                          fluidRow(box(width=12,solidHeader = TRUE, align = "center",
                                       imageOutput("home_img",height = "320px"))),
                          
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          fluidRow(box(width=12,solidHeader = TRUE, align = "center",
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       h4(strong("Project Description")),
                                       p(style="text-align: justify; font-size = 25px",
                                         "This Project is developed to make prediction about churn in Online Music Streaming KKBOX.
                                         The Prediction is made withRandom Forest Model and can describe about factor that affecting churn rate in Online Music
                                         Streaming"),
                                       tags$blockquote("For any service company that bills on a recurring basis, a key variable is the rate of churn. Harvard Business Review, March 2016"),
                                       hr())
                          )),
                 tabPanel(title=h3("| EDA"),
                          fluidRow(box(width = 12,
                                       h1("Explanatory Data Analysis"))),
                          fluidRow( box(width = 6, solidHeader = TRUE,
                                        plotlyOutput(outputId = "listen_plot")),
                                    box(width= 6, solidHeader = TRUE,
                                        plotOutput("growth_plot")
                                        
                                    )
                          ),
                          br(),
                          br(),
                          br(),
                          fluidRow(box(width = 6, solidHeader = TRUE,
                                       plotlyOutput(outputId = "loyality_plot")),
                                   box(width= 6, solidHeader = TRUE,
                                       plotlyOutput("renew_plot"))
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                          ) # tabpanel Second tap EDA
                          
                          
                          
                          
                 ),
                 tabPanel(title=h3("| Churn Prediction"),
                         sidebarPanel(fluidRow(box(width = 6,
                                       h1("Variable input"))),
                          fluidRow(box(width = 8,
                                       selectInput(inputId = "automaticsubscription",
                                                   label = "Has auto subscription feature?",
                                                   choices =   (forest_test_lr$is_auto_renew)),
                                                   selectInput(inputId = "pricerange",
                                                               label = "How much does the subscriber pay?",
                                                               choices =   (forest_test_lr$price_range)),
                                                   sliderInput(inputId = "slidelisten",
                                                              label = "The Duration of listening hour",
                                                              min = min(forest_test_lr$listening_hour),
                                                              max = max(forest_test_lr$listening_hour),
                                                              value = min(forest_test_lr$listening_hour),
                                                              step = 200)),
                                   box(width = 8,
                                       sliderInput(inputId = "slideloyal",
                                                   label = "How loyal are the subscriber?",
                                                   min = min(forest_test_lr$loyality_range),
                                                   max = max(forest_test_lr$loyality_range),
                                                   value = min(forest_test_lr$loyality_range),
                                                   step = 3),
                                       sliderInput(inputId = "slideuniquetrack",
                                                   label = "Number of listened song each day",
                                                   min = min(forest_test_lr$unq_track),
                                                   max = max(forest_test_lr$unq_track),
                                                   value = min(forest_test_lr$unq_track),
                                                   step = 5),
                                       sliderInput(inputId = "slide100",
                                                   label = "Number of listened song at full duration",
                                                   min = min(forest_test_lr$num_100_perday),
                                                   max = max(forest_test_lr$num_100_perday),
                                                   value = min(forest_test_lr$num_100_perday),
                                                   step = 5)
                                       
                                     
                                     
                                     
                                     
                                    
                                   )
                                 )
  
                          
                            
                          
                            
                            
                            
                          
                          
                          
                          
   
                          
                 ),
                 mainPanel(
                   fluidRow(box(width = 4,
                                actionButton("go", "Predict!", 
                                             style="display:inline-block;width:100%;text-align: center; font-size: 20px"),
                                br(),br(),
                                h2("Churn Probablity", style="display:inline-block;width:100%;text-align: center;font-size: 20px"),
                                tags$style(type='text/css', '#value {color: blue;font-size: 20px;text-align: center}'),
                                verbatimTextOutput("prediction")
                     
                     
                     
                     
                     
                   ))
                   
                   
                   
                 )
                 
                 
                 
                 )
                 
                 
                 
                 
)

# navbarpage close































# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$home_img <- renderImage({
    
    list(src = "www/dashboardimage.png",
         width="1200",
         height = "700")
  },deleteFile =F)
  
  
  output$listen_plot <- renderPlotly({forest_dataviz_ggplot <- ggplot(forest_viz_ggplot, aes(fill=condition, y=listening_hour, x=value,text=paste0( "Churn or stay: ", condition, "<br>",
                                                                                                                                                    "number of subscriber: ", scales::comma(value, 1), "<br>"))) + 
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_viridis(discrete = T) +
    ggtitle("churn by listening hour") +
    labs(x = 'Number of Subscriber',
         y = ' Listening Hours',
         title = 'Number of churned customer by listening hour')+
    theme_algoritma
  ggplotly(forest_dataviz_ggplot, tooltip = "text")
    
    
  })
  
  output$loyality_plot <- renderPlotly({forest_loyalty_scattter_tooltip <-ggplot(forest_viz_loyality, aes( x=loyality_range, y=churn,text=paste0("years :" ,loyality_range,"<br>",
                                                                                                                                                 "number of churn: ", scales::comma(churn, 1),"<br>",
                                                                                                                                                 "number of stay: ", scales::comma(stay, 1)))) + 
    geom_point(aes(color=loyality_range) ) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma)  +
    ggtitle("churn by listening hour") +
    labs(x = 'year',
         y = ' Number of churn customer',
         title = 'Relationship between loyality and churn customer')+
    theme_algoritma
    
  ggplotly(forest_loyalty_scattter_tooltip, tooltip = "text")
    
    
  })
  output$growth_plot <- renderPlot({
      ggplot( wsdm_for_registration_app,aes(x=year_registration, y=`number of subscriber`)) +
      geom_line(color="#69b3a2") +
      geom_point(color="#69b3a2", size=4) +
      scale_y_continuous(labels = scales::comma)  +
      ggtitle("Subscriber Growth year to year") +
      ylab("number of subscriber") +
      theme_ipsum()
  
  })
  
  output$renew_plot <-renderPlotly({auto_renew_plot <-ggplot(forest_viz_auto_renew_plot, aes(fill=`churn or not`, y=`has auto renew feature?`, x=`number of subscriber`,text=paste0( "Churn or stay: ", `churn or not`, "<br>",
                                                                                                                                                                                     "number of subscriber: ", scales::comma(`number of subscriber`, 1), "<br>"))) + 
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_viridis(discrete = T) +
    ggtitle("churn by auto renew feature") +
    labs(x = 'Number of Subscriber',
         y = ' have auto renew feature or not ?',
         title = 'Number of churned customer by auto renew feature')+
    theme_algoritma
    
    ggplotly(auto_renew_plot, tooltip = "text")})
  
  
  output$prediction <- renderText({
    if(input$go >0) {
    rfpred <- predict(forest_model_lr ,
                      prd_df = data.frame(
      plan_list_price = "149",
      actual_amount_paid = "149",
      is_auto_renew = as.factor(input$automaticsubscription),
      year_registration = 2005,
      sum_num_25 = 90,
      sum_num_50 = 45,
      sum_num_75 = 30,
      sum_num_985 =0,
      sum_num_100 = 2535,
      num_unq_sum = 2595,
      plan_list_price_mean = 139.06667,
      actual_amount_paid_mean = 149,
      avg_time_perday= 858.356563,
      unq_track = input$unq_track,
      num_25_perday = 0.113924051,
      num_50_perday = 0.056962025,
      num_75_perday = 0.037974684,
      num_985_perday = 0,
      num_25_perday = 0.113924051,
      num_50_perday= 0.056962025,
      num_75_perday = 0.037974684,
      num_985_perday = 0,
      num_100_perday = as.numeric(input$slide100),
      loyality_range  = as.numeric(input$slideloyal),
      price_day = 4.966667,
      price_range = as.factor(input$pricerange),
      listening_hour = as.numeric(input$slidelisten)),
      type = "prob") }
      
    churn_prediction <- rfpred[1]
    paste(churn_prediction*100,"%")
    
  
    
    
  })
  

}



# Run the application 
shinyApp(ui = ui, server = server)