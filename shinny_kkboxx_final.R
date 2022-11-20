#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(glue)
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
library(caret)
library(GGally)
library(yardstick)
library(randomForest)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)
library(shinydashboard)
install.packages("shinythemes")
library(shinythemes)

# Model Dataset
## Load the dataset for model

# df train
wsdm_finaldf_train
head(wsdm_finaldf_train)
# df test for decision tree
wsdm_finaldf_test 
# tuning model for logis
wsdm_model2_test 
wsdm_model2_train 
# logistic Model
kkbox_logistic_model2 

# For Listening hour
forest_viz_ggplot
#For loyalty and  churn
forest_viz_loyality
# For Registration line plot
wsdm_for_registration
# For auto renew
forest_viz_auto_renew_plot


# Variable Importance Random Forest
data_viz_varmp

# Varimp plot
varimp_forest_plotly <-ggplot(data_viz_varmp, aes(x = `mean decrease gini`, y = reorder(variable, `mean decrease gini`))) +
  geom_col(aes(fill = `mean decrease gini`, text = tooltip)) +
  scale_fill_continuous(low = "gray", high = "blue") +
  guides(fill = FALSE) +
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
tabPanel(title=h3("| Variable Importance"),
         fluidRow(box(width = 12,
                      h1("Variable Importance Random Forest"))),
         
         fluidRow( box(width = 6, solidHeader = TRUE,
                       plotlyOutput(outputId = "varimp_forest")))
                       
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
    
    
    output$listen_plot <- renderPlotly({ggplot(forest_viz_ggplot, aes(fill=condition, y=listening_hour, x=value,text=paste0( "Churn or stay: ", condition, "<br>",
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
    
    output$loyality_plot <- renderPlotly({ggplot(forest_viz_loyality, aes( x=loyality_range, y=churn,text=paste0("years :" ,loyality_range,"<br>",
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
  output$growth_plot <- renderPlot({wsdm_for_registration  %>%
      head(14) %>%
      ggplot( aes(x=year_registration, y=`number of subscriber`)) +
      geom_line() +
      geom_point(color="#69b3a2", size=4) +
      scale_y_continuous(labels = scales::comma)  +
      ggtitle("Subscriber Growth year to year") +
      ylab("number of subscriber") +
      theme_ipsum()})
  
  output$renew_plot <-renderPlotly({ggplot(forest_viz_auto_renew_plot, aes(fill=`churn or not`, y=`has auto renew feature?`, x=`number of subscriber`,text=paste0( "Churn or stay: ", `churn or not`, "<br>",
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
  
  output$varimp_forest <-renderPlotly({ggplot(data_viz_varmp, aes(x = `mean decrease gini`, y = reorder(variable, `mean decrease gini`))) +
      geom_col(aes(fill = `mean decrease gini`, text = tooltip)) +
      scale_fill_continuous(low = "gray", high = "blue") +
      guides(fill = FALSE) +
      labs(
        title = "Variable importance in Random Forest Model",
        subtitle = "KKBOXX Online Music Streaming",
        x = "variable importance",
        y = NULL
      ) +
      theme_minimal()
    ggplotly(varimp_forest_plotly, tooltip = "text")
    
    
    
    
    
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
