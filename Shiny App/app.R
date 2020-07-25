library(shiny)
library(tidyverse)
library(Matrix)


# ui.R

item_list <- readRDS("item_list.rds")
ui <- fluidPage(
    
    # App title ----
    titlePanel(h1("Recommender System For Instacart Online Grocery Store", align = "center")),
    h3("A Capstone Project Presented to Information and Data Analytics Foundation", align = "center"),
    
    fluidRow(
        
        # Input selection
        column(6, 
               # INPUT
               h3("Purchase item(s) from the store, by selecting from the dropdown list"),    
               wellPanel(
                   #selectInput("user", "User", choices = c("", user_list)),
                   selectInput("input_item1", "Item 1", choices = c("", item_list)),
                   selectInput("input_item2", "Item 2", choices = c("", item_list)),
                   selectInput("input_item3", "Item 3", choices = c("", item_list)),
                   selectInput("input_item4", "Item 4", choices = c("", item_list)),
                   selectInput("input_item5", "Item 5", choices = c("", item_list)),
                   selectInput("input_item6", "Item 6", choices = c("", item_list)),
                   selectInput("input_item7", "Item 7", choices = c("", item_list)),
                   selectInput("input_item8", "Item 8", choices = c("", item_list)),
                   selectInput("input_item9", "Item 9", choices = c("", item_list)),
                   selectInput("input_item10", "Item 10", choices = c("", item_list)),
                   actionButton("submit", "Complete Purchase")
               )
        ),
        
        # Output table
        column(6,
               h3("Because you purchased those item(s), we recommend these:"),     
               tableOutput("item_recom")
        )
    ),
    
    # COMMENTS    
    fluidRow(                                    
        column(12,
               p("Designed by Okuwoga Toluwalase, Okechukwu Princewill, Olanayo Olatunde.",
                 "For the full code, please visit the team", 
                 a("GitHub Page", href = "https://github.com/ToluwalaseOkuwoga/Instacart-Recommender-System", target="_blank"))
        ),
        
        column(12,
               p("The Instacart Online Grocery Shopping Dataset 2017, Accessed from",
                 a("Here", href = "https://www.instacart.com/datasets/grocery-shopping-2017"))
        )
    )
)


# server.R

# Load algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")
past_orders_matrix <- readRDS("past_orders_matrix.rds")

server <- function(input,output) {
    
    output$item_recom <- renderTable({
        # react to submit button
        input$submit
        # gather input in string
        customer_order <- 
            isolate(
                
                unique(c(input$input_item1, input$input_item2, input$input_item3, 
                         input$input_item4, input$input_item5, input$input_item6, input$input_item7, input$input_item8, input$input_item9, input$input_item10))
            )
        
        
        # put in a matrix format
        new_order <- item_list %>%
            # Add a 'value' column with 1's for customer order items
            mutate(value = as.numeric(item %in% customer_order)) %>%
            # Spread into sparse matrix format
            spread(key = item, value = value) %>%
            # Change to a matrix
            as.matrix() %>% 
            # Convert to class "dgCMatrix"
            as("dgCMatrix")
        
        # Add new order to retail matrix - binding 2 matrices
        all_orders_dgc <- t(rbind(new_order,past_orders_matrix))
        
        # Set items to predict range
        items_to_predict <- which(all_orders_dgc[0:50,1] == 0)
        #items_to_predict <- 2:20
        # Set user to 1
        users <- c(1)
        # Set prediction indices
        prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
        
        # Run UBCF model
        recomm <- predict_cf(all_orders_dgc, prediction_indices, 
                             "ubcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
        
        # Put recommended products into a dataframe
        recomm[,users] %>% 
            as.data.frame() %>% 
            rownames_to_column('Recommended Items') %>% 
            filter(.>0) %>% 
            select('Recommended Items')
        
    })
}

shinyApp(ui = ui, server = server)