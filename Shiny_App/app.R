# Load in the Data
load("Data/Refugee.RData")
# Load libraries
library(shiny)
library(shinydashboard)
library(ggraph)
library(tidyverse)
library(magrittr)
library(tidygraph)
library(ggiraph)
library(countrycode)
library(htmltools)
library(networkD3)
library(maps)

ui <- dashboardPage(
  dashboardHeader(title = "Major Refugee Pathway Exploration", titleWidth = 400),
  dashboardSidebar(sidebarMenu(
    menuItem("Comparing Different Layouts", tabName = "Year"), 
    menuItem("Filtering by Volume of Refugees", tabName = "Volume_Faceting"), 
    menuItem("Interactive Graphs", tabName = "Mouse"), 
    menuItem("Manipulatable Graph", icon = icon("bolt"), tabName = "D3")
  ), sliderInput("Years", "Year:", 2000, 2013, 2000, animate = animationOptions(3500, loop = TRUE)), 
  sliderInput("num", "Number of Pathways Displayed:", 2, 185, 20, animate = animationOptions(3500, loop = TRUE)), 
  checkboxInput("text", "Display Text", value = FALSE), 
  radioButtons("Size", "Size Country Nodes on:", choices = list(`Refugee Output` = "Origin", `Refugee Intake` = "Destination", Both = "Both", None = "None"), selected = "None"),
  actionButton("Manip", "Regenerate Graph", icon = icon("bolt"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Year",
              fluidRow(
                box(plotOutput("WorldPlot"), title = "Geographic Map Representation"), 
                box(plotOutput("GraphPlot"), title = "Traditional Network Graph Representation")
              )
      ),
      tabItem(tabName = "Volume_Faceting",
              fluidRow(
                box(plotOutput("Vol_Low"), title = "Low Volume Pathways", background = "blue", collapsible = TRUE), 
                box(plotOutput("Vol_Med"), title = "Medium Volume Pathways", background = "yellow", collapsible = TRUE)
              ),
              fluidRow(
                column(width = 12,
                       box(plotOutput("Vol_High"), title = "High Volume Pathways", background = "red", collapsible = TRUE),
                       offset = 3
                )
              )
      ), 
      tabItem(tabName = "Mouse", 
              fluidRow(
                box(ggiraphOutput("inter"), title = "Hover For Country Name", collapsible = TRUE),
                box(ggiraphOutput("inter_zoom"), title = "Double Click For Zoom", footer = "Change Sidebar Options to Reset", collapsible = TRUE)
              )
      ),
      tabItem(tabName = "D3",
              fluidRow((
                box(forceNetworkOutput("D3"), title = "Manipulate Plot by Clicking and Dragging", width = 12)
              )))
    )
  )
)


server <- function(input, output){
  cust_countrycode = function(src){
    countrycode(src, "country.name", "continent", custom_match = c(Tibet = "Asia"))
  }
  
  observe({
    # Define df as a clean and reactive data frame
    df <- refugees_clean %>% 
      filter(Year == !!input$Years) %>% 
      top_n(n = input$num, Refugee_Total)
    
    # Define Destination and Origin Sizes
    if(input$Size %in% c("Origin", "Destination")){
      Size_Group <- df %>%
        select(Origin, Destination, Refugee_Total, Continent_Dest, Year) %>%
        when(
          input$Size == "Origin" ~ group_by(., Origin),
          input$Size == "Destination" ~ group_by(., Destination)
        ) %>%
        summarise(Size = sum(Refugee_Total)) %>%
        `names<-`(c("name", "Size"))
    }
    if(input$Size == "Both"){
      Size_Group <- df %>%
        select(Origin, Destination, Refugee_Total, Continent_Dest, Year) %>%
        group_by(Origin) %>%
        summarise(Size = sum(Refugee_Total)) %>% 
        `names<-`(c("name", "Size")) %>% 
        full_join(df %>%
                    select(Origin, Destination, Refugee_Total, Continent_Dest, Year) %>%
                    group_by(Destination) %>%
                    summarise(Size = sum(Refugee_Total)) %>% 
                    `names<-`(c("name", "Size")), by = "name") %>% 
        mutate(Size.x = if_else(is.na(Size.x), 0L, Size.x), 
               Size.y = if_else(is.na(Size.y), 0L, Size.y), 
               Size = Size.x + Size.y) %>% 
        select(name, Size)
    }
    
    # Create a World Plot ggplot object that reacts to changes
    World_Plot <- df %>%
      ggplot() +
      borders("world", colour="gray50", fill="gray50") +
      geom_point(aes(lon_orig, lat_orig), colour = "red", alpha = 0.3) +
      geom_point(aes(lon_dest, lat_dest), col = "blue", alpha = 0.3) +
      geom_segment(aes(x = lon_orig, xend = lon_dest, y = lat_orig, yend = lat_dest), alpha = 0.9, arrow = arrow(length = unit(2.5, "mm")))
    
    output$WorldPlot <- renderPlot({World_Plot})
    
    # Create Graph Plot ggplot object that is reactive to changes
    Graph_Plot <- df %>%
      select(Origin, Destination, Refugee_Total, Continent_Dest) %>%
      as_tbl_graph() %>%
      mutate(Continent = cust_countrycode(name)) %>%
      when(
        input$Size != "None" ~ mutate(., Size = tibble(name) %>% left_join(Size_Group) %>% pull(Size),
                                      Size = if_else(is.na(Size), 0L, Size)),
        ~ .
      ) %>%
      ggraph() +
      geom_edge_fan(arrow = arrow(length = unit(2.5, "mm")), end_cap = circle(2.5, "mm"), spread = 1, alpha = 0.6) +
      {if(input$Size != "None"){
        geom_node_point(aes(colour = Continent, size = Size))
      }} +
      {if(input$Size == "None"){
        geom_node_point(aes(colour = Continent))
      }} +
      {if(input$text){
        geom_node_text(aes(label = name), repel = TRUE)
      }} +
      theme_graph() +
      scale_size_continuous(labels = scales::comma)
    
    output$GraphPlot <- renderPlot({Graph_Plot})
    
    # Define a function that facets based on volume and takes character High Medium or Low
    facet <- function(Vol){
      df %>%
        mutate(Volume = ntile(Refugee_Total, 3), Volume = case_when(
          Volume == 1 ~ "Low", 
          Volume == 2 ~ "Medium",
          Volume == 3 ~ "High"
        )) %>%
        filter(Volume == !! Vol) %>%
        select(Origin, Destination, Refugee_Total, Continent_Dest, Volume) %>%
        as_tbl_graph() %>%
        mutate(Continent = cust_countrycode(name)) %>%
        when(
          input$Size != "None" ~ mutate(., Size = tibble(name) %>% left_join(Size_Group) %>% pull(Size),
                                        Size = if_else(is.na(Size), 0L, Size)),
          ~ .
        ) %>% 
        ggraph() +
        geom_edge_fan(arrow = arrow(length = unit(2.5, "mm")), end_cap = circle(2.5, "mm"), spread = 1, alpha = 0.6) +
        {if(input$Size != "None"){
          geom_node_point(aes(colour = Continent, size = Size))
        }} +
        {if(input$Size == "None"){
          geom_node_point(aes(colour = Continent))
        }} +
        {if(input$text){
          geom_node_text(aes(label = name), repel = TRUE)
        }} +
        theme_graph() +
        scale_size_continuous(labels = scales::comma)
    }
    
    # Use the facet function to save the ggplot objects
    low <- facet("Low")
    med <- facet("Medium") 
    high <- facet("High")
    
    # Render the ggplot objects as outputs
    output$Vol_Low <- renderPlot({low})
    output$Vol_Med <- renderPlot({med})
    output$Vol_High <- renderPlot({high})
    
    # Render an interactive mouse over graph plot
    gg_interactive <-df %>% 
      select(Origin, Destination, Refugee_Total) %>%
      as_tbl_graph() %>%
      mutate(Continent = cust_countrycode(name), tooltip = paste("Country:", name)) %>%
      when(
        input$Size != "None" ~ mutate(., Size = tibble(name) %>% left_join(Size_Group) %>% pull(Size),
                                      Size = if_else(is.na(Size), 0L, Size)),
        ~ .
      ) %>% 
      ggraph() +
      geom_edge_fan(arrow = arrow(length = unit(2.5, "mm")), end_cap = circle(2.5, "mm"), spread = 1, alpha = 0.6) +
      {if(input$Size != "None"){
        geom_point_interactive(aes(x = x, y = y, tooltip = htmlEscape(tooltip, TRUE), size = Size, colour = Continent))
      }} +
      {if(input$Size == "None"){
        geom_point_interactive(aes(x = x, y = y, tooltip = htmlEscape(tooltip, TRUE), size = 1, colour = Continent))
      }} +
      {if(input$Size == "None"){
        guides(size = FALSE)
      }} +
      theme_graph() +
      scale_size_continuous(labels = scales::comma)
    output$inter <- renderggiraph(ggiraph(code = {print(gg_interactive)}))
    
    
    
    # Zoom enabled Graph
    gg_interactive_zoom <-df %>% 
      select(Origin, Destination, Refugee_Total) %>%
      as_tbl_graph() %>%
      mutate(Continent = cust_countrycode(name), tooltip = paste("Country:", name)) %>%
      when(
        input$Size != "None" ~ mutate(., Size = tibble(name) %>% left_join(Size_Group) %>% pull(Size),
                                      Size = if_else(is.na(Size), 0L, Size)),
        ~ .
      ) %>% 
      ggraph() +
      geom_edge_fan(arrow = arrow(length = unit(2.5, "mm")), end_cap = circle(2.5, "mm"), spread = 1, alpha = 0.6) +
      {if(input$Size != "None"){
        geom_point_interactive(aes(x = x, y = y, tooltip = htmlEscape(tooltip, TRUE), size = Size, colour = Continent))
      }} +
      {if(input$Size == "None"){
        geom_point_interactive(aes(x = x, y = y, tooltip = htmlEscape(tooltip, TRUE), size = 1, colour = Continent))
      }} +
      {if(input$Size == "None"){
        guides(size = FALSE)
      }} +
      theme_graph() +
      scale_size_continuous(labels = scales::comma)
    output$inter_zoom <- renderggiraph(ggiraph(code = {print(gg_interactive_zoom)}, zoom_max = 5))
    
    # Manipulatable Graph
    
    output$D3 <- renderForceNetwork({
      
      input$Manip
      isolate({
        force_group <- df %>%
          select(Origin, Destination, Refugee_Total, Continent_Dest) %>%
          as_tbl_graph() %>%
          mutate(Continent = cust_countrycode(name)) %>%
          pull(Continent)
        
        graph_d3_form <- df %>%
          select(Origin, Destination, Refugee_Total, Continent_Dest) %>%
          as_tbl_graph() %>%
          mutate(Continent = cust_countrycode(name)) %>%
          igraph_to_networkD3(group = force_group)
        
        forceNetwork(Links = graph_d3_form$links, Nodes = graph_d3_form$nodes,
                     Source = "source", Target = "target", NodeID = "name",
                     Group = "group", opacity = 0.9)
      })


    })

  })
  
}
shinyApp(ui, server)

