library(tidyverse)
library(readr)

## data tidying

library(readr)
library(tidyverse)

appearances <- read_csv("data/love_actually_appearances.csv")

appearances <- appearances |>
  ## only keep rows with data, after 71 everything is NA b/c there are no more scenes
  slice(1:71) |>
  mutate(scene_number = row_number()) |>
  pivot_longer(cols = 2:15, names_to = "actors", values_to = "appearances") |>
  mutate(appearances = if_else(appearances == "TRUE", 1, 0)) |>
  replace_na(list(appearances = 0)) |>
  mutate(appearances = as.factor(appearances),
         actors = gsub("_", " ", actors),
         actors = str_to_title(actors))

## static visualizations

scenes <- appearances |>
  filter(appearances == 1) |>
  arrange(actors, scene_number) |>
  group_by(actors) |>
  mutate(next_scene = lead(scene_number)) |> # next scene they appear in
  ungroup() |>
  filter(!is.na(next_scene))    

order_of_appearance <- appearances |>
  filter(appearances == 1) |>
  group_by(actors) |>
  summarize(first_scene = min(scene_number)) |>
  arrange(desc(first_scene)) |> 
  pull(actors)

scenes <- scenes |>
  mutate(actors = factor(actors, levels = order_of_appearance))

appearance_points <- appearances |>
  filter(appearances == 1) |>
  mutate(actors = factor(actors, levels = order_of_appearance))

## shiny app

library(shiny)
library(plotly)

actors <- appearances |>
  filter(appearances == 1) |>
  group_by(actors) |>
  summarize(first_scene = min(scene_number)) |>
  arrange(first_scene) |> 
  pull(actors)

ui <- fluidPage(
  titlePanel("Love, Actually Mapped"), ## Claude AI
  sidebarLayout(
    sidebarPanel(
      
      ## select an actor
      selectInput("actor",
                  label = "Select an actor:",
                  choices = actors,
                  selected = "Hugh Grant")
    ),
    
    mainPanel(
      plotlyOutput("appearances_seg"),
      ## Claude AI: how to add table title
        ## add text output between plot and table: h3(textOutput("...")) 
      h3(textOutput("actor_table_title")), 
      tableOutput("actor_table")
    )
  )
)

server <- function(input, output, session) {
  
  appearances_reactive_df <- reactive({
    appearance_points |>
      filter(actors == input$actor) |>
      select(`Scene Number` = scene_number, 
             Description = scenes) |>
      arrange(`Scene Number`)
  })
  
  ## Claude AI: how to add table title
    ## render title text: renderText({paste(...)})
  output$actor_table_title <- renderText({
    paste("Scenes Featuring", input$actor)
  })
  
  output$actor_table <- renderTable({
    appearances_reactive_df()
  })
  
  output$appearances_seg <- renderPlotly({
    s <- ggplot() +
      geom_segment(data = scenes,
                   aes(x = scene_number, xend = next_scene,
                       y = actors, yend = actors), color = "magenta1") +
      geom_point(data = appearance_points, 
                 aes(x = scene_number, y = actors, 
                     text = paste(scene_number)), 
                 color = "red1") +
      labs(title = "All Scene Appearances",
           x = "Scene Number",
           y = NULL) +
      theme_minimal()
    
    ggplotly(s, tooltip = "text")
  })
  
}

shinyApp(ui, server)

