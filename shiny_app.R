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

ggplot() +
  geom_segment(data = scenes,
               aes(x = scene_number, xend = next_scene,
                   y = actors, yend = actors), color = "olivedrab") +
  geom_point(data = appearance_points, 
             aes(x = scene_number, y = actors), color = "firebrick") +
  labs(title = "All Scene Appearances",
       x = "Scene Number",
       y = NULL) +
  theme_minimal()

## shiny app

library(shiny)
library(plotly)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    mainPanel(
      plotlyOutput("appearances_seg")
    )
  )
)

server <- function(input, output, session) {
  
  output$appearances_seg <- renderPlotly({
    s <- ggplot() +
      geom_segment(data = scenes,
                   aes(x = scene_number, xend = next_scene,
                       y = actors, yend = actors), color = "olivedrab") +
      geom_point(data = appearance_points, 
                 aes(x = scene_number, y = actors, 
                     text = paste0("Scene ", scene_number, "<br>", scenes)), 
                 color = "firebrick") +
      labs(title = "All Scene Appearances",
           x = "Scene Number",
           y = NULL) +
      theme_minimal()
    
    ggplotly(s, tooltip = "text")
  })
  
}

shinyApp(ui, server)
