library(tidyverse)
library(readr)

## data prep

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

nodes <- actors_long |>
  filter(actor1 == actor2) |>
  dplyr::select(name = actor1, total_scenes = weight) |>
  mutate(community = case_when(
    name %in% c("Hugh Grant", "Emma Thompson", "Liam Neeson", 
                "Alan Rickman", "Heike Makatsch", "Rowan Atkinson") ~ "community 1",
    name %in% c("Kris Marshall", "Abdul Salis", "Colin Firth", "Keira Knightley") ~ "community 2",
    .default = "floaters"
    ))

edges <- actors_long |>
  filter(actor1 != actor2, weight > 0) |>
  dplyr::select(from = actor1, to = actor2, weight)

## static visualizations

library(plotly)

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

library(tidygraph)
library(ggraph)

network_obj <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

ggraph(network_obj, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "snow4", show.legend = FALSE) +
  geom_node_point(aes(color = community, size = total_scenes), show.legend = FALSE) +
  geom_node_text(aes(label = gsub(" ", "\n", name)), size = 2.5) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size(range = c(15, 22)) +
  scale_color_manual(values = c("community 1" = "turquoise1", 
                                "community 2" = "yellow", 
                                "floaters" = "chartreuse")) +
  theme_void() 


## shiny app

library(shiny)

actors <- appearances |>
  filter(appearances == 1) |>
  group_by(actors) |>
  summarize(first_scene = min(scene_number)) |>
  arrange(first_scene) |> 
  pull(actors)

ui <- fluidPage(
  titlePanel("Love, Actually Mapped"), ## Claude AI
  ## Claude AI: how to add taps to shiny app so users can switch between visualizations
    ## tabsetPanel(tabPanel(), tabPanel(), etc.)
  tabsetPanel(
    tabPanel("Scene Appearances",
              selectInput("actor",
                          label = "Select an actor:",
                          choices = actors,
                          selected = actors[1]),
             plotlyOutput("appearances_seg"),
             ## Claude AI: how to add table title
              ## add text output between plot and table: h3(textOutput("..."))
             h3(textOutput("actor_table_title")),
             tableOutput("actor_table")
    ),
    
    tabPanel("Character Network",
             checkboxGroupInput("actors",
                                label = "Select at least two actors:",
                                choices = actors,
                                selected = actors[1:2]),
             plotOutput("char_network"))
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
  
  output$char_network <- renderPlot({
    network_obj <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
    
    ggraph(network_obj, layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.4, color = "snow4", show.legend = FALSE) +
      geom_node_point(aes(color = community, size = total_scenes), show.legend = FALSE) +
      geom_node_text(aes(label = gsub(" ", "\n", name)), size = 2.5) +
      scale_edge_width(range = c(0.5, 3)) +
      scale_size(range = c(22, 32)) +
      scale_color_manual(values = c("community 1" = "turquoise1", 
                                    "community 2" = "yellow", 
                                    "floaters" = "chartreuse")) +
      theme_void() 
  })
  
}

shinyApp(ui, server)

