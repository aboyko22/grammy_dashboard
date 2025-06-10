# load packages ----
library(tidyverse)
library(shiny)
library(bslib)
library(ggthemes)
library(gt)
library(gtExtras)
library(DT)

# load local data ----
data <- read_csv("dashboard_data/modeling_data.csv")
testing_data <- read_csv("dashboard_data/testing_data.csv")
audio_features <- read_csv("dashboard_data/audio_feature_descriptions.csv")
predictions <- read_csv("dashboard_data/predictions.csv")

predictions <- predictions %>%
  mutate(label = paste0(round(Grammy_Prob * 100, digits = 1), "%"))

testing_data  <- predictions %>%
  left_join(testing_data, by = join_by(song == song, artist == artist))

histogram_vars <- colnames(data %>% select(-c(artist, song, won_grammy, explicit, key, mode, time_signature, duration_sec, started_charting, weeks_on_chart, peak_position, starts_with("charted"))))
histogram_labels <- c("Danceability", "Energy", "Loudness", "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo (bpm)")

col_vars <- c('danceability', 'energy', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence') %>% sort()
col_labels <- str_to_title(col_vars)

# ui ----
ui <- page_navbar(
  title = tagList(icon("wand-magic-sparkles", class = "me-2"), "Predicting Grammy Winning Songs"),
  tags$head(tags$link(rel = "shortcut icon", href = "https://img.icons8.com/?size=100&id=iHkQe1pmsq8Q&format=png&color=000000")),
  tags$style(HTML(".custom-card {--bs-card-spacer-y: 0em; padding-top: 10px;}
                  .sidebar {height: 97.7%;}
                  .data-container { max-height: 650px; overflow-y: auto;}
                   table.dataTable thead th {text-align: center !important;}
                   table.dataTable tbody td {text-align: left !important;}
                   table.dataTable {font-size: 14px !important;}
                   table.dataTable td {padding: 5px 5px !important;}
                  .dataTables_wrapper .dataTables_filter input {font-size: 14px !important;}")),
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  nav_spacer(),
  
  # variable info ----
  nav_panel(title = "Audio Features", icon = icon(name = "ear-listen", lib = "font-awesome"),
            sidebarLayout(
              sidebarPanel(class = "sidebar",
                           
                           selectInput(
                             inputId = "feature_select",
                             label = "Select an audio feature:",
                             choices = setNames(histogram_vars, histogram_labels),
                             selectize = TRUE),
                           
                           card(class = "custom-card", gt_output("feature_table"),
                                card_footer(gt::html("This definition is from <a href=https://onlyoneaman.medium.com/unleashing-the-power-of-audio-features-with-the-spotify-api-c544fda1af40 target='_blank'>Medium</a>. These metrics are developed internally by Spotify, and as of 2025, are no longer publicly available.")),
                                style = "height: 535px;")),
              
              mainPanel(
                card(full_screen = TRUE, plotOutput(outputId = "histogram"),
                     style = "height: 683px; background-color: #F0F0F0;")
                )
              )
            ),
  
  # in depth ----
  nav_panel(title = "Song Profile", icon = icon(name = "fingerprint", lib = "font-awesome"),
            
            sidebarLayout(
              sidebarPanel(class = "sidebar",
                           
                           card(style = "background-color: transparent; border-color: transparent; height: 47.5%;",
                                selectInput(
                                  inputId = "song",
                                  label = "Choose a song:",
                                  choices = "Lose Control",
                                  selectize = TRUE),
                                
                                selectInput(
                                  inputId = "feature",
                                  label = "Choose an Audio Feature:",
                                  choices = setNames(col_vars, col_labels),
                                  selected = NA,
                                  selectize = TRUE),
                           
                               sliderInput(
                                 inputId = "filter",
                                 label = "Filter by Audio Feature:",
                                 min = 0, max = 100, 
                                 value = c(35, 65))),
                           
                           card(style = "background-color: transparent;
                                       border-color: transparent; height: 49.5%",
                                       uiOutput("spotify_embed"))),
              
              mainPanel(
                fluidRow(
                  column(width = 6,
                    card(gt_output(outputId = "billboard"), p("Data as of May 31st"),
                         style = "height: 230px; background-color: #F0F0F0; font-size: 12px;")),
                  
                  column(width = 6,
                    card(style = "height: 230px; background-color: #F0F0F0; font-size: 12px;",
                         h5("Chance to Win Song of the Year"),
                      div(style = "display: flex; gap: 10px;",
                          div(style = "flex: 0 0 30%;", plotOutput("donut", width = "90%", height = "90%")),
                          div(style = "flex: 1;",
                              h6("This metric represents how confident our model is that this song belongs to the same group as past Song of the Year winners."),
                              h6("It is not a prediction of whether the song would win given the pool of nominees")))
                      )
                    )
                  ),

                fluidRow(
                  column(
                    width = 12,
                    card(full_screen = TRUE,
                         plotOutput(outputId = "song_features"),
                         style = "height: 437px; background-color: #F0F0F0;"
                    )
                  )
                )
              )
            )
  ),
  
  # data search ----
  nav_panel(title = "Search", icon = icon(name = "magnifying-glass", lib = "font-awesome"),
            card(full_screen = TRUE, DTOutput("main_table", height = "100%"), class = "data-container")),
  
  # sources page ----
  nav_panel(title = "Sources", icon = icon(name = "book-open", lib = "font-awesome"),
            
            card(style = "background-color: #F0F0F0; font-size: 20px;",
              div(h3("Data Sources"), br(),
                  p("The following datasets were used to obtain Spotify audio feature data to train the model."),
                  p("•", a("https://www.kaggle.com/datasets/rodolfofigueroa/spotify-12m-songs", href = "https://www.kaggle.com/datasets/rodolfofigueroa/spotify-12m-songs")),
                  p("•", a("https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset", href = "https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset")),
                  p("•", a("https://www.kaggle.com/datasets/tomigelo/spotify-audio-features", href = "https://www.kaggle.com/datasets/tomigelo/spotify-audio-features")),
                  p("•", a("https://www.kaggle.com/datasets/yamaerenay/spotify-dataset-19212020-600k-tracks", href = "https://www.kaggle.com/datasets/yamaerenay/spotify-dataset-19212020-600k-tracks")),
                  br(),
                  p("The following dataset was used to obtain Spotify audio feature data to test the model."),
                  p("•", a("https://www.kaggle.com/code/seanbearden/so-you-want-to-be-top-50", href = "https://www.kaggle.com/code/seanbearden/so-you-want-to-be-top-50")),
                  br(),
                  p("All billboard streaming data comes from the following source."),
                  p("•", a("https://github.com/utdata/rwd-billboard-data", href = "https://github.com/utdata/rwd-billboard-data"))))
  )
)

# server ----
server <- function(input, output, session) {
  
  observeEvent(input$feature, {
    req(input$feature)
    
    feature_data <- testing_data[[input$feature]]
    if (is.null(feature_data) || !is.numeric(feature_data)) return()
    
    feature_min <- floor(min(feature_data, na.rm = TRUE))
    feature_max <- ceiling(max(feature_data, na.rm = TRUE))
    range_diff <- feature_max - feature_min
    step_size <- if (range_diff <= 1) 0.1 else 1
    
    updateSliderInput(
      session,
      inputId = "filter",
      min = feature_min,
      max = feature_max,
      value = c(feature_min, feature_max),
      step = step_size
    )
    
  })
  
  toListen <- reactive({list(input$feature,input$filter)})
  
  observeEvent(toListen(), {
      
    req(input$feature, input$filter)

      filtered_songs <- testing_data %>%
        arrange(desc(Grammy_Prob)) %>%
        filter(.data[[input$feature]] >= input$filter[1], .data[[input$feature]] <= input$filter[2]) %>%
        pull(song)
      
      updateSelectInput(session, "song", choices = filtered_songs)

      if (input$song %in% filtered_songs) {
        selected_song <- input$song
      } else {
        selected_song <- NULL
      }
      
      updateSelectInput(
      session,
      inputId = "song",
      choices = filtered_songs,
      selected = selected_song
    )
      
  })

  output$feature_table <- render_gt({

    selection <- audio_features %>%
      filter(feature == input$feature_select)

    header_title <- str_to_title(selection$feature)

    selection %>%
      select(description) %>%
      gt() %>%
      tab_header(title = header_title) %>%
      tab_style(locations = cells_title(), cell_fill("plum")) %>%
      tab_style(locations = cells_title(), cell_text(color = "black", weight = "bold", size = "30px")) %>%
      tab_style(locations = cells_body(), style = cell_text(size = "20px")) %>%
      tab_options(table.width = "100%", column_labels.hidden = TRUE, table_body.border.bottom.color = "transparent",
                  table.font.size = "20px", data_row.padding = "20px")

  })
  
  output$histogram <- renderPlot({

    data %>%
      ggplot(aes(x = !!sym(input$feature_select))) +
      geom_histogram(color = "black", fill = "plum", alpha = 0.9) +
      theme_fivethirtyeight() +
      theme(axis.title.y = element_text(), plot.title = element_text(size = 30), plot.subtitle = element_text(size = 18),
            plot.caption = element_text(size = 12)) +
      labs(title = paste0("Histogram of ", str_to_title(input$feature_select), " Values"),
           subtitle = "Values from sample of 7,608 songs used to train predictive model",
           y = "Count", caption = "\nSource: Spotify Track Data via Kaggle")

  })
  
  output$main_table <- renderDT({
   
     datatable(data, filter = "top", rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(width = '100px', targets = "_all")),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
    extensions = "Buttons",
    class = 'stripe hover cell-border compact')
    
  })
  
  output$spotify_embed <- renderUI({
    req(input$song)
    
    spotify_id <- testing_data %>% filter(song == input$song) %>% select(spotify_id)
    embed_url <- paste0("https://open.spotify.com/embed/track/", spotify_id, "?utm_source=generator")
    
    HTML(sprintf(
      '<iframe style="border-radius:12px" src="%s" width="100%%" height="250" frameBorder="0" allowfullscreen="" allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture" loading="lazy"></iframe>',
      embed_url
    ))
  })
  
  output$billboard <- render_gt({

    testing_data %>%
      filter(song == input$song) %>%
      select(weeks_on_chart, peak_position, charted_q1, charted_q2, charted_q3, charted_q4) %>%
      mutate(
        q1 = if_else(charted_q1 == TRUE, "✔", "❌"), 
        q2 = if_else(charted_q2 == TRUE, "✔", "❌"),
        q3 = if_else(charted_q3 == TRUE, "✔", "❌"),
        q4 = if_else(charted_q4 == TRUE, "✔", "❌"),
        .keep = "unused"
      ) %>% 
      head(1) %>%
      gt() %>%
      tab_header(title = "Billboard Chart Summary") %>%
      cols_label(
        weeks_on_chart = "Weeks", 
        peak_position = "Peak", 
        q1 = "Q1", q2 = "Q2", q3 = "Q3", q4 = "Q4"
      ) %>%
      gt_color_box(columns = c(weeks_on_chart), domain = c(0, 92), palette = c("lightgrey", "purple")) %>%
      gt_color_box(columns = c(peak_position), domain = c(1, 100), palette = c("purple", "lightgrey")) %>%
      fmt_markdown(columns = q1:q4) %>%
      tab_style(
        style = cell_text(weight = "bold", size = "22px"),
        locations = cells_body()
      ) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_column_labels()
      ) %>%
      tab_style(
        style = cell_text(weight = "bold", size = "24px"),
        locations = cells_title()
      ) %>%
      tab_options(
        table.font.size = "20px",
        table.width = "100%",
        column_labels.font.size = "18px",
        heading.padding = "20px",
        row.striping.include_table_body = FALSE,
        data_row.padding = px(12),
        table.background.color = "#F0F0F0"
      )
    
    
  })
  
  output$donut <- renderPlot({
    
    chance <- predictions %>%
      filter(song == input$song) %>%
      pull(label)
    
    predictions %>%
      filter(song == input$song) %>%
      ggplot(aes(x = 2, y = Grammy_Prob, fill = Grammy_Prob, label = label)) +
      geom_bar(stat = "identity", width = 1) +
      labs(title = paste(chance, "chance")) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_gradientn(colors = c("red2", "yellow2", "green2"),
                           values = c(0, 0.1, 1),
                           limits = c(0, 1)) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      theme_void() +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "#F0F0F0", linewidth = 0),
            plot.background = element_rect(fill = "#F0F0F0", linewidth = 0),
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "pt"),
            panel.spacing = unit(0, "null"))

    
  }, bg = "#F0F0F0")
  
  output$song_features <- renderPlot({
    
    artist <- testing_data %>% filter(song == input$song) %>% pull(artist)
    
    testing_data %>%
    filter(song == input$song) %>%
    select(all_of(col_vars)) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
      mutate(feature = reorder(feature, -value)) %>%
    ggplot(aes(x = feature, y = value)) +
    geom_col(color = "black", fill = "plum", alpha = 0.9) +
    scale_x_discrete(labels = function(x) stringr::str_to_title(x)) +
    theme_fivethirtyeight() +
    theme(axis.title.y = element_text(), plot.title = element_text(size = 24), plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 12)) +
    labs(title = paste("Key Audio Features:", input$song),
         subtitle = paste("By", artist),
         y = "Value", caption = "\nSource: Spotify Track Data via Kaggle")
    
  })
  
}

# run the app ----
shinyApp(ui = ui, server = server)
