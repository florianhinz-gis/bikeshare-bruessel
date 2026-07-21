library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)
library(ggplot2)
library(base64enc)
library(sf)

# -----------------------------------------------------------------------------
# Daten laden
# -----------------------------------------------------------------------------
aggregiert <- read_csv("aggregiert_rasterzellen.csv",
                       show_col_types = FALSE) %>%
  mutate(from_id = as.character(from_id))

kennzahlen <- read_csv("aggregiert_kennzahlen.csv",
                       show_col_types = FALSE)

ergebnisse <- readRDS("ergebnisse_gehzeiten.rds")

fahrzeuge <- readRDS("fahrzeuge_karte_xz.rds")
raster_zentroide <- readRDS("raster_zentroide.rds")

# Vorberechnete Geo-Daten laden
anbieter_keys <- list(
  "Villo (cyclocity/JCDecaux)" = "Villo__cyclocity_JCDecaux_",
  "Dott"                       = "Dott",
  "Bolt"                       = "Bolt",
  "Kombiniert"                 = "Kombiniert"
)

geo_cache <- list()
for (anb in names(anbieter_keys)) {
  key <- anbieter_keys[[anb]]
  for (std in c(6, 7, 8, 9)) {
    dateiname <- sprintf("geo_%s_%d.rds", key, std)
    cache_key <- paste0(anb, "_", std)
    if (file.exists(dateiname)) {
      geo_cache[[cache_key]] <- readRDS(dateiname)
    }
  }
}

message("Daten geladen")

# -----------------------------------------------------------------------------
# Hilfsfunktionen
# -----------------------------------------------------------------------------
anbieter_farben <- c(
  "Villo" = "#F5C800",
  "Dott"  = "#00C2E8",
  "Bolt"  = "#34D186"
)

radius_grad <- 400 / 111320

alle_tage    <- sort(unique(as.Date(fahrzeuge$datum)))
tage_labels  <- format(alle_tage, "%d.%m.%Y")
tage_choices <- setNames(as.character(alle_tage), tage_labels)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .irs-grid-text { font-size: 13px; font-weight: bold; }
      .irs-min, .irs-max { display: none; }
      input[value='Villo (cyclocity/JCDecaux)'] + span { color: #F5C800; font-weight: bold; }
      input[value='Dott'] + span { color: #00C2E8; font-weight: bold; }
      input[value='Bolt'] + span { color: #34D186; font-weight: bold; }
      input[value='Kombiniert'] + span { color: #666666; font-weight: bold; }
    "))
  ),
  
  titlePanel("Erreichbarkeit von Mietfahrrädern am Brüsseler Morgen"),
  
  sidebarLayout(
    sidebarPanel(
      
      h5("Anbieter"),
      radioButtons(
        inputId  = "anbieter",
        label    = NULL,
        choices  = c(
          "Villo"      = "Villo (cyclocity/JCDecaux)",
          "Dott"       = "Dott",
          "Bolt"       = "Bolt",
          "Kombiniert" = "Kombiniert"
        ),
        selected = "Kombiniert"
      ),
      
      hr(),
      
      h5("Uhrzeit"),
      sliderInput(
        inputId  = "stunde",
        label    = NULL,
        min = 6, max = 9, value = 7, step = 1,
        ticks = FALSE, sep = "", post = " Uhr"
      ),
      
      hr(),
      
      conditionalPanel(
        condition = "!output.detail_aktiv",
        
        h5("Bevölkerungsanteil mit Zugang zum nächsten Mietrad in...",
           style = "margin-bottom:10px;"),
        tags$table(style = "width:100%; font-size:12px;",
                   tags$tr(
                     tags$td(style = "color:#666; padding-bottom:4px;", "< 1 Gehminute"),
                     tags$td(style = "text-align:right;",
                             h4(textOutput("kennzahl_1min"), style = "color:#2196F3; margin:0;"))
                   ),
                   tags$tr(
                     tags$td(style = "color:#666; padding-bottom:4px;", "< 2 Gehminuten"),
                     tags$td(style = "text-align:right;",
                             h4(textOutput("kennzahl_2min"), style = "color:#2196F3; margin:0;"))
                   ),
                   tags$tr(
                     tags$td(style = "color:#666;", "< 5 Gehminuten"),
                     tags$td(style = "text-align:right;",
                             h4(textOutput("kennzahl_5min"), style = "color:#2196F3; margin:0;"))
                   )
        ),
        
        hr(),
        
        h6("mediane Gehzeit zum nächsten Mietrad"),
        tags$table(style = "font-size:11px; width:100%;",
                   tags$tr(
                     tags$td(style = "background:#1a9641; width:16px; height:12px;"),
                     tags$td(style = "padding-left:6px;", "0–2 Minuten")
                   ),
                   tags$tr(
                     tags$td(style = "background:#a6d96a; width:16px; height:12px;"),
                     tags$td(style = "padding-left:6px;", "2–5 Minuten")
                   ),
                   tags$tr(
                     tags$td(style = "background:#ffffbf; width:16px; height:12px;"),
                     tags$td(style = "padding-left:6px;", "5–10 Minuten")
                   ),
                   tags$tr(
                     tags$td(style = "background:#fdae61; width:16px; height:12px;"),
                     tags$td(style = "padding-left:6px;", "10–15 Minuten")
                   ),
                   tags$tr(
                     tags$td(style = "background:#d7191c; width:16px; height:12px;"),
                     tags$td(style = "padding-left:6px;", "> 15 Minuten")
                   )
        ),
        
        hr(),
        
        p("Auf eine Rasterzelle klicken für Tagesverlauf",
          style = "font-size:11px; color:#2196F3; font-style:italic;"),
        
        hr(),
        
        p("Quelle: GBFS-Feeds Villo, Dott, Bolt | Statbel Bevölkerungsraster | OpenStreetMap-Wegenetz",
          style = "font-size:10px; color:#999;")
      ),
      
      conditionalPanel(
        condition = "output.detail_aktiv",
        
        h5("Tag auswählen"),
        selectInput(
          inputId  = "datum_gewaehlt",
          label    = NULL,
          choices  = tage_choices,
          selected = as.character(max(alle_tage))
        ),
        div(style = "display: flex; gap: 6px;",
            actionButton("play_btn", "▶ Abspielen",
                         class = "btn-default btn-sm", style = "flex: 1;"),
            actionButton("stop_btn", "■ Stop",
                         class = "btn-default btn-sm", style = "flex: 1;")
        ),
        p("Zeigt verfügbare Mieträder in der Nähe",
          style = "font-size:11px; color:#999; margin-top:6px;"),
        p("Die Darstellung basiert auf Luftliniendistanz (400m-Radius). Gehzeiten im Tagesverlauf werden auf dem realen Straßennetz berechnet und können abweichen.",
          style = "font-size:10px; color:#999; margin-top:4px;"),
        actionButton("zurueck", "← Zurück zur Übersicht",
                     class = "btn-sm btn-default",
                     style = "margin-top:6px; width:100%;"),
        
        hr(),
        
        p("Quelle: GBFS-Feeds Villo, Dott, Bolt | Statbel Bevölkerungsraster | OpenStreetMap-Wegenetz",
          style = "font-size:10px; color:#999;")
      )
    ),
    
    mainPanel(
      leafletOutput("karte", height = "85vh")
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  aktive_zelle <- reactiveVal(NULL)
  detail_aktiv <- reactiveVal(FALSE)
  play_aktiv   <- reactiveVal(FALSE)
  play_index   <- reactiveVal(1)
  datum_aktiv  <- reactiveVal(as.character(max(alle_tage)))
  
  output$zelle_aktiv  <- reactive({ !is.null(aktive_zelle()) })
  output$detail_aktiv <- reactive({ detail_aktiv() })
  outputOptions(output, "zelle_aktiv",  suspendWhenHidden = FALSE)
  outputOptions(output, "detail_aktiv", suspendWhenHidden = FALSE)
  
  output$kennzahl_1min <- renderText({
    kz <- kennzahlen %>% filter(anbieter == input$anbieter, stunde == input$stunde)
    if (nrow(kz) == 0) return("–")
    paste0(kz$anteil_1min, "%")
  })
  
  output$kennzahl_2min <- renderText({
    kz <- kennzahlen %>% filter(anbieter == input$anbieter, stunde == input$stunde)
    if (nrow(kz) == 0) return("–")
    paste0(kz$anteil_2min, "%")
  })
  
  output$kennzahl_5min <- renderText({
    kz <- kennzahlen %>% filter(anbieter == input$anbieter, stunde == input$stunde)
    if (nrow(kz) == 0) return("–")
    paste0(kz$anteil_5min, "%")
  })
  
  output$karte <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.3517, lat = 50.8503, zoom = 12)
  })
  
  karten_daten <- reactive({
    cache_key <- paste0(input$anbieter, "_", input$stunde)
    geo_cache[[cache_key]]
  })
  
  observe({
    if (!is.null(aktive_zelle())) return()
    daten <- karten_daten()
    if (is.null(daten)) return()
    
    leafletProxy("karte") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      clearPopups() %>%
      addPolygons(
        data        = daten,
        fillColor   = ~farbe,
        fillOpacity = 0.75,
        color       = ~farbe,
        weight      = 0.3,
        layerId     = ~from_id,
        label       = ~tooltip
      )
  })
  
  zeige_fahrzeuge <- function(tag) {
    zelle_id <- aktive_zelle()
    if (is.null(zelle_id)) return()
    
    zentroid <- raster_zentroide %>% filter(from_id == zelle_id)
    if (nrow(zentroid) == 0) return()
    
    lat_z <- zentroid$lat_z
    lon_z <- zentroid$lon_z
    
    anbieter_filter <- input$anbieter
    nahes <- fahrzeuge %>%
      filter(datum == tag, stunde == input$stunde) %>%
      mutate(dist = sqrt((lon - lon_z)^2 + (lat - lat_z)^2)) %>%
      filter(dist <= radius_grad) %>%
      filter(
        anbieter_filter == "Kombiniert" |
          (anbieter_filter == "Villo (cyclocity/JCDecaux)" & anbieter == "Villo") |
          (anbieter_filter == "Dott"  & anbieter == "Dott") |
          (anbieter_filter == "Bolt"  & anbieter == "Bolt")
      )
    
    daten     <- karten_daten()
    zelle_geo <- daten[daten$from_id == zelle_id, ]
    
    proxy <- leafletProxy("karte") %>%
      clearMarkers() %>%
      clearControls()
    
    if (length(zelle_geo) > 0) {
      proxy <- proxy %>%
        addPolygons(
          data        = zelle_geo,
          fillColor   = "#2196F3",
          fillOpacity = 0.3,
          color       = "#2196F3",
          weight      = 2,
          layerId     = "aktive_zelle"
        )
    }
    
    proxy <- proxy %>%
      addCircles(
        lng       = lon_z,
        lat       = lat_z,
        radius    = 400,
        color     = "#2196F3",
        weight    = 1.5,
        opacity   = 0.8,
        fill      = FALSE,
        dashArray = "6, 6",
        layerId   = "radius_kreis"
      )
    
    if (nrow(nahes) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data        = nahes,
          lng         = ~lon,
          lat         = ~lat,
          radius      = 5,
          color       = ~case_when(
            anbieter == "Villo" ~ "#F5C800",
            anbieter == "Dott"  ~ "#00C2E8",
            TRUE                ~ "#34D186"
          ),
          fillColor   = ~case_when(
            anbieter == "Villo" ~ "#F5C800",
            anbieter == "Dott"  ~ "#00C2E8",
            TRUE                ~ "#34D186"
          ),
          fillOpacity = 0.8,
          weight      = 1,
          label       = ~anbieter
        )
    }
    
    proxy %>%
      addControl(
        html = paste0(
          "<b>", format(as.Date(tag), "%d.%m.%Y"),
          " | ", input$stunde, ":00 Uhr</b><br>",
          nrow(nahes), " Mieträder im 400m-Radius"
        ),
        position = "topright"
      )
  }
  
  observeEvent(input$karte_shape_click, {
    klick        <- input$karte_shape_click
    geklickte_id <- klick$id
    if (is.null(geklickte_id)) return()
    
    aktive_zelle(geklickte_id)
    
    zelle_daten <- ergebnisse %>%
      filter(from_id == geklickte_id, stunde == input$stunde) %>%
      mutate(
        datum    = as.Date(datum),
        anbieter = recode(anbieter,
                          "Villo (cyclocity/JCDecaux)" = "Villo"),
        anbieter = factor(anbieter, levels = c("Villo", "Dott", "Bolt"))
      )
    
    tmp         <- tempfile(fileext = ".png")
    villo_fehlt <- !"Villo" %in% zelle_daten$anbieter
    
    farben_plot <- if (villo_fehlt) {
      c("Villo: > 15 Min" = "#F5C800", "Dott" = "#00C2E8", "Bolt" = "#34D186")
    } else {
      anbieter_farben
    }
    
    if (villo_fehlt) {
      dummy <- tibble(datum = as.Date(NA), min_gehzeit = NA_real_,
                      anbieter = factor("Villo: > 15 Min"))
      zelle_daten <- bind_rows(zelle_daten, dummy)
    }
    
    p <- ggplot(
      zelle_daten %>% filter(!is.na(min_gehzeit)),
      aes(x = datum, y = min_gehzeit, color = anbieter)
    ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      scale_color_manual(values = farben_plot, breaks = names(farben_plot)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = paste0("Gehzeit zum nächsten Mietrad um ",
                       input$stunde, ":00 Uhr"),
        x = NULL, y = "Gehzeit (Min)", color = NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position  = "bottom",
        plot.title       = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    ggsave(tmp, plot = p, width = 3.5, height = 2.2, dpi = 150)
    img_base64 <- base64encode(tmp)
    
    popup_html <- paste0(
      '<div style="text-align:center;">',
      '<img src="data:image/png;base64,', img_base64,
      '" width="320" height="200"/><br>',
      '<button onclick="Shiny.setInputValue(\'tagesansicht_btn\', Math.random())" ',
      'style="margin-top:8px; padding:5px 14px; background:white; color:#555; ',
      'border:1px solid #ccc; border-radius:4px; cursor:pointer; font-size:12px;">',
      'Tagesansicht öffnen</button>',
      '<p style="font-size:11px; color:#999; margin:4px 0 0 0;">',
      'Zeigt verfügbare Mieträder in der Nähe</p>',
      '</div>'
    )
    
    leafletProxy("karte") %>%
      clearPopups() %>%
      addPopups(
        lng     = klick$lng,
        lat     = klick$lat,
        popup   = popup_html,
        options = popupOptions(maxWidth = 360)
      )
  })
  
  observeEvent(input$tagesansicht_btn, {
    zelle_id <- aktive_zelle()
    if (is.null(zelle_id)) return()
    
    zentroid <- raster_zentroide %>% filter(from_id == zelle_id)
    if (nrow(zentroid) == 0) return()
    
    detail_aktiv(TRUE)
    
    leafletProxy("karte") %>%
      clearShapes() %>%
      clearPopups() %>%
      setView(lng = zentroid$lon_z, lat = zentroid$lat_z, zoom = 16)
    
    zeige_fahrzeuge(datum_aktiv())
  })
  
  observeEvent(input$datum_gewaehlt, {
    if (!detail_aktiv()) return()
    datum_aktiv(input$datum_gewaehlt)
    zeige_fahrzeuge(input$datum_gewaehlt)
  }, ignoreInit = TRUE)
  
  observeEvent(input$stunde, {
    if (!detail_aktiv()) return()
    zeige_fahrzeuge(datum_aktiv())
  }, ignoreInit = TRUE)
  
  observeEvent(input$anbieter, {
    if (!detail_aktiv()) return()
    zeige_fahrzeuge(datum_aktiv())
  }, ignoreInit = TRUE)
  
  observeEvent(input$play_btn, {
    play_index(1)
    play_aktiv(TRUE)
  })
  
  observeEvent(input$stop_btn, {
    play_aktiv(FALSE)
  })
  
  observe({
    if (!play_aktiv()) return()
    invalidateLater(1000, session)
    
    idx <- isolate(play_index())
    if (idx > length(alle_tage)) {
      play_aktiv(FALSE)
      return()
    }
    
    tag <- as.character(alle_tage[idx])
    datum_aktiv(tag)
    updateSelectInput(session, "datum_gewaehlt", selected = tag)
    zeige_fahrzeuge(tag)
    play_index(idx + 1)
  })
  
  observeEvent(input$zurueck, {
    aktive_zelle(NULL)
    detail_aktiv(FALSE)
    play_aktiv(FALSE)
    leafletProxy("karte") %>%
      clearControls() %>%
      clearPopups() %>%
      setView(lng = 4.3517, lat = 50.8503, zoom = 12)
  })
}

shinyApp(ui = ui, server = server)