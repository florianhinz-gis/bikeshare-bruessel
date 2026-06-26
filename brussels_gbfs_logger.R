# =============================================================================
# Brussels Multi-Provider GBFS Logger
# Erfasst die verfuegbaren Fahrzeuge (Raeder/Scooter) fuer ALLE bekannten
# Bikeshare-/Sharing-Anbieter in Bruessel mit oeffentlichem GBFS-Feed:
#   - Villo (cyclocity/JCDecaux)   -> stationsbasiert
#   - Dott Brussels                -> free-floating (keine Stationen)
#   - Bolt Brussels                -> free-floating (keine Stationen)
#
# Hinweis: Lime und Voi bieten in Bruessel KEINEN oeffentlich zugaenglichen
# GBFS-Feed an (nur auf Anfrage/Lizenz ueber transportdata.be). Sie sind
# deshalb hier nicht enthalten. Seit Februar 2024 sind ohnehin nur Bolt und
# Dott als Free-Floating-Anbieter in Bruessel zugelassen.
#
# Gedacht fuer wiederholten Aufruf (z.B. alle 5-15 Minuten via GitHub Actions).
# =============================================================================

required_packages <- c("jsonlite", "dplyr", "purrr", "httr", "tibble")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(jsonlite)
library(dplyr)
library(purrr)
library(httr)
library(tibble)

# -----------------------------------------------------------------------------
# 0. Zeitfenster-Check: nur zwischen 06:00 und 10:00 Uhr Bruesseler Zeit loggen
#    Der Workflow wird extern alle 15 Minuten rund um die Uhr von cron-job.org
#    ausgeloest (workflow_dispatch hat selbst keinen Zeitplan). Damit trotzdem
#    nur in der gewuenschten Zeitspanne tatsaechlich geloggt wird, prueft das
#    Skript bei jedem Aufruf zuerst die aktuelle Uhrzeit in Bruessel (CET/CEST,
#    inkl. automatischer Sommerzeit-Umstellung) und beendet sich sofort, falls
#    wir uns ausserhalb von 06:00-10:00 Uhr befinden.
# -----------------------------------------------------------------------------
bruessel_zeit <- Sys.time()
attr(bruessel_zeit, "tzone") <- "Europe/Brussels"
stunde_bruessel <- as.integer(format(bruessel_zeit, "%H"))

if (stunde_bruessel < 6 || stunde_bruessel >= 10) {
  message(sprintf(
    "Ausserhalb des Logging-Fensters (06:00-10:00 Bruesseler Zeit). Aktuelle Zeit: %s. Beende ohne Aktion.",
    format(bruessel_zeit, "%Y-%m-%d %H:%M:%S %Z")
  ))
  quit(save = "no", status = 0)
}

message(sprintf("Innerhalb des Logging-Fensters - aktuelle Bruesseler Zeit: %s",
                 format(bruessel_zeit, "%Y-%m-%d %H:%M:%S %Z")))

client_id <- "bikeshare-research-script-mac"

gbfs_get <- function(url) {
  res <- GET(
    url,
    add_headers(
      `Client-Identifier` = client_id,
      `User-Agent` = "Mozilla/5.0 (Bikeshare-Research-Script)"
    )
  )
  fromJSON(content(res, as = "text", encoding = "UTF-8"))
}

# -----------------------------------------------------------------------------
# 1. Konfiguration: alle Systeme in Bruessel mit oeffentlichem GBFS-Feed
#    typ = "station"       -> hat station_information/station_status
#    typ = "free_floating" -> hat nur vehicle_status / free_bike_status
# -----------------------------------------------------------------------------

systeme <- tibble::tibble(
  land = c("BE", "BE", "BE"),
  stadt = c("Bruessel", "Bruessel", "Bruessel"),
  anbieter = c("Villo (cyclocity/JCDecaux)", "Dott", "Bolt"),
  typ = c("station", "free_floating", "free_floating"),
  gbfs_url = c(
    "https://api.cyclocity.fr/contracts/bruxelles/gbfs/v3/gbfs.json",
    "https://gbfs.api.ridedott.com/public/v2/brussels/gbfs.json",
    "https://mds.bolt.eu/gbfs/3/336/gbfs"
  )
)

# -----------------------------------------------------------------------------
# 2. Hilfsfunktion: aus der Auto-Discovery-Datei die richtigen Feed-URLs holen
# -----------------------------------------------------------------------------
get_feed_urls <- function(gbfs_url) {
  disc <- gbfs_get(gbfs_url)

  feeds_df <- NULL
  if (!is.null(disc$data$feeds)) {
    feeds_df <- disc$data$feeds
  } else {
    erste_sprache <- disc$data[[1]]
    feeds_df <- erste_sprache$feeds
  }

  hole_url <- function(name) {
    treffer <- feeds_df$url[feeds_df$name == name]
    if (length(treffer) == 0) NA_character_ else treffer[1]
  }

  list(
    info_url        = hole_url("station_information"),
    status_url      = hole_url("station_status"),
    vehicle_url     = hole_url("vehicle_status"),
    free_bike_url   = hole_url("free_bike_status")
  )
}

# Hilfsfunktion: mehrsprachige GBFS-v3-Textfelder ({language, text}-Paare)
# auf einen einzelnen String normalisieren.
normalisiere_text <- function(x) {
  vapply(x, function(e) {
    if (is.data.frame(e) && "text" %in% names(e)) {
      if (nrow(e) == 0) return(NA_character_)
      as.character(e$text[1])
    } else if (is.list(e) && !is.null(e$text)) {
      as.character(e$text[1])
    } else if (is.list(e)) {
      werte <- unlist(e)
      if (length(werte) == 0) return(NA_character_)
      as.character(werte[1])
    } else {
      as.character(e)
    }
  }, character(1))
}

# -----------------------------------------------------------------------------
# 3a. Stationsbasiertes System abfragen (z.B. Villo)
# -----------------------------------------------------------------------------
log_station_system <- function(land, stadt, anbieter, urls, zeitstempel) {
  info   <- gbfs_get(urls$info_url)$data$stations
  status <- gbfs_get(urls$status_url)$data$stations

  info_df <- info %>%
    select(station_id, name, lat, lon, any_of("capacity")) %>%
    mutate(name = normalisiere_text(name))

  status_df <- status %>%
    select(station_id, any_of(c("num_bikes_available", "num_vehicles_available")), any_of("num_docks_available"))

  if ("num_vehicles_available" %in% names(status_df) && !("num_bikes_available" %in% names(status_df))) {
    status_df <- status_df %>% rename(num_bikes_available = num_vehicles_available)
  }
  status_df <- status_df %>% select(-any_of("num_vehicles_available"))

  info_df %>%
    inner_join(status_df, by = "station_id") %>%
    mutate(land = land, stadt = stadt, anbieter = anbieter, timestamp = zeitstempel)
}

# -----------------------------------------------------------------------------
# 3b. Free-Floating-System abfragen (z.B. Dott, Bolt)
#     Es gibt keine Stationen - wir loggen jedes verfuegbare Fahrzeug als
#     eigene "Station" mit seiner aktuellen Position, damit das Datenformat
#     mit dem stationsbasierten Fall vergleichbar bleibt (eine Zeile pro
#     verfuegbarem Fahrzeug/Standort und Zeitpunkt).
# -----------------------------------------------------------------------------
log_free_floating_system <- function(land, stadt, anbieter, urls, zeitstempel) {
  vehicle_url <- if (!is.na(urls$vehicle_url)) urls$vehicle_url else urls$free_bike_url
  if (is.na(vehicle_url)) stop("Kein vehicle_status/free_bike_status-Feed gefunden")

  raw <- gbfs_get(vehicle_url)
  fahrzeuge <- raw$data$vehicles
  if (is.null(fahrzeuge)) fahrzeuge <- raw$data$bikes  # GBFS v2-Name als Fallback

  if (is.null(fahrzeuge) || nrow(fahrzeuge) == 0) {
    return(tibble(
      station_id = NA_character_, name = anbieter, lat = NA_real_, lon = NA_real_,
      capacity = NA_real_, num_bikes_available = 0L,
      land = land, stadt = stadt, anbieter = anbieter, timestamp = zeitstempel
    ))
  }

  id_spalte <- if ("vehicle_id" %in% names(fahrzeuge)) "vehicle_id" else "bike_id"

  fahrzeuge %>%
    transmute(
      station_id = .data[[id_spalte]],
      name = anbieter,
      lat = lat,
      lon = lon,
      capacity = NA_real_,
      num_bikes_available = 1L
    ) %>%
    mutate(land = land, stadt = stadt, anbieter = anbieter, timestamp = zeitstempel)
}

# -----------------------------------------------------------------------------
# 4. Dispatcher: ein einzelnes System abfragen (je nach Typ)
# -----------------------------------------------------------------------------
log_system <- function(land, stadt, anbieter, typ, gbfs_url, zeitstempel) {
  tryCatch({
    urls <- get_feed_urls(gbfs_url)

    ergebnis <- if (typ == "station") {
      log_station_system(land, stadt, anbieter, urls, zeitstempel)
    } else {
      log_free_floating_system(land, stadt, anbieter, urls, zeitstempel)
    }

    message(sprintf("OK: %s (%s) - %d Zeilen", stadt, anbieter, nrow(ergebnis)))
    return(ergebnis)

  }, error = function(e) {
    message(sprintf("FEHLER bei %s (%s): %s", stadt, anbieter, conditionMessage(e)))
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# 5. Alle Systeme durchlaufen und Ergebnisse sammeln
# -----------------------------------------------------------------------------
zeitstempel <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

alle_ergebnisse <- pmap(
  list(systeme$land, systeme$stadt, systeme$anbieter, systeme$typ, systeme$gbfs_url),
  function(land, stadt, anbieter, typ, gbfs_url) {
    log_system(land, stadt, anbieter, typ, gbfs_url, zeitstempel)
  }
)

gesamt_df <- bind_rows(alle_ergebnisse)

# -----------------------------------------------------------------------------
# 6. An CSV-Datei anhaengen (Header nur beim ersten Mal, taegliche Aufteilung).
#    Es wird eine neue Datei pro Tag (UTC) erzeugt, z.B.
#    "bikeshare_log_bruessel_2026-06-26.csv".
# -----------------------------------------------------------------------------
tagesstempel <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
ausgabe_datei <- sprintf("bikeshare_log_bruessel_%s.csv", tagesstempel)

if (nrow(gesamt_df) > 0) {
  write.table(
    gesamt_df,
    file = ausgabe_datei,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(ausgabe_datei),
    append = file.exists(ausgabe_datei),
    qmethod = "double"
  )
  message(sprintf("\n%d Zeilen geschrieben nach %s (Stand: %s UTC)",
                   nrow(gesamt_df), ausgabe_datei, zeitstempel))
} else {
  message("Keine Daten erhalten - nichts geschrieben.")
}

# -----------------------------------------------------------------------------
# 7. Test-/Plausibilitaets-Zusammenfassung
# -----------------------------------------------------------------------------
zusammenfassung <- systeme %>%
  select(land, stadt, anbieter) %>%
  left_join(
    gesamt_df %>%
      group_by(land, stadt, anbieter) %>%
      summarise(
        zeilen = n(),
        fahrzeuge_gesamt = sum(num_bikes_available, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("land", "stadt", "anbieter")
  ) %>%
  mutate(
    status = ifelse(is.na(zeilen), "FEHLER", "OK"),
    zeilen = ifelse(is.na(zeilen), 0, zeilen),
    fahrzeuge_gesamt = ifelse(is.na(fahrzeuge_gesamt), 0, fahrzeuge_gesamt)
  )

message("\n===================== ZUSAMMENFASSUNG (BRUESSEL) =====================")
print(as.data.frame(zusammenfassung), row.names = FALSE)
message("========================================================================")
message(sprintf(
  "Erfolgreich: %d von %d Anbietern | Fahrzeuge gesamt: %d",
  sum(zusammenfassung$status == "OK"),
  nrow(zusammenfassung),
  sum(zusammenfassung$fahrzeuge_gesamt)
))
