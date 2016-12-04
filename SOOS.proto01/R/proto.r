random_id <- function() {
  paste(sample(c(letters,LETTERS, 0:9), 6, replace = TRUE), collapse = "")
}


#' Title
#'
#' @param fname
#'
#' @return
#' @export
#'
#' @examples
init_db <- function(fname) {
  if (!file.exists(fname)) {
    db <- src_sqlite(fname, create = TRUE)
    dummytab <- data_frame(ID = "0", name = "dummy", lead = "dummy",
                           date_start_year = 0L, date_start_month = 1L, date_start_day = 1L,
                           date_end_year = 0L, date_end_month = 12L, date_end_day = 31L,
                           team_size = 0L,
                           llbox = "",
                           created = Sys.time(),
                           show = FALSE)
    tab <- copy_to(db, dummytab, temporary = FALSE )
    db_insert_into( con = db$con, table = "dummytab", values = dummytab)
    #tab <- tbl(db, "d")
  } else (
    db <- src_sqlite(fname, create = FALSE)
  )
  db
}

get_data <- function(dbfile) {
  db <- src_sqlite(dbfile, create = FALSE)
  tab <- tbl(db, "dummytab") %>% filter(show > 0) %>% select(-show) %>% arrange(desc(created)) %>%  distinct() %>% collect()
  rm(db)
  tab$date_start <- ISOdate(tab$date_start_year, tab$date_start_month, tab$date_start_day)
  tab$date_end <- ISOdate(tab$date_end_year, tab$date_end_month, tab$date_end_day)
  tab
}

store_data <- function(innew, dbfile) {
  db <- src_sqlite(dbfile, create = FALSE)
  #dummytab <- data_frame(name = "dummy", date_start = Sys.Date(), date_end = Sys.Date(), team_size = 1L, show = TRUE)
  db_insert_into( con = db$con, table = "dummytab", values = as.data.frame(innew))
  rm(db)
}

cdate <- function(x) {
  as.numeric(x) + as.POSIXct("1970-01-01")
}
fdate <- function(x) format(x, "%Y,%B")
formatLoc <- function(lon, lat) {
  sprintf("%s,%s %s,%s",
          format(lon[1], digits = 5),
          format(lon[2], digits = 5),
          format(lat[1], digits = 4),
          format(lat[2], digits = 4)
  )
}

build_map <- function(voyage, ports) {
  pal <- leaflet::colorFactor(c( "#440154FF", "#FDE725FF"), domain = c("out", "in"))
  leaflet() %>% addTiles() %>%
    addMarkers(~lon_dd, ~lat_dd,
               popup = ports$content, data = ports) %>%
    addPolylines(~LONGITUDE_DEGEAST, ~LATITUDE_DEGNORTH,
                 data = voyage) %>%
    addCircleMarkers(~LONGITUDE_DEGEAST, ~LATITUDE_DEGNORTH, fillOpacity = 0.85, color = ~pal(stage), radius = ~radius, popup = ~date, data = voyage) %>%
    setView(lng = 147, lat = -44, zoom = 4)
}
prepare_ports <- function()  {
  ports <- readr::read_csv(system.file("extdata", "Southern_Ocean_Ports.csv", package = "SOOS.cuboc"))
  ports$content <- sprintf(
    "<b><a href='https://www.google.com.au/search?q=%s'>%s</a></b>",
    gsub("\\s+", "_", ports$Name), ports$Name)
  ports
}

prepare_voyage <- function(){

  voyage$date <- format(voyage$DATE_TIME_UTC)

  voyage$outgoing <- voyage$DATE_TIME_UTC < as.POSIXct("2013-02-04")
  voyage$stage <- c("out", "in")[voyage$outgoing + 1]
  voyage$radius <- ifelse(voyage$outgoing, 6, 3)
  #voyage$colour <- [voyage$outgoing + 1]
  voyage
}

# get_time <- reactive({
#   input$write
#   dd <- get_data()
#
#   timedata <- data.frame(
#     id      = dd$ID,
#     content = dd$name,
#     start   = dd$date_start,
#     end     = dd$date_end
#   )
#   timevis(timedata)
# })

