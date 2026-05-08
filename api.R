# api.R
library(plumber)
library(DBI)
library(pool)
library(RPostgres)
library(httr)
library(jsonlite)
library(tibble)

# ==================================================================
# 1. SETUP & DATABASE POOL (Runs once when API starts)
# ==================================================================
# Helper to prevent blank strings from breaking the connection
get_env <- function(name, default) {
  val <- Sys.getenv(name)
  if (val == "") return(default)
  return(val)
}

DB_HOST        <- get_env("DB_HOST", "")
DB_PORT        <- get_env("DB_PORT", "26257")
DB_NAME        <- get_env("DB_NAME", "defaultdb")
DB_USER        <- get_env("DB_USER", "")
DB_PASSWORD    <- get_env("DB_PASSWORD", "")

# FORCE the system's root certificates to be used
DB_SSLMODE     <- get_env("DB_SSLMODE", "require")
DB_SSLROOTCERT <- get_env("DB_SSLROOTCERT", "system")

db_pool <- pool::dbPool(
  drv         = RPostgres::Postgres(),
  host        = DB_HOST,
  port        = as.integer(DB_PORT),
  dbname      = DB_NAME,
  user        = DB_USER,
  password    = DB_PASSWORD,
  sslmode     = DB_SSLMODE,
  sslrootcert = DB_SSLROOTCERT
)

# Pl@ntNet API Key
KEY_FILE <- file.path("data", "plantnet_key.txt")
PLANTNET_KEY <- if (file.exists(KEY_FILE)) trimws(readr::read_file(KEY_FILE)) else get_env("PLANTNET_KEY", "")

# ==================================================================
# 2. API ENDPOINTS
# ==================================================================

#* @apiTitle Sentinel Garden API
#* @apiDescription Backend for the offline-capable mobile app

#* Check if the API is running
#* @get /health
function() {
  list(status = "online", time = Sys.time())
}

#* Upload an image and identify it using PlantNet
#* @param req The request object
#* @post /identify
#* @parser multi
function(req, res) {
  # Extract the uploaded image from the request
  img_upload <- req$body$image
  
  if (is.null(img_upload)) {
    res$status <- 400
    return(list(error = "No image file provided in the request."))
  }
  
  img_path <- img_upload$datapath
  
  # Call PlantNet (Using your exact logic)
  pn_res <- httr::POST(
    url   = "https://my-api.plantnet.org/v2/identify/all",
    query = list(`api-key` = PLANTNET_KEY, `include-related-images` = "true", lang = "en"),
    body  = list(images = httr::upload_file(img_path)),
    encode = "multipart",
    httr::timeout(45)
  )
  
  if (httr::status_code(pn_res) != 200) {
    res$status <- 500
    return(list(error = "PlantNet API failed to process the image."))
  }
  
  # Parse the JSON response
  parsed_res <- jsonlite::fromJSON(httr::content(pn_res, "text", encoding = "UTF-8"))
  results <- parsed_res$results
  
  if (is.null(results) || nrow(results) == 0) {
    return(list(matches = list())) # Return empty list if no matches
  }
  
  # Format top 5 results for the mobile app
  top_5 <- head(results, 5)
  matches <- lapply(seq_len(nrow(top_5)), function(i) {
    list(
      score = top_5$score[i],
      scientific_name = top_5$species$scientificNameWithoutAuthor[i],
      common_names = if(length(top_5$species$commonNames[[i]]) > 0) paste(top_5$species$commonNames[[i]], collapse=", ") else "-",
      family = top_5$species$family$scientificNameWithoutAuthor[i]
    )
  })
  
  return(list(matches = matches))
}


#* Submit a new garden survey (Built for Offline Syncing)
#* @param req The JSON request containing the survey data
#* @post /submit-survey
#* @parser json
function(req, res) {
  # The mobile app will send JSON when the phone gets signal
  data <- req$body
  
  # Create a clean row for PostgreSQL
  new_record <- data.frame(
    timestamp             = Sys.time(),
    observer_name         = NA_character_,
    include_name          = NA_character_,
    observer_email        = ifelse(is.null(data$email), NA_character_, data$email),
    site_name             = ifelse(is.null(data$site_name), "Unknown", data$site_name),
    grid_cell             = NA_character_,
    latitude              = ifelse(is.null(data$lat), NA_real_, as.numeric(data$lat)),
    longitude             = ifelse(is.null(data$lon), NA_real_, as.numeric(data$lon)),
    species_name          = ifelse(is.null(data$species_name), NA_character_, data$species_name),
    spread_beyond         = ifelse(is.null(data$spread_beyond), NA_character_, data$spread_beyond),
    spread_mode           = ifelse(is.null(data$spread_mode), "", data$spread_mode),
    control_effectiveness = ifelse(is.null(data$control_effectiveness), NA_character_, data$control_effectiveness),
    control_methods       = ifelse(is.null(data$control_methods), "", data$control_methods),
    disposal_methods      = ifelse(is.null(data$disposal_methods), "", data$disposal_methods),
    introduction_routes   = ifelse(is.null(data$introduction_routes), "", data$introduction_routes),
    source_of_plant       = ifelse(is.null(data$source_of_plant), "", data$source_of_plant),
    outside_garden        = ifelse(is.null(data$outside_garden), NA_character_, data$outside_garden),
    warning_label         = ifelse(is.null(data$warning_label), NA_character_, data$warning_label),
    outcompeted           = ifelse(is.null(data$outcompeted), NA_character_, data$outcompeted),
    coverage_dafor        = ifelse(is.null(data$coverage_dafor), NA_character_, data$coverage_dafor),
    notes                 = ifelse(is.null(data$notes), "", data$notes),
    inat_id               = NA_character_,
    stringsAsFactors      = FALSE
  )
  
  # Save to Database
  tryCatch({
    DBI::dbAppendTable(conn = db_pool, name = "observations", value = new_record)
    return(list(status = "success", message = "Survey saved successfully!"))
  }, error = function(e) {
    res$status <- 500
    return(list(status = "error", message = conditionMessage(e)))
  })
}