# Global Fishing Watch Data Access and Plotting Script

## the icesVMS tools only works for 0.05 degree c-squares just now - this is a more flexible version
wkt_csquare_flexible <- function(lat, lon, size = 0.05) {
  half_size <- size / 2
  glue::glue("POLYGON(({lon - half_size} {lat - half_size},", 
       "{lon - half_size} {lat + half_size},", 
       "{lon + half_size} {lat + half_size},", 
       "{lon + half_size} {lat - half_size},", 
       "{lon - half_size} {lat - half_size}))")
}


# Load required libraries
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(icesVMS)

# Try to load the official gfwr package
if (!require(gfwr, quietly = TRUE)) {
  cat("Installing gfwr package...\n")
  # Install gfwr if not available
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
    library(devtools)
  }
  devtools::install_github("GlobalFishingWatch/gfwr")
  library(gfwr)
}

# Set up API parameters
# Note: You'll need to register for a GFW API key at https://globalfishingwatch.org/our-apis/
# then save your api token to a text file, and then point to it like below
api_key <- readLines("C:/Work/Spatial_Data/GFW/api_token.txt")
base_url <- "https://gateway.api.globalfishingwatch.org"

# Define spatial and temporal parameters
min_lat <- 58.0
max_lat <- 59.0
min_lon <- -6.5
max_lon <- -5.0
start_date <- "2023-07-01"
end_date <- "2023-07-31"


# Function to query GFW API for vessel tracks and events
get_gfw_data <- function(api_key, min_lat, max_lat, min_lon, max_lon, start_date, end_date) {
  
  cat("Trying to get vessel events data using Events API...\n")
  
  # Method 1: Try Events API for fishing events - fix the limit/offset issue
  events_url <- "https://gateway.api.globalfishingwatch.org/v3/events"
  
  # Prepare parameters for events API - add required offset parameter
  params <- list(
    "datasets[0]" = "public-global-fishing-events:latest",
    "start-date" = start_date,
    "end-date" = end_date,
    "limit" = "100",      # Reduced limit
    "offset" = "0",       # Required when using limit
    "format" = "json"
  )
  
  # Add spatial filter as bbox
  bbox_param <- paste(min_lon, min_lat, max_lon, max_lat, sep = ",")
  params[["bbox"]] <- bbox_param
  
  cat("Events API URL:", events_url, "\n")
  cat("Parameters:", paste(names(params), params, sep = "=", collapse = "&"), "\n")
  
  response <- GET(
    url = events_url, 
    query = params,
    add_headers(Authorization = paste("Bearer", api_key))
  )
  
  cat("Events API response status:", status_code(response), "\n")
  
  if (status_code(response) == 200) {
    events_data <- fromJSON(content(response, "text"))
    cat("Successfully retrieved events data\n")
    return(events_data)
  } else {
    cat("Events API error:", content(response, "text"), "\n")
    
    # Method 2: Try Vessels API - add required dataset parameter
    cat("Trying Vessels API...\n")
    
    vessels_url <- "https://gateway.api.globalfishingwatch.org/v3/vessels/search"
    
    vessels_params <- list(
      "datasets[0]" = "public-global-vessel-identity:latest",  # Add required dataset
      "where" = paste0("lat >= ", min_lat, " AND lat <= ", max_lat, 
                       " AND lon >= ", min_lon, " AND lon <= ", max_lon),
      "limit" = "100",
      "offset" = "0",      # Add offset for consistency
      "format" = "json"
    )
    
    vessels_response <- GET(
      url = vessels_url,
      query = vessels_params,
      add_headers(Authorization = paste("Bearer", api_key))
    )
    
    cat("Vessels API response status:", status_code(vessels_response), "\n")
    
    if (status_code(vessels_response) == 200) {
      vessels_data <- fromJSON(content(vessels_response, "text"))
      cat("Successfully retrieved vessels data\n")
      return(vessels_data)
    } else {
      cat("Vessels API error:", content(vessels_response, "text"), "\n")
      
      # Method 3: Use the working gfwr approach but for different data
      cat("Trying to get fishing events using gfwr package...\n")
      
      if (exists("get_event", where = "package:gfwr")) {
        tryCatch({
          # Create spatial region for events
          events_region_sf <- sf::st_polygon(list(matrix(c(
            min_lon, min_lat,
            max_lon, min_lat, 
            max_lon, max_lat,
            min_lon, max_lat,
            min_lon, min_lat
          ), ncol = 2, byrow = TRUE))) %>%
            sf::st_sfc(crs = 4326) %>%
            sf::st_sf()
          
          # Get fishing events using gfwr
          events_data <- get_event(
            event_type = "FISHING",
            start_date = start_date,
            end_date = end_date,
            region = events_region_sf,
            region_source = "USER_SHAPEFILE",
            key = api_key
          )
          
          cat("Successfully retrieved events using gfwr package\n")
          return(events_data)
          
        }, error = function(e) {
          cat("gfwr events method failed:", e$message, "\n")
          stop("All API methods failed")
        })
      } else {
        stop("All API methods failed and gfwr package not available")
      }
    }
  }
}

track_data <- get_gfw_data(api_key, min_lat, max_lat, min_lon, max_lon, start_date, end_date)


# This function uses the fishing effort gridded data which is easier to work with
get_gfw_fishing_effort <- function(api_key, min_lat, max_lat, min_lon, max_lon, start_date, end_date) {
  
  # Method 1: 4wings API with corrected dataset version
  effort_url <- "https://gateway.api.globalfishingwatch.org/v1/4wings/report"
  
  # Prepare parameters with correct dataset version
  params <- list(
    "datasets[0]" = "public-global-fishing-effort:v3",  # Fixed: removed :latest, use :v3
    "date-range" = paste0(start_date, ",", end_date),
    "region" = paste(min_lon, min_lat, max_lon, max_lat, sep = ","),
    "spatial-resolution" = "low",
    "temporal-resolution" = "daily",
    "format" = "json"
  )
  
  cat("Trying 4wings API with corrected dataset version\n")
  cat("URL:", effort_url, "\n")
  cat("Parameters:", paste(names(params), params, sep = "=", collapse = "&"), "\n")
  
  response <- GET(
    url = effort_url, 
    query = params,
    add_headers(Authorization = paste("Bearer", api_key))
  )
  
  cat("Response status:", status_code(response), "\n")
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    return(data)
  } else {
    # Print error details for debugging
    error_content <- content(response, "text")
    cat("Error response content:", error_content, "\n")
    
    # Try Method 2: Use the datasets API to find available datasets first
    cat("Trying to list available datasets...\n")
    
    datasets_url <- "https://gateway.api.globalfishingwatch.org/v1/datasets"
    
    datasets_response <- GET(
      url = datasets_url,
      add_headers(Authorization = paste("Bearer", api_key))
    )
    
    cat("Datasets API status:", status_code(datasets_response), "\n")
    
    if (status_code(datasets_response) == 200) {
      datasets_data <- fromJSON(content(datasets_response, "text"))
      cat("Available datasets with 'fishing' in name:\n")
      if("entries" %in% names(datasets_data)) {
        fishing_datasets <- datasets_data$entries[grepl("fishing", datasets_data$entries$id, ignore.case = TRUE), ]
        print(fishing_datasets[, c("id", "name", "version")])
      }
    }
    
    warning(paste("API request failed with status:", status_code(response)))
    return(NULL)
  }
}

# Get base map data for context
world <- ne_countries(scale = 10, returnclass = "sf")
coastline <- ne_coastline(scale = 10, returnclass = "sf")

# Create the study area polygon
study_area <- data.frame(
  lon = c(min_lon, max_lon, max_lon, min_lon, min_lon),
  lat = c(min_lat, min_lat, max_lat, max_lat, min_lat)
)

# Try to get GFW data using the official gfwr package
cat("Attempting to retrieve Global Fishing Watch data...\n")

  # Create study area as sf polygon for gfwr
  study_region_sf <- sf::st_polygon(list(matrix(c(
    min_lon, min_lat,
    max_lon, min_lat, 
    max_lon, max_lat,
    min_lon, max_lat,
    min_lon, min_lat
  ), ncol = 2, byrow = TRUE))) %>%
    sf::st_sfc(crs = 4326) %>%
    sf::st_sf()
  
  # Try to get fishing effort data using gfwr
    cat("Requesting fishing effort data using gfwr...\n")

    gfw_data <- get_raster(
      spatial_resolution = "HIGH",
      temporal_resolution = "MONTHLY",  # Use monthly instead of daily
      start_date = start_date,
      end_date = end_date,          # End date excludes this date, so this is just January
      region = study_region_sf,
      region_source = "USER_SHAPEFILE",
      key = api_key,                    # Explicitly pass the API key instead of relying on gfw_auth()
      print_request = TRUE              # Enable debugging to see the actual request
    )
    
    if (!is.null(gfw_data) && nrow(gfw_data) > 0) {
      cat("Successfully retrieved", nrow(gfw_data), "records using gfwr\n")
    }
      # Examine the structure of gfwr data
      cat("GFW data structure:\n")
      print(str(gfw_data))
      cat("First few rows:\n")
      print(head(gfw_data))
      
      # Convert to our standard format based on typical gfwr output
      # Common columns in gfwr: Lat, Lon, ApparentFishingHrs, Date
      fishing_points <- gfw_data
      
      # Standardise column names if needed
      if ("Lat" %in% names(fishing_points)) {
        names(fishing_points)[names(fishing_points) == "Lat"] <- "lat"
      }
      if ("Lon" %in% names(fishing_points)) {
        names(fishing_points)[names(fishing_points) == "Lon"] <- "lon"
      }
      if ("ApparentFishingHrs" %in% names(fishing_points)) {
        names(fishing_points)[names(fishing_points) == "ApparentFishingHrs"] <- "effort"
      }
      if ("Date" %in% names(fishing_points)) {
        names(fishing_points)[names(fishing_points) == "Date"] <- "date"
      }
    

fishing_points$csquare <- vmstools::CSquare(fishing_points$lon, fishing_points$lat, 0.01)
fishing_points$geometry <- wkt_csquare_flexible(fishing_points$lat, fishing_points$lon, 0.01)

fishing_points_sf <- st_as_sf(fishing_points, wkt = "geometry", crs = 4326) %>%
  rename(time_range = `Time Range`, vessel_ids = `Vessel IDs`,
         effort = `Apparent Fishing Hours`)

# Create the main plot
p1 <- ggplot() +
  # Add world map
  geom_sf(data = world, fill = "lightgrey", colour = "white", size = 0.2) +
  geom_sf(data = coastline, colour = "darkgrey", size = 0.3) +
  
  # Add study area boundary
  geom_polygon(data = study_area, aes(x = lon, y = lat), 
               fill = NA, colour = "red", size = 1, linetype = "dashed") +
  
  # Add fishing activity polygons coloured by effort
  geom_sf(data = fishing_points_sf, aes(fill = effort), 
          colour = "darkblue", alpha = 0.8, size = 0.3) +
  
  # Add colour scale for fishing effort
  scale_fill_viridis_c(name = "Fishing Effort\n(Hours)", 
                       trans = "sqrt",  # Square root transformation for better visualisation
                       option = "plasma") +
  
  # Set coordinate limits
  coord_sf(xlim = c(min_lon, max_lon), 
           ylim = c(min_lat, max_lat)) +
  
  # Styling
  theme_minimal() +
  theme(
    panel.grid = element_line(colour = "lightblue", size = 0.2),
    panel.background = element_rect(fill = "aliceblue")
  ) +
  
  labs(
    title = "Global Fishing Watch Data",
    subtitle = paste0("Region: ", min_lat, " to ", max_lat, "°N, ", min_lon, ifelse(min_lon>=0, "°E", "°W"), " to ", max_lon, ifelse(max_lon>=0, "°E", "°W")," | ", start_date, " to ", end_date),
    x = "Longitude",
    y = "Latitude",
    size = "Fishing Effort"
  )

# Display the plot
print(p1)



cat("Creating fishing events plot with vessel data...\n")

# Convert events data to data frame if it isn't already
events_df <- as.data.frame(track_data)

# Calculate fishing duration in hours for point sizing
events_df$start_time <- as.POSIXct(events_df$start)
events_df$end_time <- as.POSIXct(events_df$end)
events_df$duration_hours <- as.numeric(difftime(events_df$end_time, events_df$start_time, units = "hours"))




# Create a plot with fishing events colored by vessel
p2 <- ggplot() +
  # Add world map
  geom_sf(data = world, fill = "lightgrey", colour = "white", size = 0.2) +
  geom_sf(data = coastline, colour = "darkgrey", size = 0.3) +

  # Add study area boundary
  geom_polygon(data = study_area, aes(x = lon, y = lat), 
               fill = NA, colour = "red", size = 1, linetype = "dashed") +
  
  # Add fishing events points colored by vessel
  geom_point(data = events_df, aes(x = lon, y = lat, 
                                   colour = vessel_name, 
                                   size = duration_hours), 
             alpha = 0.8) +
  
  # Add color scale for vessels
  scale_colour_viridis_d(name = "Vessel Name", option = "turbo") +
  
  # Add size scale for duration
  scale_size_continuous(name = "Duration\n(Hours)", 
                        range = c(1, 6),
                        guide = guide_legend(override.aes = list(alpha = 1))) +
  
  # Centre the plot on the study area
  coord_sf(xlim = c(min_lon, max_lon), 
           ylim = c(min_lat, max_lat)) +
  
  # Styling
  theme_minimal() +
  theme(
    panel.grid = element_line(colour = "lightblue", size = 0.2),
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = "right"
  ) +
  
  labs(
    title = "Global Fishing Watch - Fishing Events by Vessel",
    subtitle = paste0("Region: ", min_lat, " to ", max_lat, "°N, ", min_lon, ifelse(min_lon>=0, "°E", "°W"), " to ", max_lon, ifelse(max_lon>=0, "°E", "°W")," | ", start_date, " to ", end_date),
    x = "Longitude",
    y = "Latitude"
  )

# Display the events plot
print(p2)

# Print summary of events data
cat("\n=== FISHING EVENTS SUMMARY ===\n")
cat("Number of fishing events:", nrow(events_df), "\n")
cat("Number of unique vessels:", length(unique(events_df$vessel_name)), "\n")
cat("Vessels in the data:\n")
vessel_summary <- events_df %>%
  group_by(vessel_name, vessel_flag) %>%
  summarise(
    events = n(),
    total_hours = round(sum(duration_hours, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_hours))
print(vessel_summary)



