# library(here)
# library(dplyr)
# library(OpenStreetMap)
library(RJSONIO)

# Function for formatting members
format_member_table <- function(member_table){
  member_table$inst <- gsub(',|-|\n|"', " ", member_table$Institución)
  member_table$inst <- gsub("\\s+", " ", member_table$inst)
  member_table$inst <- gsub(" ", "+", member_table$inst)
  member_table$city <- gsub(" ", "+", member_table$Ciudad)
  member_table$country <- gsub(" ", "+", member_table$País)
  member_table$afil_id <- paste(member_table$Institución, member_table$Ciudad, member_table$País, sep = "_")
  return(member_table)
}

# Function for searching institutions
look_up <- function(location) {
  
  # Return NA if no field present (otherwise finds Namibia (NA)):
  if (is.na(location["inst"]) & is.na(location["city"]) & is.na(location["country"])) {
    return(c("lon" = NA, "lat" = NA))
  }
  
  # First Search when only Institution field present:
  if (is.na(location["city"]) & is.na(location["country"])) {
    url1 <- paste0(
      "http://nominatim.openstreetmap.org/search?q=", 
      location["inst"], 
      "&limit=9&format=json"
    )
    
    resOSM <- RJSONIO::fromJSON(url1)
    
    # If it has results, return coordinates
    if (length(resOSM) > 0) {
      return(c("lon" = resOSM[[1]]$lon, "lat" = resOSM[[1]]$lat))
    }
  }
  
  # Second Search using city and country as additional fields:
  url2 <- paste0(
    "http://nominatim.openstreetmap.org/search?street=", 
    location["inst"], 
    "&city=", 
    location["city"], 
    "&country=", 
    location["country"], 
    "&limit=9&format=json"
  )
  
  resOSM2 <- RJSONIO::fromJSON(url2)
  
  # If it has results, return coordinates
  if (length(resOSM2) > 0) {
    return(c("lon" = resOSM2[[1]]$lon, "lat" = resOSM2[[1]]$lat))
  }
  
  # Third Search using only city and country:
  url3 <- paste0(
    "http://nominatim.openstreetmap.org/search?city=", 
    location["city"], 
    "&country=", 
    location["country"], 
    "&limit=9&format=json"
  )
  
  resOSM3 <- RJSONIO::fromJSON(url3)
  
  # If search has results, return coordinates
  if (length(resOSM3) > 0) {
    return(c("lon" = resOSM3[[1]]$lon, "lat" = resOSM3[[1]]$lat))
  }
  
  # If all searches fail, return NA
  return(c("lon" = NA, "lat" = NA))
}

#Function to retrieve coordinates for all affiliations not stored yet in institutuioncoords.csv
update_coord_table <- function(member.table, coords.table){
  
  existing <- which( (member.table$afil_id %in% coords.table$afil_id))
  lookup   <- which(!(member.table$afil_id %in% coords.table$afil_id))
  
  if(length(lookup)>0){
  coords <- t(apply(member.table[lookup,], 1, look_up)) |> 
    as.data.frame()
  
  
  new.coords <-  data.frame("afil_id" = member.table[lookup, "afil_id"],
                            "lon" = coords[,"lon"],
                            "lat" = coords[,"lat"])
  
  coords.table <- rbind(coords.table, new.coords)
  }
  return(coords.table)
}



#### TRY FUNCTIONS

# Load the new list of members 
# member.table <- gsheet::gsheet2tbl("1tRlCcIxurHIDg-CCHsFFbkD3e4YkiDlA3PkFlTcBlk8") |>
#   format_member_table()

# Load the old list of members 
# members.old <- readr::read_csv("MembersInfo.csv")

# # extract unique institutions
# institutions.list <- members.old |> 
#   distinct(Afiliación) |> 
#   mutate(Institución = Afiliación,
#          Ciudad = NA,
#          País = NA) |> 
#   format_member_table()

# # fix institutions
# fixed_institutions <- institutions.list |> 
#   mutate(inst = if_else(inst %in% c("EBD", "EBD+CSIC",
#                                     "Estación+Biológica+de+Doñana+(EBD+CSIC)"), 
#                         "Estación+Biológica+de+Doñana", inst),
#          inst = if_else(inst %in% c("BC3",
#                                     "Basque+Center+for+Climate+Change+(BC3)"), 
#                         "UPV+Leioa", inst),
#          inst = if_else(inst == "Instituto+de+Recursos+Naturales+y+Agrobiología+de+Sevilla+(IRNAS+CSIC)+41012+Sevilla+(España)", 
#                         "Instituto+de+Recursos+Naturales+y+Agrobiología+Sevilla", inst),
#          inst = if_else(inst == "Departamento+de+Botánica+Ecología+y+Fisiología+Vegetal.+Universidad+de+Córdoba", 
#                         "Universidad+de+Córdoba", inst),
#          inst = if_else(inst %in% c("iuFOR+Instituto+de+Investigación+en+Gestión+Forestal+Sostenible+Universidad+de+Valladolid+(Campus+de+Palencia)",
#                                     "iuFOR+Universidad+de+Valladolid+(campus+de+Palencia)"),
#                         "Instituto+de+Investigación+en+Gestión+Forestal+Sostenible", inst),
#          inst = if_else(inst == "Universidad+Pablo+de+Olavide+Dpto.+Sistemas+Físicos+Químicos+y+Naturales+Ctra.Utrera+Km+1+41013+Sevilla+Spain", 
#                         "Universidad+Pablo+de+Olavide+Sevilla", inst),
#          inst = if_else(inst == "Centro+Tecnológico+Forestal+de+Cataluña", "CTFC", inst),
#          inst = if_else(inst == "Laboratorio+de+Ecología+Instituto+Investigación+Sistema+Tierra+Andalucía+Universidad+de+Granada", 
#                         "Universidad+de+Granada", inst), 
#          inst = if_else(inst == "	+CEO+in+TechInCrop+Asistente+Honorario+Dpto.+Biología+Vegetal+y+Ecología.+Área+Fisiología+Vegetal.+Universidad+de+Sevilla", 
#                         "Universidad+de+Sevilla", inst),
#          inst = if_else(inst == "Swedish+University+of+Agricultural+Sciences+SLU+Faculty+of+Forest+Sciences+Department+of+Forest+Resource+Management+Section+of+Mathematical+Statistics+Applied+to+Forest+Sciences",
#                         "Swedish+University+of+Agricultural+Sciences", inst),
#          inst = if_else(inst == "Institudo+Meditarráneo+de+Estudios+Avanzados+(IMEDEA).+CSIC+UIB", 
#                         "Instituto+Mediterráneo+de+Estudios+Avanzados", inst),
#          inst = if_else(inst == "Forest+Ecology+and+Restoration+Group+Universidad+de+Alcalá", 
#                         "Universidad+de+Alcalá", inst),
#          inst = if_else(inst == "Centro+de+Ecología+Aplicada+Baeta+Neves+Universidad+de+Lisboa+Universidad+de+Cádiz+(a+partir+de+abril)", 
#                         "Universidad+de+Cádiz", inst),
#          inst = if_else(inst == "Departamento+de+Biología+y+Geología+Física+y+Química+Inorgánica+Universidad+Rey+Juan+Carlos",
#                         "Universidad+Rey+Juan+Carlos", inst),
#          inst = if_else(inst == "Universidad+de+Sevilla+Joint+Research+Unit+LifeWatch+Spain+(JRU+LW.ES)", 
#                         "Universidad+de+Sevilla", inst),
#          inst = if_else(inst == "Grupo+Ingeniería+Electronica+(US)+JRU+LifeWatch+Spain", 
#                         "Universidad+de+Sevilla", inst),
#          inst = if_else(inst == "GRECO+Institute+of+Aquatic+Ecology+University+of+Girona+Girona+Spain", 
#                         "Universidad+de+Girona", inst),
#          inst = if_else(inst == "Graduate+School+of+Design+Harvard+University", 
#                         "Harvard+University", inst),
#          inst = if_else(inst == "Área+de+Biodiversidad+Universidad+Rey+Juan+Carlos", 
#                         "Universidad+Rey+Juan+Carlos", inst),
#          inst = if_else(inst == "Centre+de+Recerca+Ecològica+i+Aplicacions+Forestals+(CREAF)", 
#                         "CREAF", inst),
#          inst = if_else(inst == "Universidad+de+Huelva+Departamento+de+Ciencias+Integradas",
#                         "Universidad+de+Huelva", inst),
#          inst = if_else(inst == "Data+Science+and+Machine+Learning+Engineer.+Engineer+specialized+in+Bioinformatics+and+also+Business+Marketing+studies+for+the+University+of+Zaragoza+University+of+Wales+and+Barcelona+University.", 
#                         "UB", inst),
#          inst = if_else(inst == "", "", inst),
#          inst = if_else(inst == "", "", inst),
#          inst = if_else(inst %in% c("Freelance", 
#                                     "Trabajador+independiente",
#                                     "sí"), NA, inst)) |> 
#   mutate(country = if_else(inst %in% c("UB", "CREAF",
#                                        "Universidad+de+Córdoba",
#                                        "Universidad+de+Sevilla", 
#                                        "Instituto+de+Investigación+en+Gestión+Forestal+Sostenible",
#                                        "CTFC",
#                                        "Universidad+de+Granada",
#                                        "Gestión+Ambiental+de+Navarra+(GAN+NIK)"), "Spain", country)) |> 
#   mutate(city = if_else(inst %in% c("Universidad+de+Córdoba"), "Córdoba", city),
#          city = if_else(inst %in% c("Universidad+de+Sevilla"), "Sevilla", city),
#          city = if_else(inst %in% c("Instituto+de+Investigación+en+Gestión+Forestal+Sostenible"), "Palencia", city),
#          city = if_else(inst %in% c("CTFC"), "Lérida", city),
#          city = if_else(inst %in% c("Universidad+de+Granada"), "Granada", city),
#          city = if_else(inst %in% c("Gestión+Ambiental+de+Navarra+(GAN+NIK)"), "Navarra", city),
#          city = if_else(inst %in% c("UB", "CREAF"), "Barcelona", city),
#          city = if_else(inst %in% c(""), "", city))
# Extract coords

# coords <- as.data.frame(t(apply(fixed_institutions, 1, look_up)))
# institutions.coords <- cbind(fixed_institutions, coords)
# 
# readr::write_csv(institutions.coords, "institution_coords.csv")
# 
# # Try mapping
# 
# library(leaflet)
# library(sf)
# library(htmltools)
# 
# points <- institutions.coords |> 
#   filter(!is.na(lon)) |> 
#   sf::st_as_sf(coords = c("lon", "lat"))
# 
# leaflet(points) |> 
#   addTiles() |> 
#   addMarkers(popup = points, 
#              label = ~(Institución),
#              labelOptions = labelOptions(zoomAnimation = T),
#              clusterOptions = markerClusterOptions())
