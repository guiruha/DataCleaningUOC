# Importamos todas las librerías a utilizar
knitr::opts_chunk$set(echo = TRUE)
options(tinytex.verbose = TRUE)
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(stringr))
suppressWarnings(library(knitr))
suppressWarnings(library(VIM))
suppressWarnings(library(corrplot))
suppressWarnings(library(MASS))
suppressWarnings(library(devtools))
install_github("ProcessMiner/nlcor")
suppressWarnings(library(nlcor))

# Lectura y breve descripción de los datos
booking <- read.csv("hotels_data.csv", header = TRUE)
summary(booking)

# Identificamos el tipo de dato asignado a cada variable
res <- sapply(booking, function(x) class(x))
kable(data.frame(var=names(res), clase=as.vector(res)))

# Separamos los valores de la variable hotel coordinates en dos nuevas columnas, latitud y longitud
# La columna hotel_coordinates se elimina en el proceso

booking <- booking %>%
  separate(hotel_coordinates, c("latitude", "longitude"), sep = ",", remove = FALSE)

# Transformamos a formato numérico
booking$latitude <- as.numeric(booking$latitude)
booking$longitude <- as.numeric(booking$longitude)

# Definimos un vector con los nombres de las columnas a crear
columns <-  c("staff_score", "facilities_score", "cleanliness_score", "comfort_score", "value_for_money_score", "location_score", "free_wifi_score")

# Separamos los strings (diccionarios) en función de las comas y creamos las columnas correspondientes. La primera columna creada es una NA porque todos los primeros registros en cada diccionario están vacíos (por un fallo en la extracción de datos). Estableciendo la columna como NA, indicamos a la función separate que ignore dichos datos y no cree columna alguna para ellos.
booking <- booking %>%
  separate(hotel_scores, c(NA, "staff_score", "facilities_score", "cleanliness_score", "comfort_score", "value_for_money_score", "location_score", "free_wifi_score"), sep = ",", remove = FALSE)

# Extraemos los valores numéricos de cada columna con la función del paquete readr, "parse_number"
booking[columns] <- apply(booking[columns], 2, readr::parse_number)

booking <- booking %>%
  # Separamos el nombre del mes del resto de elementos del registro para check_in y check_out.
  separate(check.in, c(NA, "month", NA), sep = "-", remove = FALSE) %>%
  mutate(is_december = ifelse(month == "December", 1, 0),
         is_march = ifelse(month == "March", 1, 0))

# Hemos tenido que cambiar esta función para que el extract no diera problemas
Sys.setlocale('LC_ALL', 'C')
# Extraemos los valores de codigo postal de la columna addres utilizando expressiones regulares
booking <- booking %>%
  extract(address, c("postal_code"), regex = "( [0-9]{5} )", remove = FALSE)

# Definimos las features que consideramos que son interesantes. Utilizamos este formato para filtrar por expresión regular.
feature_list <- c("('Free WiFi')", "('Air conditioning')", "('24-hour front desk')", "('Safe')", "('Heating')", "('Elevator')", "('Private Bathroom')", "('Non-smoking rooms')", "('Aparments')", "('City view')","('Kitchen')", "('Pet friendly')", "('Swimming pool')", "('Balcony')")

# Creamos un bucle en el que se recorre cada elemento de la lista de features
for (feature in feature_list){
  # Se eliminan los parentésis y comillas de la variable local feature para crear el nombre de la columna.
  col_name <- str_replace(str_replace(str_to_lower(str_extract(feature, "([A-Z][a-z]*( |-)?[A-Z]?[a-z]* ? ?[A-Z]?[a-z]*)")), " ", "_"), "-", "_")
  booking <- booking %>%
    # Extraemos el nombre del feature de cada uno de los registros (strings). En caso de que no encuentre ningún valor devuelve un NA.
    extract(features, c(col_name), regex = feature, remove = FALSE) %>%
    # Transformamos la columna que acaba de ser creada para que indique con un 1 si el registro tenia dicho servicio y 0 si el valor era NA (no tenida dicho servicio)
    mutate_(.dots = setNames(list(paste0("as.integer(!is.na(",col_name,"))")), col_name))
}

# Evaluamos la longitud de la descripción del hotel
booking$length_description <- lengths(gregexpr("\\W+", booking$hotel_description)) + 1

# Función que encuentra el precio mínimo de la habitación dado un string con la información de las habitaciones
find_min_price <- function(text) {
  if (text=="{}"){
    return(NA)
  }
  a <- gregexpr("([0-9]*', 'room_capacity)", text)
  
  min_price <- NA
  for (value in a[[1]]){
    substring <- substr(text, value, value+20)
    price <- as.numeric(strsplit(substring, "'")[[1]][1])
    if (is.na(min_price)) {
      min_price <- price
    }
    
    if (price < min_price) {
      min_price <- price
    } 
  }
  if (min_price == 0){
    return(NA)
  }
  else {
    return(min_price)
  }
}

# Aplicamos la función a todos los valores
min_price_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  min_price_vector <- append(min_price_vector, find_min_price(booking$room_data[i]))
}

# Guardamos los resultados en una nueva columna.
booking$min_price <- min_price_vector

# Función que encuentra el precio máximo de la habitación dado un string con la información de las habitaciones
find_max_price <- function(text) {
  if (text=="{}"){
    return(NA)
  }
  a <- gregexpr("([0-9]*', 'room_capacity)", text)
  
  max_price <- NA
  for (value in a[[1]]){
    substring <- substr(text, value, value+20)
    price <- as.numeric(strsplit(substring, "'")[[1]][1])
    if (is.na(max_price)) {
      max_price <- price
    }
    
    if (price > max_price) {
      max_price <- price
    } 
  }
  if (max_price == 0){
    return(NA)
  }
  else {
    return(max_price)
  }
}

# Aplicamos la función a todos los valores
max_price_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  max_price_vector <- append(max_price_vector, find_max_price(booking$room_data[i]))
}

# Guardamos los resultados en una nueva columna.
booking$max_price <- max_price_vector

# Buscamos si hay una habitación en suite
is_suite_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  is_suite <- gregexpr("(suite)", booking$room_data[i])[[1]][1]
  
  if (is_suite==-1) {
    is_suite_vector <- append(is_suite_vector, 0)
  } else {
    is_suite_vector <- append(is_suite_vector, 1)
  }
}

# Guardamos los resultados en una nueva columna.
booking$is_suite <- is_suite_vector

# Buscamos si hay opción de apartamento
is_apartment_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  is_apartment <- gregexpr("(Apartment)", booking$room_data[i])[[1]][1]
  
  if (is_apartment==-1) {
    is_apartment_vector <- append(is_apartment_vector, 0)
  } else {
    is_apartment_vector <- append(is_apartment_vector, 1)
  }
}

# Guardamos los resultados en una nueva columna.
booking$is_apartment <- is_apartment_vector

# Buscamos si tiene cancelación gratuita
free_cancelation_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  free_cancelation <- gregexpr("(Free cancellation)", booking$room_data[i])[[1]][1]
  
  if (free_cancelation==-1) {
    free_cancelation_vector <- append(free_cancelation_vector, 0)
  } else {
    free_cancelation_vector <- append(free_cancelation_vector, 1)
  }
}

# Guardamos los resultados en una nueva columna.
booking$has_free_cancelation <- free_cancelation_vector

# Comprobamos que columnas tienen datos faltantes
kable(sapply(booking, function(y) sum(length(which(is.na(y))))))

# Comprobamos algunos de los registros donde hay datos faltantes de postal_code
kable(booking[is.na(booking$postal_code),] %>%
        dplyr::select(c("city", "month", "postal_code", "longitude", "hotel_score", "staff_score", "facilities_score")) %>%
        head(3)
)
# Filtramos los datos para eliminar aquellos registros que tienen NAs en postal_code
booking <- booking[!is.na(booking$postal_code),]

# Comprobamos que columnas tienen datos faltantes
kable(sapply(booking, function(y) sum(length(which(is.na(y))))))

# Comprobamos algunos de los registros donde hay datos faltantes de hotel_score
kable(booking[is.na(booking$hotel_score),] %>%
        dplyr::select(c("city", "month", "hotel_score", "staff_score", "location_score", "longitude", "facilities_score", "cleanliness_score", "comfort_score")) %>%
        head(5))

# Comprobamos algunos de los registros donde hay datos faltantes de hotel_score
kable(booking[booking$hotel_score == "-1",] %>%
        dplyr::select(c("city", "month", "hotel_score", "staff_score", "free_wifi_score", "free_wifi")) %>%
        tail(5))


# Tranformamos los valores -1 en NAs para poder llevar a cabo el proceso de inputación correctamente.
booking$hotel_score <- ifelse(booking$hotel_score == "-1", NA, booking$hotel_score)

# Imputamos los datos con K Neirest Neighbours
booking$hotel_score <- kNN(booking)$hotel_score
booking$staff_score <- kNN(booking)$staff_score 
booking$facilities_score <- kNN(booking)$facilities_score
booking$cleanliness_score <- kNN(booking)$cleanliness_score
booking$comfort_score <- kNN(booking)$comfort_score
booking$value_for_money_score <- kNN(booking)$value_for_money_score
booking$location_score <- kNN(booking)$location_score
booking$free_wifi_score <- kNN(booking)$free_wifi_score

# Comprobamos valores faltantes en min_price
kable(booking[is.na(booking$min_price),] %>%
        dplyr::select(c("city", "month", "hotel_score", "max_price", "min_price")) %>%
        tail(5))


