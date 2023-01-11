# Importamos todas las librer??as a utilizar
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


########### LIMPIEZA DE LOS DATOS ###############
# Lectura y breve descripci??n de los datos
booking <- read.csv("hotels_data.csv", header = TRUE)
summary(booking)

## PREPROCESADO DE DATOS ##
# Identificamos el tipo de dato asignado a cada variable
res <- sapply(booking, function(x) class(x))
kable(data.frame(var=names(res), clase=as.vector(res)))

# Separamos los valores de la variable hotel coordinates en dos nuevas columnas, latitud y longitud
# La columna hotel_coordinates se elimina en el proceso

booking <- booking %>%
  separate(hotel_coordinates, c("latitude", "longitude"), sep = ",", remove = FALSE)

# Transformamos a formato num??rico
booking$latitude <- as.numeric(booking$latitude)
booking$longitude <- as.numeric(booking$longitude)

# Definimos un vector con los nombres de las columnas a crear
columns <-  c("staff_score", "facilities_score", "cleanliness_score", "comfort_score", "value_for_money_score", "location_score", "free_wifi_score")

# Separamos los strings (diccionarios) en funci??n de las comas y creamos las columnas correspondientes. La primera columna creada es una NA porque todos los primeros registros en cada diccionario est??n vac??os (por un fallo en la extracci??n de datos). Estableciendo la columna como NA, indicamos a la funci??n separate que ignore dichos datos y no cree columna alguna para ellos.
booking <- booking %>%
  separate(hotel_scores, c(NA, "staff_score", "facilities_score", "cleanliness_score", "comfort_score", "value_for_money_score", "location_score", "free_wifi_score"), sep = ",", remove = FALSE)

# Extraemos los valores num??ricos de cada columna con la funci??n del paquete readr, "parse_number"
booking[columns] <- apply(booking[columns], 2, readr::parse_number)

booking <- booking %>%
  # Separamos el nombre del mes del resto de elementos del registro para check_in y check_out.
  separate(check.in, c(NA, "month", NA), sep = "-", remove = FALSE) %>%
  mutate(is_december = ifelse(month == "December", 1, 0),
         is_march = ifelse(month == "March", 1, 0))

# Hemos tenido que cambiar esta funci??n para que el extract no diera problemas
Sys.setlocale('LC_ALL', 'C')
# Extraemos los valores de codigo postal de la columna addres utilizando expressiones regulares
booking <- booking %>%
  extract(address, c("postal_code"), regex = "( [0-9]{5} )", remove = FALSE)

# Definimos las features que consideramos que son interesantes. Utilizamos este formato para filtrar por expresi??n regular.
feature_list <- c("('Free WiFi')", "('Air conditioning')", "('24-hour front desk')", "('Safe')", "('Heating')", "('Elevator')", "('Private Bathroom')", "('Non-smoking rooms')", "('Aparments')", "('City view')","('Kitchen')", "('Pet friendly')", "('Swimming pool')", "('Balcony')")

# Creamos un bucle en el que se recorre cada elemento de la lista de features
for (feature in feature_list){
  # Se eliminan los parent??sis y comillas de la variable local feature para crear el nombre de la columna.
  col_name <- str_replace(str_replace(str_to_lower(str_extract(feature, "([A-Z][a-z]*( |-)?[A-Z]?[a-z]* ? ?[A-Z]?[a-z]*)")), " ", "_"), "-", "_")
  booking <- booking %>%
    # Extraemos el nombre del feature de cada uno de los registros (strings). En caso de que no encuentre ning??n valor devuelve un NA.
    extract(features, c(col_name), regex = feature, remove = FALSE) %>%
    # Transformamos la columna que acaba de ser creada para que indique con un 1 si el registro tenia dicho servicio y 0 si el valor era NA (no tenida dicho servicio)
    mutate_(.dots = setNames(list(paste0("as.integer(!is.na(",col_name,"))")), col_name))
}

# Evaluamos la longitud de la descripci??n del hotel
booking$length_description <- lengths(gregexpr("\\W+", booking$hotel_description)) + 1

# Funci??n que encuentra el precio m??nimo de la habitaci??n dado un string con la informaci??n de las habitaciones
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

# Aplicamos la funci??n a todos los valores
min_price_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  min_price_vector <- append(min_price_vector, find_min_price(booking$room_data[i]))
}

# Guardamos los resultados en una nueva columna.
booking$min_price <- min_price_vector

# Funci??n que encuentra el precio m??ximo de la habitaci??n dado un string con la informaci??n de las habitaciones
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

# Aplicamos la funci??n a todos los valores
max_price_vector <- c()
for (i in seq(1, length(booking$room_data))) {
  max_price_vector <- append(max_price_vector, find_max_price(booking$room_data[i]))
}

# Guardamos los resultados en una nueva columna.
booking$max_price <- max_price_vector

# Buscamos si hay una habitaci??n en suite
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

# Buscamos si hay opci??n de apartamento
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

# Buscamos si tiene cancelaci??n gratuita
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


## TRATAMIENTO DE DATOS FALTANTES ##
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


# Tranformamos los valores -1 en NAs para poder llevar a cabo el proceso de inputaci??n correctamente.
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

# Imputamos los datos con K Neirest Neighbours
booking$max_price <- kNN(booking)$max_price
booking$min_price <- kNN(booking)$min_price 

## DATOS EXTREMOS ##

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = num_rooms, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$num_rooms)$out

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = latitude, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$latitude)$out

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = longitude, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$longitude)$out

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = hotel_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$hotel_score)$out

# Seleccionamos muestras de habitaciones con nota menor a 7
kable(booking %>%
        filter(hotel_score <= 7.0) %>%
        dplyr::select(c("hotel_score", "facilities_score", "cleanliness_score", "location_score")) %>%
        sample_n(5))

# Seleccionamos muestras de habitaciones con nota de 10
kable(booking %>%
        filter(hotel_score == 10.0) %>%
        dplyr::select(c("hotel_score", "facilities_score", "cleanliness_score", "location_score") %>%
                        sample_n(3))
      
# Graficamos box-plot con ggplot
ggplot(booking, aes(y = facilities_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$facilities_score)$out

# Seleccionamos una muestra de hoteles con facilities_score menor a 6.5
kable(booking %>%
        filter(facilities_score <= 6.5) %>%
        dplyr::select(c("hotel_score", "facilities_score", "cleanliness_score", "location_score")) %>%
        sample_n(3))

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = staff_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$staff_score)$out

# Seleccionamos una muestra de hoteles con staff_score menor a 6.7
kable(booking %>%
        filter(staff_score <= 6.7) %>%
        dplyr::select(c("hotel_score", "facilities_score", "staff_score", "location_score")) %>%
        sample_n(3))

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = cleanliness_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$cleanliness_score)$out

# Seleccionamos una muestra de hoteles con cleanliness_score menor a 7
kable(booking %>%
        filter(cleanliness_score < 7.0) %>%
        dplyr::select(c("hotel_score", "facilities_score", "cleanliness_score", "location_score")) %>%
        sample_n(3))

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = comfort_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$comfort_score)$out

# Seleccionamos una muestra de hoteles con comfort_score menor a 7
kable(booking %>%
        filter(comfort_score < 7.0) %>%
        dplyr::select(c("hotel_score", "facilities_score", "comfort_score", "location_score")) %>%
        sample_n(3))

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = value_for_money_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$value_for_money_score)$out

# Seleccionamos una muestra de hoteles con value_for_money_score menor a 7
kable(booking %>%
        filter(value_for_money_score <= 7.0) %>%
        dplyr::select(c("hotel_score", "facilities_score", "value_for_money_score", "location_score")) %>%
        sample_n(3))

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = location_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$location_score)$out

# Seleccionamos una muestra de hoteles con location_score menor a 7
kable(booking %>%
        filter(location_score < 7.0) %>%
        dplyr::select(c("hotel_score", "facilities_score", "value_for_money_score", "location_score")) %>%
        sample_n(3))

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = free_wifi_score, color = city))+
  geom_boxplot() +
  facet_grid(~city)

# Sacamos los valores considerados outliers
boxplot.stats(booking$free_wifi_score)$out

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = length_description, color = city))+
  geom_boxplot() +
  facet_grid(~city)
# Sacamos los valores considerados outliers
boxplot.stats(booking$length_description)$out

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = min_price, color = city))+
  geom_boxplot() +
  facet_grid(~city)
# Sacamos los valores considerados outliers
boxplot.stats(booking$min_price)$out

# Graficamos box-plot con ggplot
ggplot(booking, aes(y = max_price, color = city))+
  geom_boxplot() +
  facet_grid(~city)
# Sacamos los valores considerados outliers
boxplot.stats(booking$max_price)$out

########### SELECCI??N E INTEGRACI??N #############

# Seleccionamos con la funci??n "select" de dplyr todas las columnas excepto las que ya hemos limpiado para crear otras, adem??s de aquellasque nos van a ser de poca utilidad.
booking <- booking %>%
  dplyr::select(-c("name", "search_date", "hotel_coordinates", "hotel_scores", "check.in", "check.out", "address", "features", "hotel_description", "room_data"))

# Leemos los datos de vuelos
flights <- read.csv("avia_tf_apal_linear.csv.gz")
summary(flights)

proc_flights <- flights %>% 
  # Filtramos para quedarnos solo con los aeropuertos de inter??s
  # C??digo OACI/ICAO: Barcelona --> ES_LEBL; Valencia --> ES_LEVC; Madrid: ES_LEMD
  filter(rep_airp %in% c("ES_LEBL", "ES_LEVC", "ES_LEMD")) %>% 
  # Filtramos para quedarnos solo con los datos de carga de pasajeros
  filter(tra_meas == "PAS_CRD") %>%
  # Nos interesa solo los datos mensuales, as?? que filtramos por ellos
  filter(freq == "M") %>%
  separate(TIME_PERIOD, c("YEAR", "MONTH"), sep = "-") %>%
  # Nos quedamos solo con los ??ltimos 5 a??os y los meses de Marzo, Junio y Diciembre
  filter(YEAR %in% c("2022", "2021", "2020", "2019", "2018") & MONTH %in% c("03", "06", "12")) %>%
  # No nos interesa los datos por aerol??nea, solo los generales
  filter(airline == "TOTAL") %>%
  # Modificamos la columna de MONTH para tener los nombres del mes y cambiamos el c??digo del aeropuerto por el nombre de la ciudad
  mutate(month_name = ifelse(MONTH == "03", "March", ifelse(MONTH == "06", "June", "December")),
         city_airp = ifelse(rep_airp == "ES_LEBL", "Barcelona", ifelse(rep_airp == "ES_LEVC", "Valencia", "Madrid"))) %>%
  # Agrupamos por ciudad y mes y calculamos la media de vuelos.
  group_by(rep_airp, month_name) %>%
  mutate(mean_flights = mean(OBS_VALUE)) %>%
  ungroup() %>%
  dplyr::select(c(city_airp, month_name, mean_flights))

# Nos quedamos con los registros ??nicos
final_flights <- unique(proc_flights)

# Hacemos un merge con bookings
booking <- booking %>%
  merge(final_flights, by.x = c("city", "month"), by.y = c("city_airp", "month_name"))

# Exportamos los datos a un archivo csv
write.csv(booking, "data/hotel_data_processed.csv")

########### ANALISIS DE LOS DATOS ###############
## Estudio de la correlacion
# Transformamos las variables a num??rico
booking$postal_code <- as.numeric(booking$postal_code)
booking$latitude <- as.numeric(booking$latitude)
booking$longitude <- as.numeric(booking$longitude)
booking$free_wifi <- as.numeric(booking$free_wifi)
booking$is_apartment <- as.numeric(booking$is_apartment)
booking$pet_friendly <- as.numeric(booking$pet_friendly)
booking$page_count <- booking$page_count + 1

# Seleccionamos solo las variables num??ricas
booking_numeric <- booking %>%
  dplyr::select(-c("city", "month", "as.integer(!is.na(NA))"))

mattmp <- booking_numeric %>%
  dplyr::select(c("adults", "children", "num_rooms", "postal_code", "latitude", "longitude", "hotel_score", "length_description", 
                  "is_suite", "is_apartment", "has_free_cancelation", "min_price","max_price", "current_page", "in_page_count", "page_count"))

sapply(mattmp, function(y) sum(is.na(y)))

# Creamos una matriz de correlaci??n del dataset
cor_mat <- cor(mattmp, method = "spearman")

mattmp <- booking_numeric %>%
  dplyr::select(c("hotel_score", "staff_score", "facilities_score", "cleanliness_score", "comfort_score", "value_for_money_score", "location_score",
                  "mean_flights", "free_wifi_score", "is_december", "is_march", "min_price", "max_price", "current_page", "in_page_count", "page_count"))

sapply(mattmp, function(y) sum(is.na(y)))

# Creamos una matriz de correlaci??n del dataset
cor_mat <- cor(mattmp, method = "spearman")
mattmp <- booking_numeric %>%
  dplyr::select(c("balcony", "swimming_pool", "pet_friendly", "kitchen", "city_view", "is_apartment", "non_smoking_rooms", "hotel_score", 
                  "private_bathroom", "elevator", "heating", "safe", "air_conditioning", "free_wifi", "max_price", "min_price", "current_page", "in_page_count", "page_count"))

sapply(mattmp, function(y) sum(is.na(y)))

# Creamos una matriz de correlaci??n del dataset
cor_mat <- cor(mattmp, method = "spearman")

# Filtramos para obtener cuales son las variables m??s correlacionadas con nuestra variable objetivo, current_page
cor_mat <- cor(booking_numeric, method = "spearman")
cor_mat_df <- as.data.frame(cor_mat)
cor_mat_df %>%
  filter(abs(page_count) > 0.065) %>%
  filter(page_count < 0.95) %>%
  arrange(page_count) %>%
  dplyr::select(page_count)

# Filtramos para obtener cuales son las variables m??s correlacionadas con nuestra variable objetivo, page_count
cor_mat <- cor(booking_numeric, method = "spearman")
cor_mat_df <- as.data.frame(cor_mat)
cor_mat_df %>%
  filter(abs(max_price) > 0.1) %>%
  filter(max_price < 0.95) %>%
  arrange(max_price) %>%
  dplyr::select(max_price)

## Normalidad y homogeneidad de la varianza

# Seleccionamos las variables a analizar
norm_cols <- c("value_for_money_score", "comfort_score", "hotel_score", "staff_score", "cleanliness_score", "location_score", "facilities_score", "length_description", "min_price", "max_price", "page_count")

booking_norm <- booking_numeric %>%
  dplyr::select(norm_cols)

# Ploteamos los qq-plots y los histogramas
par(mfrow=c(2,2))
for(i in 1:ncol(booking_norm)){
  qqnorm(booking_norm[,i], main = paste("QQ-Plot de",colnames(booking_norm)[i]), col = "navy")
  qqline(booking_norm[,i], col = "red")
  hist(booking_norm[,i], main = paste("Histograma de", colnames(booking_norm)[i]), xlab=colnames(booking_norm)[i], freq = FALSE)
}

# Calculamos el test de Shapiro-Wilk para cada variable y sacamos su pvalor
pvalues <- t(data.frame(lapply(booking_norm, function(y) format(shapiro.test(y)$p.value, digits = 3))))

kable(pvalues, col.names = c("p-value"))

# Obtenemos los Q-Q plots
par(mfrow=c(2,3))
for(i in 1:ncol(booking_norm)){
  b <- boxcox(lm(booking_norm[,i] ~ 1))
  lambda <- b$x[which.max(b$y)]
  qqnorm((booking_norm[,i]^lambda - 1)/lambda, main = paste("QQ-Plot de",colnames(booking_norm)[i]), col = "navy")
  qqline((booking_norm[,i]^lambda - 1)/lambda, col = "red")
  hist((booking_norm[,i]^lambda - 1)/lambda, main = paste("Histograma de", colnames(booking_norm)[i]), xlab=colnames(booking_norm)[i], freq = FALSE)
}

# Generamos el scatter para compromar la homocedasticidad de page_count cuando dividimos por air_conditioning.
ggplot(booking, aes(x = max_price, y = page_count, color = air_conditioning)) + 
  geom_point() +
  facet_grid(air_conditioning~.)

# Ajustamos el test de fligner-killeen para page_count y air_conditioning
fligner.test(page_count ~ air_conditioning, data = booking_pcount)

# Generamos el scatter para compromar la homocedasticidad de page_count cuando dividimos por free_wifi.
ggplot(booking, aes(x = max_price, y = page_count, color = free_wifi)) + 
  geom_point()+
  facet_grid(free_wifi~.)

# Generamos la variable para el contraste que realizaremos en la siguiente secci??n. Si el hotel score es mayor a 9 se marca con una flag
booking <- booking %>%
  mutate(high_hotel_score = ifelse(hotel_score >= 9, 1, 0))

# Ploteamos el scatter para comprobar la homocedasticidad de page_count cuando dividimos por la nueva variable.
ggplot(booking, aes(x = max_price, y = page_count, color = high_hotel_score)) + 
  geom_point()+
  facet_grid(high_hotel_score~.)

# Ajustamos el test de fligner-killeen para page_count y la nueva variable
fligner.test(page_count ~ high_hotel_score, data = booking)

# Generamos la variable para el contraste que realizaremos en la siguiente secci??n. Si el precio m??ximo supera la media de precios maximos
# pone un flags
booking <- booking %>%
  mutate(high_max_price = ifelse(max_price >= mean(max_price), 1, 0))

# Ploteamos el scatter para comprobar la homocedasticidad de page_count cuando dividimos por la nueva variable.
ggplot(booking, aes(x = min_price, y = page_count, color = high_max_price)) + 
  geom_point()+
  facet_grid(high_max_price~.)

# Ajustamos el test de fligner-killeen para page_count y la nueva variable
fligner.test(page_count ~ high_max_price, data = booking)


## Contraste de hipotesis
# Hipotesis 1

# Buscamos el conjunto de datos de estudio:
X1 <- booking %>%
  filter(air_conditioning==1) %>%
  dplyr::select("page_count")
X1 <- as.vector(unlist(X1),'numeric')

X2 <- booking %>%
  filter(air_conditioning==0) %>%
  dplyr::select("page_count")
X2 <- as.vector(unlist(X2),'numeric')

# Buscamos la varianza de ambas variables
var1 <- var(X1)
var2 <- var(X2)

# Definimos el nivel de confianza al 95%
alfa <- 0.05

# Buscamos las medias y la el n??mero de registros en cada variable
mean1 <- mean(X1); n1 <- length(X1)
mean2 <- mean(X2); n2 <- length(X2)

# Calculamos el valor observado y el valor cr??tico
tobs <- (mean1-mean2)/sqrt((1/n1 + 1/n2) * ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2))
tcrit <- pt(1-alfa, df = (n1+n2-2))

# Finalmente calculamos el pvalue
pvalue <- pt(tobs, lower.tail=TRUE, df = (n1+n2-2))
print(paste("The p-value obtained is: ", pvalue))
# Mostramos las medias de las dos variables
print(paste("The mean of the group 1 is: ", mean(X1)))
print(paste("The mean of the group 2 is: ", mean(X2)))

# Hipotesis 2

# Buscamos el conjunto de datos de estudio:
X1 <- booking %>%
  filter(has_free_cancelation==1) %>%
  dplyr::select("page_count")
X1 <- as.vector(unlist(X1),'numeric')

X2 <- booking %>%
  filter(has_free_cancelation==0) %>%
  dplyr::select("page_count")
X2 <- as.vector(unlist(X2),'numeric')

# Buscamos la varianza de ambas variables
var1 <- var(X1)
var2 <- var(X2)

# Definimos el nivel de confianza al 95%
alfa <- 0.05

# Buscamos las medias y la el n??mero de registros en cada variable
mean1 <- mean(X1); n1 <- length(X1)
mean2 <- mean(X2); n2 <- length(X2)

# Calculamos el valor observado y el valor cr??tico
tobs <- (mean1-mean2)/sqrt((1/n1 + 1/n2) * ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2))
tcrit <- pt(1-alfa/2, df = (n1+n2-2))

# Finalmente calculamos el pvalue
pvalue <- pt(tobs, lower.tail=TRUE, df = (n1+n2-2))
print(paste("The p-value obtained is: ", pvalue))
# Mostramos las medias de las dos variables
print(paste("The mean of the group 1 is: ", mean(X1)))
print(paste("The mean of the group 2 is: ", mean(X2)))


# Hipotesis 3

# Buscamos el conjunto de datos de estudio:
X1 <- booking %>%
  filter(high_hotel_score == 1) %>%
  dplyr::select("page_count")
X1 <- as.vector(unlist(X1),'numeric')

X2 <- booking %>%
  filter(high_hotel_score == 0) %>%
  dplyr::select("page_count")
X2 <- as.vector(unlist(X2),'numeric')

# Buscamos la varianza de ambas variables
var1 <- var(X1)
var2 <- var(X2)

# Definimos el nivel de confianza al 95%
alfa <- 0.05

# Buscamos las medias y la el n??mero de registros en cada variable
mean1 <- mean(X1); n1 <- length(X1)
mean2 <- mean(X2); n2 <- length(X2)

# Calculamos el valor observado y el valor cr??tico
tobs <- (mean1-mean2)/sqrt((1/n1 + 1/n2) * ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2))
tcrit <- pt(1-alfa, df = (n1+n2-2))

# Finalmente calculamos el pvalue
pvalue <- pt(tobs, lower.tail=TRUE, df = (n1+n2-2))
print(paste("The p-value obtained is: ", pvalue))
# Mostramos las medias de las dos variables
print(paste("The mean of the group 1 is: ", mean(X1)))
print(paste("The mean of the group 2 is: ", mean(X2)))

# Hipotesis 4

# Buscamos el conjunto de datos de estudio:
X1 <- booking %>%
  filter(high_max_price == 1) %>%
  dplyr::select("page_count")
X1 <- as.vector(unlist(X1),'numeric')

X2 <- booking %>%
  filter(high_max_price == 0) %>%
  dplyr::select("page_count")
X2 <- as.vector(unlist(X2),'numeric')

# Buscamos la varianza de ambas variables
var1 <- var(X1)
var2 <- var(X2)

# Definimos el nivel de confianza al 95%
alfa <- 0.05

# Buscamos las medias y la el n??mero de registros en cada variable
mean1 <- mean(X1); n1 <- length(X1)
mean2 <- mean(X2); n2 <- length(X2)

# Calculamos el valor observado y el valor cr??tico
tobs <- (mean1-mean2)/sqrt((1/n1 + 1/n2) * ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2))
tcrit <- pt(1-alfa, df = (n1+n2-2))

# Finalmente calculamos el pvalue
pvalue <- pt(tobs, lower.tail=FALSE, df = (n1+n2-2))
print(paste("The p-value obtained is: ", pvalue))
# Mostramos las medias de las dos variables
print(paste("The mean of the group 1 is: ", mean(X1)))
print(paste("The mean of the group 2 is: ", mean(X2)))


## Regresion lineal
# Regresion lineal definitiva para page_count
#Create the linear model
mrl_final <- lm(page_count~staff_score + value_for_money_score + free_wifi_score 
                + balcony + pet_friendly + air_conditioning + swimming_pool 
                + mean_flights + length_description, data=booking)

# Regresion lineal definitiva para max_price
#Create the linear model
mrl_price <- lm(max_price~staff_score + location_score + balcony + air_conditioning 
                + free_wifi + swimming_pool + non_smoking_rooms + mean_flights 
                + length_description + safe, data=booking)
