# Cargar librerías
library(httr)
library(XML)
library(dplyr)

# URL de la página web que quieres descargar
url <- "https://www.mediawiki.org/wiki/MediaWiki"

# Descargar la página web
respuesta <- GET(url)

# Verificar si la solicitud fue exitosa
if (status_code(respuesta) == 200) {
  # Extraer el contenido en formato texto
  contenido_html <- content(respuesta, as = "text")
  
  # Convertir el contenido HTML a XML
  contenido_xml <- htmlParse(contenido_html)
  
  # Mostrar estructura del XML
  print(contenido_xml)
} else {
  cat("Error: No se pudo descargar la página web. Código de estado:", status_code(respuesta), "\n")
}

# Obtener el título de la página
titulo <- xpathSApply(contenido_xml, "//title", xmlValue)
cat("El título de la página es:", titulo, "\n")

# Buscar y extraer todos los enlaces
enlaces <- xpathSApply(contenido_xml, "//a", xmlGetAttr, "href")
textos_enlace <- xpathSApply(contenido_xml, "//a", xmlValue)

# Verificar si hay enlaces NULL y tratarlos
nulos_enlaces <- sapply(enlaces, is.null)
nulos_textos <- sapply(textos_enlace, is.null)

enlaces[nulos_enlaces] <- NA
textos_enlace[nulos_textos] <- NA

# Crear un dataframe con los enlaces y sus textos
data_enlaces <- data.frame(link = enlaces, text = textos_enlace)

# Base URL del dominio
base_url <- "https://www.mediawiki.org"

# Procesar cada enlace del data.frame original 'data_enlaces'
data_enlaces$estado <- sapply(data_enlaces$link, function(link) {
  # Manejo de enlaces (relativos, absolutos, subdominios e internos)
  if (is.na(link)) {
    return(NA)
  } else if (grepl("^http", link)) {
    full_url <- link  # URLs absolutas
  } else if (grepl("^//", link)) {
    full_url <- paste0("https:", link)  # Subdominios
  } else if (grepl("^#", link)) {
    full_url <- paste0(base_url, link)  # URLs internas con tags
  } else {
    full_url <- paste0(base_url, link)  # URLs relativas
  }
  
  # Intentar obtener el código de estado usando HEAD
  tryCatch({
    response <- HEAD(full_url)
    status_code(response)
  }, error = function(e) {
    NA  # Asignar NA en caso de error
  })
})

# Resumir los datos en el data.frame final 'final_data'
final_data <- data_enlaces %>%
  group_by(link) %>%
  summarize(
    texto_enlace = first(text),
    Frecuencia = n(),
    estado = first(estado)
  )

# Mostrar el resumen final
print(final_data)

# Cargar librerías necesarias
library(ggplot2)
library(gridExtra)

# Clasificar URLs absolutas y relativas
data_enlaces$url_tipo <- ifelse(grepl("^http", data_enlaces$link), "Absoluta", "Relativa")

# 1. Histograma: Frecuencia de aparición de enlaces separados por tipo
# Usando gráficos base
hist_data <- table(data_enlaces$url_tipo)
barplot(hist_data, 
        main = "Frecuencia de Enlaces por Tipo (Gráficos Base)", 
        xlab = "Tipo de URL", 
        ylab = "Frecuencia", 
        col = c("black", "blue"))

# Usando ggplot2
gg_hist <- ggplot(data_enlaces, aes(x = url_tipo)) +
  geom_bar(fill = c("black", "blue")) +
  theme_minimal() +
  labs(
    title = "Frecuencia de Enlaces por Tipo",
    x = "Tipo de URL",
    y = "Frecuencia"
  )

# 2. Gráfico de barras: Suma de enlaces a dominios externos vs internos
data_enlaces$dominio <- ifelse(
  grepl("^http", data_enlaces$link) & grepl("mediawiki\\.org", data_enlaces$link),
  "Interno", "Externo"
)
dominio_data <- table(data_enlaces$dominio)

# Usando gráficos base
barplot(dominio_data, 
        main = "Enlaces Internos vs Externos (Gráficos Base)", 
        xlab = "Dominio", 
        ylab = "Cantidad de Enlaces", 
        col = c("black", "blue"))

# Usando ggplot2
gg_dominio <- ggplot(data_enlaces, aes(x = dominio, fill = dominio)) +
  geom_bar() +
  scale_fill_manual(values = c("black", "blue")) +
  theme_minimal() +
  labs(
    title = "Enlaces Internos vs Externos",
    x = "Dominio",
    y = "Cantidad de Enlaces"
  )

# 3. Gráfico de tarta: Porcentaje de status de los enlaces
status_data <- table(data_enlaces$estado)
pie_data <- data.frame(
  Status = names(status_data),
  Frecuencia = as.numeric(status_data)
)
pie_data$Porcentaje <- pie_data$Frecuencia / sum(pie_data$Frecuencia) * 100

# Usando gráficos base
pie(pie_data$Frecuencia, 
    labels = paste0(pie_data$Status, " (", round(pie_data$Porcentaje, 1), "%)"),
    col = rainbow(length(pie_data$Frecuencia)),
    main = "Porcentaje de Status de Enlaces (Gráficos Base)")

# Usando ggplot2
gg_pie <- ggplot(pie_data, aes(x = "", y = Porcentaje, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Porcentaje de Status de Enlaces"
  ) +
  scale_fill_brewer(palette = "Set3")

# Componer los gráficos en una sola figura (usando ggplot2)
grid.arrange(gg_hist, gg_dominio, gg_pie, nrow = 2)

