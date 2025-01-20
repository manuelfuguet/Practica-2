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

