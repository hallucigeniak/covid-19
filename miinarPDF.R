library(pdftools)

minarPDF <- function(new_pdf){

  new_rows <<- tibble()
  
  for (f in 1:length(new_pdf)){
    print(new_pdf[f])
    
    print(paste0("Procesando archivos fuente: [", f, " de ", length(new_pdf), "]"))
    
    text_casos <- suppressMessages(pdf_text(new_pdf[f])) %>%
      strsplit(split = "\\.\\B", perl = T) %>% 
      unlist() %>%
      str_replace_all("\\\r\\\n|\\s{2,}", replacement = " " ) %>%
      grep(pattern = "M[ée]xico[^.]*(confirmado)[^.]*(casos*)", value = T)

    if (length(text_casos) > 1){
      casos <<- text_casos %>%
        grep(pattern = "[Mm]apa", invert = T, value = T) %>%
        str_replace_all("(?<=\\d),(?=\\d)", replacement = "") %>%
        str_replace_all("\\b(un|uno)\\b", replacement = "1") %>%
        str_replace_all("\\b(dos)\\b", replacement = "2") %>%
        str_replace_all("\\b(tres)\\b", replacement = "3") %>%
        str_replace_all("\\b(cuatro)\\b", replacement = "4") %>%
        str_replace_all("\\b(cinco)\\b", replacement = "5") %>%
        str_replace_all("\\b(seis)\\b", replacement = "6") %>%
        str_replace_all("\\b(siete)\\b", replacement = "7") %>%
        str_replace_all("\\b(ocho)\\b", replacement = "8") %>%
        str_replace_all("\\b(nueve)\\b", replacement = "9") %>%
        str_replace_all("\\b(diez)\\b", replacement = "10") %>%
        str_extract(pattern = "(?<=confirmado )\\d+|\\d+(?= casos)|\\d+(?=casos)") %>%
        as.numeric()
    } else if (length(text_casos) == 1){
      casos <<- text_casos %>%
        str_replace_all("(?<=\\d),(?=\\d)", replacement = "") %>%
        str_replace_all("\\b(un|UN|uno|UNO)\\b", replacement = "1") %>%
        str_replace_all("\\b(dos|DOS)\\b", replacement = "2") %>%
        str_replace_all("\\b(tres|TRES)\\b", replacement = "3") %>%
        str_replace_all("\\b(cuatro|CUATRO)\\b", replacement = "4") %>%
        str_replace_all("\\b(cinco|CINCO)\\b", replacement = "5") %>%
        str_replace_all("\\b(seis|SEIS)\\b", replacement = "6") %>%
        str_replace_all("\\b(siete|SIETE)\\b", replacement = "7") %>%
        str_replace_all("\\b(ocho|OCHO)\\b", replacement = "8") %>%
        str_replace_all("\\b(nueve|NUEVE)\\b", replacement = "9") %>%
        str_replace_all("\\b(diez|DIEZ)\\b", replacement = "10") %>%
        str_extract(pattern = "(?<=confirmado )\\d+|\\d+(?= casos)|\\d+(?=casos)") %>%
        as.numeric()
    } else if (length(text_casos) == 0){
      casos <<- 0
    } else if (is.na(text_casos)){
      casos <<- 0
    }

    fecha <<- suppressMessages(pdf_text(new_pdf[f])) %>%
      strsplit(split = "\\.\\B", perl = T) %>% 
      unlist() %>%
      str_replace_all("\\\r\\\n|\\s{2,}", replacement = " " ) %>%
      grep(pattern = "Subsecretaria de Prevención y Promoción de la Salud|Comunicado Técnico Diario", value = T) %>%
      str_extract("\\d+\\/\\d+\\/\\d+") %>%
      str_replace_all(pattern = "(^\\d{2}).*(\\/\\d{2}\\/).*(\\d{4}$)", replacement = "\\3\\2\\1") %>%
      unique()

    if (length(text_casos) > 1){
      defuncion <<- text_casos %>%
        grep(pattern = "[Mm]apa", invert = T, value = T) %>%
        str_replace_all("(?<=\\d),(?=\\d)", replacement = "") %>%
        str_extract("\\b(\\w+)\\b +defunciones") %>%
        str_replace_all("\\b(un|uno)\\b", replacement = "1") %>%
        str_replace_all("\\b(dos)\\b", replacement = "2") %>%
        str_replace_all("\\b(tres)\\b", replacement = "3") %>%
        str_replace_all("\\b(cuatro)\\b", replacement = "4") %>%
        str_replace_all("\\b(cinco)\\b", replacement = "5") %>%
        str_replace_all("\\b(seis)\\b", replacement = "6") %>%
        str_replace_all("\\b(siete)\\b", replacement = "7") %>%
        str_replace_all("\\b(ocho)\\b", replacement = "8") %>%
        str_replace_all("\\b(nueve)\\b", replacement = "9") %>%
        str_replace_all("\\b(diez)\\b", replacement = "10") %>%
        str_extract("\\d+") %>%
        as.numeric()
    } else if (length(text_casos) == 1){
      defuncion <<- text_casos %>%
        str_replace_all("(?<=\\d),(?=\\d)", replacement = "") %>%
        str_extract("\\b(\\w+)\\b +defunciones") %>%
        str_replace_all("\\b(un|UN|uno|UNO)\\b", replacement = "1") %>%
        str_replace_all("\\b(dos|DOS)\\b", replacement = "2") %>%
        str_replace_all("\\b(tres|TRES)\\b", replacement = "3") %>%
        str_replace_all("\\b(cuatro|CUATRO)\\b", replacement = "4") %>%
        str_replace_all("\\b(cinco|CINCO)\\b", replacement = "5") %>%
        str_replace_all("\\b(seis|SEIS)\\b", replacement = "6") %>%
        str_replace_all("\\b(siete|SIETE)\\b", replacement = "7") %>%
        str_replace_all("\\b(ocho|OCHO)\\b", replacement = "8") %>%
        str_replace_all("\\b(nueve|NUEVE)\\b", replacement = "9") %>%
        str_replace_all("\\b(diez|DIEZ)\\b", replacement = "10") %>%
        str_extract("\\d+") %>%
        as.numeric()
    } else if (length(text_casos) == 0){
      defuncion <<- 0
    } else if (is.na(text_casos)){
      defuncion <<- 0
    }
    new_rows <<- bind_rows(new_rows, bind_cols(fechas = fecha, confirmados = casos, defunciones = defuncion))
  }
  #--------------- Si ya existe la tabla con datos historicos, agregar nuevas filas ---------

  if (file_test("-f", "Data/historico_mx.rds")){

    print("Agregando datos a tabla historica")
    historico_mx <<- readRDS("Data/historico_mx.rds") %>%
      as_tibble()

    new_rows <<- new_rows %>%
      as_tibble() %>%
      mutate(fechas = as_date(fechas),
             confirmados = replace_na(confirmados, 0),
             confirmados = as.numeric(confirmados),
             defunciones = replace_na(defunciones, 0),
             defunciones = as.numeric(defunciones)) %>%
      select(date = fechas, confirmed = confirmados, deaths = defunciones) %>%
      arrange(date)
    
    print(tail(historico_mx))
    print(new_rows)
    historico_mx <<- bind_rows(historico_mx, new_rows)
    saveRDS(historico_mx, "Data/historico_mx.rds")

    return(historico_mx)
    
  } else {
    print("Creando tabla con datos historicos de MX:")
    historico_mx <- cbind(fechas, confirmados, defunciones) %>% 
      as_tibble() %>% 
      arrange(fechas) %>% 
      mutate(fechas = as_date(fechas), 
             confirmados = replace_na(confirmados, 0),
             confirmados = as.numeric(confirmados),
             defunciones = replace_na(defunciones, 0),
             defunciones = as.numeric(defunciones)) %>%
      select(date = fechas, confirmed = confirmados, deaths = defunciones)
    
    saveRDS(historico_mx, "Data/historico_mx.rds")
    return(historico_mx)

  }
}