library(dplyr)
library(tidyr)
library(rvest)
library(pdftools)
library(stringr)
library(lubridate)
source("scripts/regex.R", encoding = "utf-8")
source("scripts/miinarPDF.R")
#------------------- Existen los archivos fuente en el directorio local? -------------------
getTablaHistorico <- function(){
  
  local_pdf <- list.files(path = "Data/pdf/")
  
  if (file_test("-f", "Data/historico_mx.rds")){
    historico_mx <- readRDS("Data/historico_mx.rds")
  }
  #--------------------- Descargar la lista de reportes diarios --------
  pdf_list_1 <- read_html("https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov") %>%
    html_nodes( 'a') %>% html_attr('href') %>%
    grep(pattern = "\\.pdf$", value = T) %>%
    grep(pattern = "Comunicado_Tecnico", value = T) %>%
    sort()
  
  pdf_list_1 <- paste0("https://www.gob.mx", pdf_list_1)
  
  #--------------------- Descargar el ultimo comunicado --------------  
  urls <- read_html("https://www.gob.mx/salud/es/archivo/prensa") %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    grep(pattern = "[Cc]omunicado-[Tt]ecnico-[Dd]iario-\\d*", value = T) %>%
    sort(decreasing = T) %>%
    str_replace(pattern = ".*(/.*=es).*", replacement = "https://www.gob.mx/salud/prensa\\1")
  
  pdf_list_2 <- read_html(urls[1]) %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    grep(pattern = "[Cc]omunicado_[Tt]ecnico_[Dd]iario_\\d*", value = T) %>%
    unique()
  
  pdf_list <- c(pdf_list_1, pdf_list_2)
  
  if(any(!basename(pdf_list) %in% local_pdf)){
    historico_mx <- minarPDF(pdf_list = pdf_list, local_pdf = local_pdf)
  }
  
    return(historico_mx)
  }

#---------------- Scraping tabla de casos por estado de wikipedia --------------------------
getTablaEstados <- function(){
  df_estados_mx <- read_html("https://es.wikipedia.org/wiki/Pandemia_de_enfermedad_por_coronavirus_de_2020_en_M%C3%A9xico") %>% 
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]/tbody/tr/td[1]/table') %>% html_table(fill=T, header = T) %>%
    tail(-1) %>%
    head(-2)
  
  df_estados_mx <- df_estados_mx[,c(1,2,3)]
  colnames(df_estados_mx) <- c("Estados", "Casos", "Muertes")
  saveRDS(df_estados_mx, paste0("Data/casos_por_estado_", Sys.Date(), ".rds"))
  
  list_rds <- list.files("Data/", "\\d\\.rds", full.names = T)
  
  rds_df <- tibble()
  
  for (i in list_rds){
    fecha <- basename(i) %>%
      str_extract("\\d{4}.*\\d{2}")
    
    temp_df <- readRDS(i) %>%
      mutate(date = as_date(fecha)) %>%
      as_tibble() 
    
    temp_df$Estados <- temp_df$Estados %>%
      str_replace_all(paste0(estados_regex, ".*"), "\\1") %>%
      str_replace_all(pattern = "\\[.+\\]| $", replacement = "")
    
    temp_df <- temp_df %>%
      mutate(Estados = ifelse(Estados == "México", "Estado de México", Estados))
    
     temp_df$Casos <- temp_df$Casos %>%
       str_replace_all("\\s+", "") %>%
       str_extract_all("^\\d+|\\d+$", simplify = T) %>%
       as.numeric()
     
     temp_df$Muertes <- temp_df$Muertes %>%
       str_replace_all("\\s+", "") %>%
       str_extract_all("^\\d+|\\d+$", simplify = T) %>%
       as.numeric()
     
    rds_df <- rbind(rds_df, temp_df)
    
  }

  return(as_tibble(rds_df))
}

#------------------ Data Italia -----------------
getTablaItalia <- function(){
  raw_html_it <- read_html("https://es.wikipedia.org/wiki/Pandemia_de_enfermedad_por_coronavirus_de_2020_en_Italia#Estad%C3%ADsticas") %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill=T, header = T) %>%
    mutate(Pais = "Italia") %>%
    select(Pais, date = Día, confirmed = `Casos confirmados`, deaths = Muertos) %>%
    as_tibble()
  
  raw_html_it$date <- raw_html_it$date %>%
    str_replace_all("^de", "27 de") %>%
    str_replace_all("^", "2020-") %>%
    str_replace_all("(-\\d{1,2}) de enero", "-01\\1") %>%
    str_replace_all("(-\\d{1,2}) de febrero", "-02\\1") %>%
    str_replace_all("(-\\d{1,2}) de marzo", "-03\\1") %>%
    str_replace_all("(-\\d{1,2}) de abril", "-04\\1") %>%
    str_replace_all("-(\\d)$", "-0\\1") %>%
    as_date()
  
  raw_html_it$confirmed <- raw_html_it$confirmed %>%
    str_replace_all("\\s+", "") %>%
    as.numeric()
  
  raw_html_it$deaths <- raw_html_it$deaths %>%
    str_replace_all("\\s+", "") %>%
    as.numeric()
  
  df_it <- raw_html_it %>%
    mutate(lag_confirmed = lag(confirmed, default = 0),
           lag_deaths = lag(deaths, default = 0),
           new_cases = confirmed - lag_confirmed,
           new_deaths = deaths - lag_deaths,
           days_since_first_case = row_number()) %>%
    select(Pais, date, days_since_first_case, confirmed, deaths, new_cases, new_deaths)
  
  return(df_it)
}

#------------------ Data Espana -----------------
getTablaEspana <- function(){
   
    data_table <- read_html("https://es.wikipedia.org/wiki/Pandemia_de_enfermedad_por_coronavirus_de_2020_en_España#Estadísticas") %>%
    html_nodes('.wikitable') %>% html_node('th') %>% html_text() 
  
  data_table_index <- grep("casos.*confirmados.*España", data_table)
  
  raw_html_es <- read_html("https://es.wikipedia.org/wiki/Pandemia_de_enfermedad_por_coronavirus_de_2020_en_España#Estadísticas") %>%
    html_nodes('.wikitable') %>% `[[`(data_table_index) %>% html_table(fill=T, header = F) %>%
    mutate(Pais = "España") %>%
    select(Pais, date = X2, confirmed = X4, deaths = X11) %>%
    tail(-3) %>%
    head(-1) %>%
    as_tibble()
  
  raw_html_es$date <- str_replace_all(raw_html_es$date, "^", "2020-") %>%
    str_replace_all("(-\\d{1,2}) de enero", "-01\\1") %>%
    str_replace_all("(-\\d{1,2}) de febrero", "-02\\1") %>%
    str_replace_all("(-\\d{1,2}) de marzo", "-03\\1") %>%
    str_replace_all("(-\\d{1,2}) de abril", "-04\\1") %>%
    str_replace_all("-(\\d)$", "-0\\1") %>%
    as_date()
  
  raw_html_es$confirmed <- raw_html_es$confirmed %>%
    str_replace_all("\\s+", "") %>%
    as.numeric()
  
  raw_html_es$deaths <- raw_html_es$deaths %>%
    str_replace_all("\\s+", "") %>%
    as.numeric()
  
  df_es <- raw_html_es %>%
    mutate(lag_confirmed = lag(confirmed, default = 0),
           lag_deaths = lag(deaths, default = 0),
           new_cases = confirmed - lag_confirmed,
           new_deaths = deaths - lag_deaths,
           days_since_first_case = row_number()) %>%
    select(Pais, date, days_since_first_case, confirmed, deaths, new_cases, new_deaths)
  
  return(df_es)
}

#------------------ Plotear animaciones -------------
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

#------------------ Data Taiwan -----------------------
#https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Taiwan