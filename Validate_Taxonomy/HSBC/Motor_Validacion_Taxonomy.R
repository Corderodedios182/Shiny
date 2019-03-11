Librerias  <- c("flexdashboard","plotly","ggplot2","readxl","stringr","readr","readxl","stringr","lubridate",
                "xtable","plotrix","readxl","stringr","stats","xtable","knitr","dygraphs","dplyr","dplyr",
                "scales","dplyr","rvest","tidyverse","stringr","magick")

lapply(Librerias, require, character.only = TRUE)

library("kableExtra")
library(googlesheets)
library("googledrive", lib.loc="~/R/win-library/3.4")

mes <- 'Historico'
ruta <- paste("C:/Users/Carlos.Flores/Documents/GitHub/Shiny/Validate_Taxonomy/HSBC/",mes,sep = '')
setwd(ruta)
#Diccionario#
diccionario <- gs_read(gs_key("1baj2dS-aY5rJDDsHgkIZJrec62s890ZsWzx2vatq-OI"))
Base <- gs_read(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"))

###################
### Estructura ####
###################
n = 1
Niveles = unique(diccionario$Panel)
Variables = filter(diccionario, Panel == Niveles[n]) %>%
  select(Variable,field_item_code)
#Desglose de la informacion
eval(parse(text = paste0('Validar = data.frame(Validar = matrix(str_split(Base$',Niveles[n],',"_")))')))
Validar['Panel'] = Niveles[n]
#Conteo de registros por fila
eval(parse(text = paste0('columnas <- data.frame(matrix(ncol = 2,nrow = length(str_split(Base$',Niveles[n],',"_"))))
for (i in seq(length(str_split(Base$',Niveles[n],',"_")))) {
  columnas[i,1:2] <- c(i,length(str_split(Base$',Niveles[n],',"_")[[i]]))  
}')))
colnames(columnas) <- c('No_Registro','No_Columnas') 
#Union del desglose con el conteo de registros
Base_i = cbind(columnas,Validar) %>% 
  select(Panel,No_Registro,No_Columnas,Validar)
#Desglose del nombre  por "_" y estructura 
eval(parse(text = paste0('estructura = data.frame(str_split_fixed(Base$',Niveles[n],',"_",max(Base_i$No_Columnas)))')))
colnames(estructura) <- unique(Variables$Variable)
Base_i = cbind(Base_i,estructura)

names(Base_i)[which(is.na(names(Base_i)))] <- "error_estructura"

############
#Validacion#
############
for(i in 1:length(unique(Variables$Variable))){
  Valores = diccionario %>% filter(Panel == unique(Base_i$Panel)) %>%
  select(Variable,field_item_code)
  x = unique(Valores$Variable)[i]
  eval(parse(text = paste0('V_',x,' = Base_i[,grep("^',x,'$", colnames(Base_i))]')))
  
  eval(parse(text = paste0('Base_i["V_',x,'"] =  is.element(V_',x,',Valores$field_item_code)')))
  eval(parse(text = paste0('rm(V_',x,')')))
  }

###################
#Reglas de Errores#
###################

##Regla 1, tamaño de estructura
Base_i = Base_i %>% 
  mutate(Comentarios = ifelse(Base_i$No_Columnas > length(unique(Variables$Variable))
                              ,"Error_Estructura","Correcta_Estructura"))

##Regla 2 Aunque la estructura este bien, el contenido como esta.
libres = Variables[Variables$field_item_code == "libre" | Variables$field_item_code == "fecha",]
libres = as.vector(names(Base_i[,libres$Variable]))
libres = c(libres,paste0("V_",libres))

Regla_2 <- Base_i %>% select(- libres)
Regla_2 <- select(Regla_2, contains("V_"))

for(i in 1:dim(Regla_2)[2]){
  eval(parse(text = paste0(
  'Regla_2 = Regla_2 %>% mutate(',names(Regla_2)[i],' = ifelse(Regla_2$',names(Regla_2)[i],' == "FALSE","error_',names(Regla_2)[i],'","No error"))')))
}

Base_i_F <- cbind(Base_i, Regla_2)
##Regla 3, Fecha correcta


####################
#Resumen Resultados#
####################

Regla_2















