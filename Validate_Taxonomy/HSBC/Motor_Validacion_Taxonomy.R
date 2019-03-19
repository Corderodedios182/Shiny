Librerias  <- c("flexdashboard","plotly","ggplot2","readxl","stringr","readr","readxl","stringr","lubridate",
                "xtable","plotrix","readxl","stringr","stats","xtable","knitr","dygraphs","dplyr","dplyr",
                "scales","dplyr","rvest","tidyverse","stringr","magick")

lapply(Librerias, require, character.only = TRUE)
library(googlesheets)

#Info Google Sheets
diccionario <- gs_read(gs_key("1baj2dS-aY5rJDDsHgkIZJrec62s890ZsWzx2vatq-OI"))
Hojas = c("HSBC_Facebook","HSBC_DCM","HSBC_DBM","HSBC_Adwords")

for(i in 1:length(Hojas)){
eval(parse(text = paste0('',Hojas[i],' <- gs_read(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"),ws = "',Hojas[i],'")')))
}

###################
### Estructura ####
###################
#Se tienen 3 niveles a revisar
#table(diccionario$Panel)

k = 3 #Hoja a revisar
n = 3 #Se coloca el nivel que se revisara
Niveles = unique(diccionario$Panel)
Variables = filter(diccionario, Panel == Niveles[n]) %>%
  select(Variable,field_item_code)

#Desglose de la informacion
eval(parse(text = paste0('Validar = data.frame(Validar = matrix(str_split(',Hojas[k],'$',Niveles[n],',"_")))')))
Validar['Panel'] = Niveles[n]
#Conteo de registros por fila
eval(parse(text = paste0('columnas <- data.frame(matrix(ncol = 2,nrow = length(str_split(',Hojas[k],'$',Niveles[n],',"_"))))
for (i in seq(length(str_split(',Hojas[k],'$',Niveles[n],',"_")))) {
  columnas[i,1:2] <- c(i,length(str_split(',Hojas[k],'$',Niveles[n],',"_")[[i]]))  
}')))

colnames(columnas) <- c('No_Registro','No_Columnas') 
#Union del desglose con el conteo de registros
Base_i = cbind(columnas,Validar) %>% 
  select(Panel,No_Registro,No_Columnas,Validar)
#Desglose del nombre  por "_" y estructura 
eval(parse(text = paste0('estructura = data.frame(str_split_fixed(',Hojas[k],'$',Niveles[n],',"_",max(Base_i$No_Columnas)))')))
colnames(estructura) <- unique(Variables$Variable)
Base_i = cbind(Base_i,estructura)

names(Base_i)[which(is.na(names(Base_i)))] <- "error_estructura"

Base_i = Base_i[,-2]
Base_i = unique(Base_i)
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
  mutate(Comentarios = ifelse(Base_i$No_Columnas != length(unique(Variables$Variable))
                              ,"Error_Estructura","Correcta_Estructura"))

##Regla 2 Aunque la estructura este bien, el contenido como esta.
libres = Variables[Variables$field_item_code == "libre" | Variables$field_item_code == "fecha",]
libres = as.vector(names(Base_i[,libres$Variable]))
libres = c(libres,paste0("V_",libres))

Regla_2 <- Base_i %>% select(- libres) %>% select(contains("V_"))

for(i in 1:dim(Regla_2)[2]){
  eval(parse(text = paste0(
  'Regla_2 = Regla_2 %>% mutate(',names(Regla_2)[i],' = ifelse(Regla_2$',names(Regla_2)[i],' == "FALSE","error_',names(Regla_2)[i],'","Bien"))')))
}

nombres = gsub("V_","F_",names(Regla_2))
names(Regla_2) <- nombres

Base_i_F <- cbind(Base_i, Regla_2)
Base_i_F <- Base_i_F %>% select(- contains("V_"))
##Regla 3, Fecha correcta


####################
#Resumen Resultados#
####################

Regla_2 <- Base_i %>% select(- libres) %>% select(contains("V_"))

Porcentaje = (sum(apply(Regla_2, 2, sum)))/(dim(Regla_2)[1]*dim(Regla_2)[2])
Porcentaje


#Escritura en el Sheets

#FACEBOOK
#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "F_Campaign_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "F_Channel_Unifying_String_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "F_Creative_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#DCM
#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "DCM_Campaign_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "DCM_Channel_Unifying_String_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "DCM_Creative_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#DBM
#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "DBM_Campaign_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "DBM_Channel_Unifying_String_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "DBM_Creative_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#
#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "AD_Campaign_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "AD_Channel_Unifying_String_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)

#gs_edit_cells(gs_key("1g7WHq0vTRGnqUUyFT3vuEa_piUT_NkZ2KBz0r0wp1_4"), ws = "AD_Creative_Name_Generator", anchor = "B1", input = Base_i_F, byrow = TRUE)










