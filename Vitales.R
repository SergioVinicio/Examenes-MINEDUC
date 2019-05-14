setwd("~/Tareas universidad/7 Semestre/Elements of Machine Learning/Proyecto Final/Vitales")

#Librerias
library(dplyr)

#Leer dataset omitiendo levels de los factores
ExamenGraduandos2017 <- read.csv("ExamenGraduandos2017.csv", stringsAsFactors = FALSE)

#Omitiar las NA
data <- na.omit(ExamenGraduandos2017)

#Eliminar los #NULL!
data <- data %>%  filter_all(all_vars(!grepl('#NULL!',.)))

#Codificar la variable urbana o rural de la escuela primaria
#1 Urbano, 0 Rural
data$Ed_Area_De_Escuela_Finalizo_Primaria <- ifelse(data$Ed_Area_De_Escuela_Finalizo_Primaria == 11, 1, 0)

#Eliminar variables no tan relevantes
#Es la misma variable para todos
data$Cod_Nivel <- NULL

#Quitar observaciones con 0 en columna Idioma materno, porque es igual a un valor nulo
data <- subset(data, data$IE_Idioma_Materno_Recodificado != 0)
data <- subset(data, data$Identificacion_Etnica_RECO != 0)
data <- subset(data, data$IE_Frecuencia_Uso_Idioma_Materno_RECO != 9)


#Mejor omitir la columna, de lo contrario se pierden casi 100,000 observaciones
data$IE_Segundo_Idioma_Materno_RECO <- NULL

#Quitar Codigo establecimiento
data$Cod_Estable <- NULL

#Variables que sean rescatar
data$CC_Cuenta_Linea_Telefonica <- as.numeric(as.character(data$CC_Cuenta_Linea_Telefonica))
data$CC_Ambiente_Cocina_Separada <- as.numeric(as.character(data$CC_Ambiente_Cocina_Separada))
data$CC_Electricidad <- as.numeric(as.character(data$CC_Electricidad))
data$Fm_Asistio_Escuela_Mama <- as.numeric(as.character(data$Fm_Asistio_Escuela_Mama))
data$Fm_Asistio_Escuela_Papa <- as.numeric(as.character(data$Fm_Asistio_Escuela_Papa))
data$Ed_Trabaja_Actualmente <- as.numeric(as.character(data$Ed_Trabaja_Actualmente))
data$Ed_Asistio_Preprimaria <- as.numeric(as.character(data$Ed_Asistio_Preprimaria))
data$Etnia_RECO <- as.numeric(as.character(data$Etnia_RECO))
data$Edad_RECO <- as.numeric(as.character(data$Edad_RECO))
data$Edad <- as.numeric(as.character(data$Edad))
data$CC_Tiene_Familia_Celular <- as.numeric(as.character(data$CC_Tiene_Familia_Celular))
data$Ed_Repitio_Algun_Grado_Primaria <- as.numeric(as.character(data$Ed_Repitio_Algun_Grado_Primaria))
data$CC_Servicio_Internet <- as.numeric(as.character(data$CC_Servicio_Internet))
data$CC_Servicio_Tv_Cable <- as.numeric(as.character(data$CC_Servicio_Tv_Cable))

#Codificar variables o eliminar innecesarias
#todas son iguales
#library(fastDummies)

#Es una variable irrelevante
data$Corr_Año <- NULL

#Cod Region se quita al igual que region para ponerlas como variables dummies
#Si no aparece en ninguna de region entonces es del area metropolitana
DummyRegion <- dummy_cols(data$Region, remove_first_dummy = TRUE)
DummyRegion <- DummyRegion[,-1]
data$Region <- NULL
data$Cod_Region <- NULL
data <- cbind(data,DummyRegion)

#Cod Departamento, si no aparece en ningun cod depar es de la capital, se quita cod departa al igual
#que departamento
DummyDepart <- dummy_cols(data$Departamento, remove_first_dummy = TRUE)
DummyDepart <- DummyDepart[,-1]
data$Departamento <- NULL
data$Cod_Depa <- NULL
data <- cbind(data,DummyDepart)

#Cod municipio y municipio se quitaron por ser demasiados y no creo valga la pena crear tantas dummies
data$Cod_Muni <-NULL
data$Municipio <- NULL

#Nivel se omite ya que es irrevelante, porque todos tienen lo mismo
data$Nivel <- NULL

#Rama abstracta y codigo rama abstraca se quitan y se pasan a dummy, si no aparece es Perito
DummyRama <- dummy_cols(data$Rama_Abstracta, remove_first_dummy = TRUE)
DummyRama <- DummyRama[,-1]
data$Rama_Abstracta <- NULL
data$Cod_Rama_Abstracta <- NULL
data <- cbind(data,DummyRama)

#Se omite la variable edad_Reco y se usa solo Edad para fines del modelo ya que sería redundante
data$Edad_RECO <- NULL

#Se pone como dummy el codigo de area, 1 si es urbano y 0 si es rural
data$Cod_Area <- ifelse(data$Cod_Area == 11, 1, 0)

#Se omite jornada y cod_jornada para ponerlo como variable dummy en el modelo
#Si no aparece entonces es de jornada doble
DummyJornada <- dummy_cols(data$Jornada, remove_first_dummy = TRUE)
DummyJornada <- DummyJornada[,-1]
data$Jornada <- NULL
data$Cod_Jornada <- NULL
data <- cbind(data,DummyJornada)

#Se omite ciclo y codigo de ciclo para ponerlo como variable dummy en el modelo
#Si no aparece entonces es anual
DummyCiclo <- dummy_cols(data$Ciclo, remove_first_dummy =  TRUE)
DummyCiclo <- DummyCiclo[,-1]
data$Ciclo <- NULL
data$Cod_Ciclo <- NULL
data <- cbind(data,DummyCiclo)

#Se omite la variable Etnia Reco para ponerlo como dummy
#Se quita tambien la de identificación etnia reco ya que es redundante
#Si no aparece, entonces es Ladino
DummyEtniaReco <- dummy_cols(data$Etnia_RECO, remove_first_dummy =  TRUE)
DummyEtniaReco <-DummyEtniaReco[,-1]
colnames(DummyEtniaReco) <- c("Maya","Garifuna","Xinca","Extranjero")
data$Etnia_RECO <- NULL
data$Identificacion_Etnica_RECO <- NULL
data <- cbind(data,DummyEtniaReco)


#Idioma materno recodificado se omite por redundante y se pone como dummy solo la de 
#idioma materno Reco
#Si no aparece entonces su idioma materno es español
data$IE_Idioma_Materno_Recodificado <- NULL
DummyIdiomaMaterno <- dummy_cols(data$IE_Idioma_Materno_RECO, remove_first_dummy = TRUE)
DummyIdiomaMaterno <- DummyIdiomaMaterno[,-1]
colnames(DummyIdiomaMaterno) <- c("Mas_de_un_idioma_materno","Maya_IMaterno",
                                  "Extranjero_IMaterno","Xinca_IMaterno","Garifuna_IMaterno")
data$IE_Idioma_Materno_RECO <- NULL
data <- cbind(data,DummyIdiomaMaterno)

#Jornada que trabaja, se quita la variable y se crean las dummies respectivas
#Si no aparece en ninguna, entonces trabaja en jornada Nocturna
DummyJornadaTrabajo <- dummy_cols(data$Ed_Jornada_Trabaja_RECO, remove_first_dummy = TRUE)
DummyJornadaTrabajo <- DummyJornadaTrabajo[,-1]
colnames(DummyJornadaTrabajo) <- c("Matutina","No_trabaja","Fin de semana",
                                   "Vespertina","Mas_de_una_jornada","Completa")
data$Ed_Jornada_Trabaja_RECO <- NULL
data$Ed_Trabaja_Actualmente <- NULL
data <- cbind(data, DummyJornadaTrabajo)

#Poner como dummy la variable si papa asistio a la escuela o no al igual para la mama
#Si no sabe si asistio o no, se supondrá que no lo hizo
data$Fm_Asistio_Escuela_Papa <- ifelse(data$Fm_Asistio_Escuela_Papa == 0 | 
                                         data$Fm_Asistio_Escuela_Papa == 2, 0, 1)

data$Fm_Asistio_Escuela_Mama <- ifelse(data$Fm_Asistio_Escuela_Mama == 0 |
                                         data$Fm_Asistio_Escuela_Mama == 2, 0, 1)

#Descartando donde no dieron respuesta al uso de tecnologia
data <- subset(data, data$Tec_Periodos_Semana_Utiliza_Computadora_RECO != 9)
data <- subset(data, data$Tec_Horas_Diarias_Uso_Compu_Casa_RECO != 9)
data <- subset(data, data$Tec_Horas_Diarias_Uso_Compu_Estab_RECO != 9)
data <- subset(data, data$Mate_Periodos_Matematicas_Semana_RECO != 9)
data <- subset(data, data$Lect_Dias_Semana_Lee_Periodicos_RECO != 9)
data <- subset(data, data$Lect_Periodos_Lectura_Semana_RECO != 9)
data <- subset(data, data$Lect_Libros_Completos_Ha_Leido_RECO != 9)

#Quedarse solo con valores numericos
data <- select_if(data, is.numeric)

#Quitar variables irrelevantes
data$Cod_Evaluacion <- NULL
data$Num_Boleta <- NULL
data$Cod_Bases_Informes <- NULL
data$Barcode <- NULL
data$Año_Cierre <- NULL

#Guardar los datos
#write.csv(data,"DataColab.csv", fileEncoding = "utf-8")


#Regresion Lineal
#regressor = lm(formula = data$Logro_Mate ~ .,data = data)


