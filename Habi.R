library(openxlsx)
library(dplyr)
library(tidyr) 
library(readxl)
library(tidyverse)
library(datos)
library(Hmisc)

Datos <- read.csv("C:/Users/Camila.Ochoa/Downloads/data_FIFA.csv", sep= ";")


#DATA CLEANING
#Se realiza el proceso de limpieza de datos, transformación, eliminación de na´s y ceros
head(Datos)
tail(Datos)
glimpse(Datos)


Datos1<- Datos %>% distinct(ID, .keep_all = TRUE)
Datos1<- Datos %>% distinct(Name, .keep_all = TRUE)

sapply(Datos1, function(x) sum(is.na(x)))

Datos1 <- Datos1[!is.na(Datos1$International.Reputation),]


Datos1$Unidad_Value<- str_sub(Datos1$Value,-1,-1)
Datos1$Value=  gsub("M","",Datos1$Value)
Datos1$Value= gsub("€","",Datos1$Value)
Datos1$Value= gsub("K","",Datos1$Value)
which(is.na(Datos1$Value))
Datos1 <- Datos1[!is.na(Datos1$Value),]
Datos1$Value= as.numeric(Datos1$Value)
Datos1$Value_total<- ifelse(Datos1$Unidad_Value=="M", (Datos1$Value*1000000),(Datos1$Value*1000))

Datos1$Wage= gsub("K","",Datos1$Wage)
Datos1$Wage= gsub("€","",Datos1$Wage)
Datos1 <- Datos1[!is.na(Datos1$Wage),]
Datos1$Wage= as.numeric(Datos1$Wage)
Datos1$Wage_total= Datos1$Wage*1000000


Datos1$Unidad_release<- str_sub(Datos1$Release.Clause,-1,-1)
Datos1$Release.Clause=  gsub("M","",Datos1$Release.Clause)
Datos1$Release.Clause= gsub("€","",Datos1$Release.Clause)
Datos1$Release.Clause= gsub("K","",Datos1$Release.Clause)
Datos1$Release.Clause= as.numeric(Datos1$Release.Clause)
Datos1 <- mutate_at(Datos1, c("Release.Clause"), ~replace(., is.na(.), 0))
Datos1$Total_release<-ifelse(Datos1$Unidad_release=="M", (Datos1$Release.Clause *1000000),(Datos1$Release.Clause*1000))

#Data final para respuestas de punto B

data_final= Datos1[,c("ID","Name","Age","Nationality","Overall","Potential","Club","International.Reputation",
                      "Height","Weight","Total_release","Value_total","Wage_total","Position")]


#1. Jugadores con características atípicas  salario, edad

desv_salario<- sqrt(var(data_final$Wage_total))
desv_edad <- sqrt(var(data_final$Age))


data_final$resta_wage<- data_final$Wage - 1.5*desv_salario
data_final$resta_edad <- data_final$Age - 1.5*desv_edad

#Las variables renta_wage y resta_edad permiten observar las distancias entre los valores y las desviación 
#Lo que permite ver, una vez ordenado, cuáles valores están mas lejos y son atípicos

#Obtenemos los 10 registros con que más se alejan de la media, tienen las desviaciones más altas
max_wage<- data_final[order(-data_final$resta_wage),][1:10,]
min_wage<- data_final[order(data_final$resta_wage),][1:10,]
max_age<- data_final[order(-data_final$resta_edad),][1:10,]
min_age<- data_final[order(data_final$resta_edad),][1:10,]

#2. Top 5 de los mejores salarios, valoración general y potencial

best_salario= data_final[order(-data_final$Wage_total),][1:5,]
best_valoracion_general= data_final[order(-data_final$Overall),][1:5,]
best_potencial= data_final[order(-data_final$Potential),][1:5,]

#3. Comparativo entre clubes de distribución de salario y edad

comparativo= data_final[,c("Club","Age","Wage_total")] %>% group_by(Club)%>%
  summarise(Age= mean(Age), Wage= mean(Wage_total))

#El club Paraná tiene los jugadores con mayor edad en promedio
#El Real Madrid es el club que presenta en promedio simple los salarios más altas


#4. Correlación 
data_prov= data_final[,c("Value_total","International.Reputation","Total_release")]
matriz_correlacion<- cor(data_prov)

#A partir de la matriz de correlacion se puede observar una correlación positiva entre las variables 

pairs(data_prov)

#El gráfico permite observar que la relación más evidente es entre Value_total y Total_release
#Con una correlación positiva


#5. Exploratorio de características de jugadores con base en su posición 

Jugador= Datos1[1:1,]

#Este jugador corresponde a Messi. Su posición es RF que corresponde al extremo derecho
#Aunque se parece a los delanteros, esta posición se usa para formaciones 4321
#Aunque esta es su posición, de acuerdo a sus habilidades, es compatible con otras posiciones
#como LF que tienen el mismo rol, pero su ubicación corresponde al otro extremo
#Así mismo, puede tomar el rol de CF que se encarga de realizar goles y de distraer a los defensores
#Otra de las posiciones que puede tomar es CAM con el fin de crear oportunidades de gol
#Los valores más bajos los toma en posiciones defensivas, pues sus habilidades están poco desarrolladas en este campo


agrupacion= Datoa1 %>% group_by(Position)%>%
  summarise(Jugadores=sum(ID), Wage= mean(Wage_total), Age= mean(Age),Overal= mean(Overall), Potential=mean(Potential),
            Crossing=mean(Crossing), Finishing=mean(Finishing), HeadingAccuracy=mena(HeadingAccuracy),
            ShortPassing=mean(ShortPassing), Volleys=mean(Volleys), Dribbling=mena(Dribbling),
            Curve=mean(Curve), FKAccuracy=mean(FKAccuracy), LongPassing=mean(LongPassing),BallContro=mean(BallContro))

#Exportar data para construir dashboard
write.xlsx(Datos1, "data_habi.xlsx")

