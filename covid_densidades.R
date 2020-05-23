###### Limpiamos el área de trabajo
rm(list=ls())

##### Cargamos bibliotecas
library(ggplot2)
library(ggridges)

##### Cargamos los datos

#### Datos covid
# Fuente: Our world in data
covid<-read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# ISO countries
iso.countries<-read.csv("https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv")
#Descriptor de continentes
iso.continentes<-read.csv("https://pkgstore.datahub.io/core/continent-codes/continent-codes_csv/data/c3b949e4fe1a00d4997ea5a41bfe7e22/continent-codes_csv.csv")

### Nos quedamos con las columnas de Continent_name y Three_Letter_Country_Code
iso.countries<-iso.countries[,c("Continent_Name","Three_Letter_Country_Code")]
colnames(iso.countries)<-c("Continent_Name","iso_code")
### Agregamos a South America y a North America como America
iso.countries$Continent_Name<-as.character(iso.countries$Continent_Name)
iso.countries$Continent_Name[iso.countries$Continent_Name=="South America"]<-"America"
iso.countries$Continent_Name[iso.countries$Continent_Name=="North America"]<-"America"

### Hacemos un right join para agregar el continente al que pertenece el país
covid<-merge(covid,iso.countries,by="iso_code",all.y=TRUE)
covid<-subset(covid, covid$location!="International")

### Nos quedamos con las columnas necesarias 
covid_plot<-covid[c("date","Continent_Name","total_cases")]

### Generamos un arreglo de las fechas de los registros
fechas<-as.character(unique(covid_plot$date))
fechas<-as.Date(fechas,format = "%Y-%m-%d")
fechas<-fechas[order(fechas)]

#### Ahora haremos el gif

library(animation)
library(lubridate)
library(XML)
library(mosaic)

saveGIF({
  for (i in as.character(fechas)) {
    ## Nos quedamos con los casos totales de un día determinado
    covid_plot_sub<-subset(covid_plot,covid_plot$date==as.character(i))
    ## Escalamos los datos por continente
    df_scaled<-covid_plot_sub%>% group_by(Continent_Name)%>% mutate(total_cases=scale(total_cases, center = TRUE, scale = TRUE))
    p<-ggplot(df_scaled,aes(y=as.factor(Continent_Name))) +
      geom_density_ridges(aes(x=total_cases,fill=Continent_Name), alpha = .6, color = "white", show.legend = FALSE) +
      scale_y_discrete(expand = c(0.01, 0)) +
      scale_x_continuous(expand = c(0.01, 0)) +
      theme_ridges(grid = FALSE)
    fecha=as.character(i)
    print(p+labs( 
      title= "Distribuciones de casos totales confirmados de COVID-19 por continente", 
      subtitle=fecha,
      x="Casos totales confirmados normalizados",
      y="",
      caption = "Fuente:Our World in Data. Liga: https://covid.ourworldindata.org/data/owid-covid-data.csv"))
  
  }
  
},
interval=.1, ani.width = 800, ani.height = 400)
