#------------------------------------------------------------------------------#
#           APLICACIÓN PARA DETERMINAR AL EVAPOTRANSPIRACIÓN POTENCIAL         #
#------------------------------------------------------------------------------#

library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(writexl)

# CONSTANTES
# m
m <- 8.03E6
# parámetrod de ángulo de declinación solar
b1 = 3999.12; b2 = 702.57; b3 = 67.58; b4 = 9.08
# parámetros de (dm/d)**2
C1 = 0.033523; C2 = 0.00128; C3 = 0.000739; C4 = 0.000099

# ECUACIONES POR REGIÓN
ecuaciones <- list(
  "costa norte" = list(
    Rnol = Vectorize(function(x) -0.111+0.255*x), # x=dT
    Ea = Vectorize(function(x) -5.8423+0.461*x), # x=Tmax
    Qi = Vectorize(function(x,y,z) x*(0.36+0.211*y/z)) # x=Qs, y=dT, z=N 
  ),
  "costa central" = list(
    Rnol = Vectorize(function(x) -0.24+0.23*x), # x=dT
    Ea = Vectorize(function(x) -0.53+1.40*x), # x=Rnol
    Qi = Vectorize(function(x,y,z) x*(0.06+0.64*y/z)) # x=Qs, y=dT, z=N 
  ),
  "costa sur" = list(
    Rnol = Vectorize(function(x) -0.474+0.5134*x), # x=dT
    Ea = Vectorize(function(x) -3.01+2.82*x), # x=Rnol
    Qi = Vectorize(function(x,y,z) x*(0.23+0.38*y/z)) # x=Qs, y=dT, z=N 
  ),
  "sierra norte" = list(
    Rnol = Vectorize(function(x) -0.827+0.209*x), # x=dT
    Ea = Vectorize(function(x) -5.35+3.98*x), # x=Rnol
    Qi = Vectorize(function(x,y,z) x*(0.284+0.205*y/z)) # x=Qs, y=dT, z=N 
  ),
  "sierra central" = list(
    Rnol = Vectorize(function(x) -0.0971+0.188*x), # x=dT
    Ea = Vectorize(function(x) -0.066+0.74*x), # x=Rnol
    Qi = Vectorize(function(x,y,z) x*(0.457+0.207*y/z)) # x=Qs, y=dT, z=N
  ),
  "sierra sur" = list(
    Rnol = Vectorize(function(x) -4.74+0.5134*x), # x=dT
    Ea = Vectorize(function(x) -3.01+2.82*x), # x=Rnol
    Qi = Vectorize(function(x,y,z) x*(0.457+0.207*y/z)) # x=Qs, y=dT, z=N
  ),
  "selva" = list(
    Rnol = function(x,y) -1.252+2.5882*x/y, # x= dT; y=N
    Ea = function(x,y) -2.108+0.095*x+0.186*y, # x=Tmax, y=dT
    Qi = function(x,y,z) x*(0.27+0.38*y/z) # x=Qs, y=dT, z=N 
  )
)

# RECOMNEDACIONDE ZONA
zonaRecF <- function(lat, alt){
  if(alt > 1000){
    region = "sierra"
  } else {
    region = "selva/costa"
  }
  if(lat < -13){
    reglat = "sur"
  } else if(lat < -10){
    reglat = "central"
  } else{
    reglat = "norte"
  }
  return(paste(region, reglat))
}

leer_datos <- function(file){
  df <- read_excel(file, col_types = "text")
  df[, 2:3] <- lapply(df[, 2:3], as.numeric)
  df$fecha<- as.Date(as.numeric(df$fecha), origin = "1899-12-30")
  return(df)
}

# Importar datos
evapPM <- function(arch, alt, lat, alb, zon){
  # Argumentos:
  #   df: datos
  #   alt: altitud en m.s.n.m.
  #   lat: latitud en grados decimales
  #   alb: albedo (0 a 1)
  #   zon: zono en texto
  # library(readxl)
  df <- read_excel(arch)
  # Leer fechas
  fechas <- df$fecha
  # Temperaturas
  TMAX <- df$tmax
  TMIN <- df$tmin
  # Número de dia del año (día juliano)
  diaj <- yday(fechas)
  # Temperatura media
  tmean <- (TMAX + TMIN)/2
  # Rango de temperatura diurna
  deltaT = TMAX - TMIN
  # Latitud en radianes
  lat_rad = lat*pi/180
  # Presión de vapor de saturación
  es = 9.422E23*(tmean+273.15)**(-5.08)*exp(-6801.27/(tmean+273.15))
  # Presión atmosférica
  P = 1014.78*exp(-1.17E-4*alt)
  # Tetha
  tetha = (2*pi*diaj)/365
  # Ángulo de declinación solar
  dec_sol = (0.00692-b1*cos(tetha)+b2*sin(tetha)-b3*cos(2*tetha)+b4*sin(2*tetha))*1E-4
  # H
  H = acos(-tan(lat_rad)*tan(dec_sol))
  # Horas de insolación máxima
  N = 2*(180*H/pi)/15
  # (dm/d)**2
  dmd = 1.00011+C1*cos(tetha)+C2*sin(tetha)+C3*cos(2*tetha)+C4*sin(2*tetha)
  # Qs (MJ/m2.dia)
  Qs = 37.211*dmd*(H*sin(lat_rad)*sin(dec_sol)+cos(lat_rad)*cos(dec_sol)*sin(H))
  # Qi (MJ/m2.dia)
  if(zon == "selva"){
    Qi = ecuaciones[[zon]]$Qi(deltaT, N)
  } else {
    Qi = ecuaciones[[zon]]$Qi(Qs, deltaT, N)
  }
  # Rnoc (MJ/m2.dia)
  Rnoc = Qi*(1-alb)
  # Rnol (MJ/m2.dia)
  if(zon == "selva"){
    Rnol = ecuaciones[[zon]]$Rnol(deltaT, N)
  } else {
    Rnol = ecuaciones[[zon]]$Rnol(deltaT)
  }
  Rnol = Rnol/0.41 # mm/día a MJ/m2/día
  # Revisar valores negativos de Rnol
  nRnol = length(which(Rnol < 0))
  nRonlP = 100*nRnol/length(Rnol)
  textRnol <- paste0("Cantidad de datos con Rnol < 0 = ", nRnol, 
                     " (", sprintf("%.1f", nRonlP),"%)\n")
  # cat(textRnol)
  Rnol = ifelse(Rnol < 0, 0, Rnol)
  # Rn (mm/dia)
  Rn = 0.408*(Rnoc - Rnol) # MJ/m2/día a mm/dia
  # Ea (mm/día)
  if(zon == "costa norte"){
    Ea = ecuaciones[[zon]]$Ea(TMAX)
  } else if (zon == "selva") {
    Ea = ecuaciones[[zon]]$Ea(TMAX, deltaT)
  } else {
    Ea = ecuaciones[[zon]]$Ea(Rnol*0.408) # rnol de MJ/m2/día a mm/dia
  }
  # Revisar valores negativos de Ea
  nEa = length(which(Ea < 0))
  nEaP = 100*nEa/length(Ea)
  textEa <- paste0("Cantidad de datos con Ea < 0 = ", nEa,
                   " (", sprintf("%.1f", nEaP),"%)\n")
  # cat(textEa)
  Ea = ifelse(Ea < 0, 0, Ea)
  # Evapotranspiración (mm/dia)
  rsl1 = (m*es)/(P*(tmean+273.15)**2)
  ETo = (rsl1*Rn+Ea)/(rsl1+1)
  rsl <- list(resultados = data.frame(fecha = fechas, 
                                      tmax = TMAX,
                                      tmin = TMIN,
                                      Rnoc = Rnoc,
                                      Rnol = Rnol,
                                      Rn = Rn/0.408,
                                      Ea = Ea,
                                      ETo = ETo) %>% as_tibble(),
              textRnol = textRnol,
              textEa = textEa)
  return(rsl)
}

# Elementores de ploteo
xlabel <- list(title = NA)
ylabel <- list(title = "ETo (mm/día)")

