#Laster ned pakker som trengs
library(tidyverse)
library(dplyr)
library(jtools)
library(sjPlot)
library(vtable)
library(car)
library(knitr)
library(psych)
library(Hmisc)
library(huxtable)
library(broom.mixed)
library(stargazer)


# Deskriptiv statistikk

data_2020 <- read_csv("NSD2935_2020.csv") #laser ned data

df2 <- subset(data_2020, select = c("H80", "H81","alder","H13","saminnt", 
                                    "A1a", "H84", "bu_nus2000_niva")) #velger varabler 

df2 <- df2 %>%  #fjerner data som ikke er relevante
  filter(if_any(starts_with('H80'), ~ . < 8)) %>%
  filter(if_any(starts_with('H81'), ~ . < 8)) %>%
  filter(if_any(starts_with('H13'), ~ . < 8)) %>%
  filter(if_any(starts_with('A1a'), ~ . < 11)) %>%
  filter(if_any(starts_with('H84'), ~ . < 8)) %>%
  filter(if_any(starts_with('bu_nus2000_niva'), ~ . < 4)) %>%
  filter(if_any(starts_with('saminnt'), ~ . > -1)) 


#Gir variablene navn som gir mening. 
labs <- c("Stoy", "Forurensing","Alder","Helse","Inntekt", "Lykke", "Natur","Utdanning")

#Lagr tabellen over deskriptiv statisikk
st(df2, labels=labs)
st(df2, vars =c("stoy", "forurensing","alder","helse","inntekt","bor", "lykke","Fylke", "Natur"))
st(df2, labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Dette beskriver hvilken informasjon du vil ha med i tabellen
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') #Dette setter titler til kolumnene.
   ))



# Aalyse

data_2020 <- read_csv("NSD2935_2020.csv")

data <- data_2020 %>% #fjerner data som ikke er relevante
  filter(if_any(starts_with('H80'), ~ . < 8)) %>%
  filter(if_any(starts_with('H81'), ~ . < 8)) %>%
  filter(if_any(starts_with('H13'), ~ . < 8)) %>%
  filter(if_any(starts_with('A1a'), ~ . < 11)) %>%
  filter(if_any(starts_with('H84'), ~ . < 8)) %>%
  filter(if_any(starts_with('bu_nus2000_niva'), ~ . < 4)) %>%
  filter(if_any(starts_with('saminnt'), ~ . > -1)) %>% 
  rename("utdnivaa"="bu_nus2000_niva")


#Lager dummy-variabler for de variablene som trenger det og setter inntekt til 10000
data_1 <- data %>% 
  mutate(dummy_ja=H81, dummy_nei=H81) %>% 
  mutate(dummy_ja_stoy=H80, dummy_nei_stoy=H80) %>% 
  mutate(saminnt = saminnt/100000) %>% 
  mutate(dummy_ja_nat = H84, dummy_nei_nat = H84)


#data_1 <- data %>% 
  #mutate(dummy_ja=H81, dummy_nei=H81) %>% 
  #mutate(dummy_ja_stoy=H80, dummy_nei_stoy=H80) %>% 
  #mutate(saminnt =log(saminnt)) %>% 
  #filter(saminnt > -1)

data_1$dummy_ja[data_1$dummy_ja>1] <- 0
data_1$dummy_nei[data_1$dummy_nei==1] <- 0  
data_1$dummy_nei[data_1$dummy_nei==2] <- 1

data_1$dummy_ja_stoy[data_1$dummy_ja_stoy>1] <- 0
data_1$dummy_nei_stoy[data_1$dummy_nei_stoy==1] <- 0  
data_1$dummy_nei_stoy[data_1$dummy_nei_stoy==2] <- 1

data_1$dummy_ja_nat[data_1$dummy_ja_nat>1] <- 0
data_1$dummy_nei_nat[data_1$dummy_nei_nat==1] <- 0  
data_1$dummy_nei_nat[data_1$dummy_nei_nat==2] <- 1


#Lager første analyse, tester bare for fourensing
labs1 <- c("Konstant", "Opplever forurensing")
model1 <- lm(A1a~ dummy_ja, data_1)
tab_model(model1, p.style = "stars", pred.labels =labs1, dv.labels = "Modell 1", title = "Forurensings effekt på livstilfredsheten", file = "plot3.html")
stargazer(model1, title="Results", out = "modell.html")

#Lager andre analyse, inkluderer testvariabler 
labs2 <- c("Konstant", "Opplever forurensing","Helse","Inntekt", "Utdanning", "Alder")
model2 <- lm(A1a~ dummy_ja + H13 + saminnt + utdnivaa + alder, data_1)
tab_model(model2, p.style = "stars", pred.labels =labs2, dv.labels = "Modell 2",title = "Forurensings effekt på livstilfredsheten")

#Lager tredje analyse, inkluderer andre miljøvariabler 
labs3 <- c("Konstant", "Opplever forurensing","Helse","Inntekt", "Utdanning", "Alder", "Opplever stoy", "Nærhet til natur")
model3 <- lm(A1a~ dummy_ja + H13 + saminnt + utdnivaa + alder + dummy_ja_stoy + dummy_ja_nat ,data=data_1)
tab_model(model3, p.style = "stars", pred.labels =labs3, dv.labels = "Modell 3" ,title = "Forurensings effekt på livstilfredsheten")

#setter modellene sammen 
tab_model(model1, model2, model3, p.style = "stars", pred.labels =labs3, dv.labels = c("Modell 1","Modell 2","Modell 3") ,show.ci = FALSE, title = "Forurensings effekt på livstilfredsheten", string.est="Estimert", digits.rsq=4, string.pred="Variabler")




#gjør samme som over bra med data fra 2021
data_2021 <- read_csv("NSD2995_2021.csv")

data2 <- data_2021 %>% #fjerner data som ikke er relevante
  filter(if_any(starts_with('H80'), ~ . < 8)) %>%
  filter(if_any(starts_with('H81'), ~ . < 8)) %>%
  filter(if_any(starts_with('H13'), ~ . < 8)) %>%
  filter(if_any(starts_with('A1a'), ~ . < 11)) %>%
  filter(if_any(starts_with('H84'), ~ . < 8)) %>%
  filter(if_any(starts_with('bu_nus2000_niva'), ~ . < 4)) %>%
  filter(if_any(starts_with('saminnt'), ~ . > -1)) %>% 
  filter(if_any(starts_with('utdnivaa'), ~ . < 9))

#Lager dummy-variabler for de variablene som trenger det og setter inntekt til 10000
data_2 <- data2 %>% 
  mutate(dummy_ja=H81, dummy_nei=H81) %>% 
  mutate(dummy_ja_stoy=H80, dummy_nei_stoy=H80) %>% 
  mutate(saminnt =(saminnt/100000)) %>% 
  mutate(dummy_ja_nat = H84, dummy_nei_nat = H84)

#data_1 <- data %>% 
#mutate(dummy_ja=H81, dummy_nei=H81) %>% 
#mutate(dummy_ja_stoy=H80, dummy_nei_stoy=H80) %>% 
#mutate(saminnt =log(saminnt)) %>% 
#filter(saminnt > -1)

data_2$dummy_ja[data_2$dummy_ja>1] <- 0
data_2$dummy_nei[data_2$dummy_nei==1] <- 0  
data_2$dummy_nei[data_2$dummy_nei==2] <- 1

data_2$dummy_ja_stoy[data_2$dummy_ja_stoy>1] <- 0
data_2$dummy_nei_stoy[data_2$dummy_nei_stoy==1] <- 0  
data_2$dummy_nei_stoy[data_2$dummy_nei_stoy==2] <- 1

data_2$dummy_ja_nat[data_2$dummy_ja_nat>1] <- 0
data_2$dummy_nei_nat[data_2$dummy_nei_nat==1] <- 0  
data_2$dummy_nei_nat[data_2$dummy_nei_nat==2] <- 1

#lager analysen for 2021
labs3 <- c("Konstant", "Opplever forurensing","Helse","Inntekt", "Utdanning", "Alder", "Opplever stoy", "Nærhet til natur")
model4 <- lm(A1a~ dummy_ja + H13 + saminnt + utdnivaa + alder + dummy_ja_stoy + dummy_ja_nat ,data=data_2)
tab_model(model4, p.style = "stars", pred.labels =labs3, dv.labels = "Model 4" ,title = "Forurensings effekt på livstilfredsheten")

#leger sammen modell fra 2020 og 2021
tab_model(model3,model4, dv.labels = c("Modell 2020", "Modell 2021"), p.style = "stars", pred.labels =labs3, title = "Forurensings effekt på livstilfredsheten", show.ci = FALSE, string.est="Estimert", string.pred="Variabler", digits.rsq=4)


plot_summs(model3,model4, model.names = c("Model 2020", "Model 2021"))



