library(readxl)
library(tidyverse)


# This is excell with the participant of the survey
cases <- read_excel("database/cases.xlsx")
cases <-  cases %>% 
  rename(id ="...2",
         job = `Actualmente en relación a tu situación laboral te encuentras`) %>% 
  drop_na(job)


# information patient from Pharmacy to get demographics
pharmacy <- read_excel("database/info_pat_pharmacy.xlsx")
pharmacy <- pharmacy %>% 
  rename(id =nFicha)
pharmacy <- pharmacy %>% 
  select(id, sex=Sexo, Fecha_nacimiento, Prevision, Region, Comuna, Nacionalidad, Inicio_TAR) %>% 
  mutate(Nacionalidad =ifelse(id==8084,'Venezolana', Nacionalidad))


# Viral load
cv <- read_excel('database/newest_test copy.xlsx')
cv <- cv %>%
  rename(id= ID_FA) %>% 
  group_by(id) %>%
  slice_max(VL) %>%
  slice_max(test_date) %>% 
  #filter(test_date == max(test_date)) %>% 
  ungroup

cv <- cv %>% select(id, VL, test_date)


# Education 
education  <- read_excel("database/education.xlsx")
education <- education %>%
  rename(id= Id) %>% 
  group_by(id) %>%
  slice_max(fecha) %>% 
  #filter(test_date == max(test_date)) %>% 
  ungroup
education <- education %>% select(id, education =nivel)


## Joint all dataset
df <- cases %>% 
  left_join(pharmacy, by = 'id')

df <- df %>% 
  left_join(cv, by = 'id')

df <- df %>% 
  left_join(education, by = 'id')

#remove
rm(cases, cv,education, pharmacy)

# Arrange demographics
df <- df %>% 
  # Exlude some individual
   #  filter(
   #         job != 'Trabajo no remunerado (ej. Cuidado de familiares, dueña de casa).',
   #         job != 'Soy estudiante.',
   #         job != 'Soy jubilado/pensionado.'
   # ) %>% 
  # drop_na(VL) %>% 
  mutate(age = round(lubridate::time_length(index_date - Fecha_nacimiento, "years"),0),
         VL.dectable = ifelse(VL >= 50,1,0),
         sex = as.factor(ifelse(sex == 'Hombre', 'men', 'women')),
         # Here exposure is modifiable
         exposure=
           as.factor(
             case_when(
               job == 'No tengo trabajo/cesante.' ~ 0, 
               job == 'Soy estudiante.' ~ 0,
               job == 'Trabajo como independiente con boletas.' ~ 1,
               job == 'Soy estudiante y trabajo' ~  1,
               job == 'Soy jubilado/pensionado.' ~ 0,
               job == 'Trabajo como independiente sin boletas.' ~ 1,
               job == 'Trabajo con contrato.' ~ 1,
               job == 'Trabajo no remunerado (ej. Cuidado de familiares, dueña de casa).' ~ 0
             )),
         insurance=
           as.factor(
             case_when(
               Prevision == 'Dipreca' ~ "C/D",
               Prevision == 'Fonasa A' ~ "A/B",
               Prevision == 'Fonasa B' ~ "A/B",
               Prevision == 'Fonasa C' ~ "C/D",
               Prevision == 'Fonasa D' ~ "C/D", 
               Prevision == 'Isapre'    ~ "C/D",
               Prevision == 'Prais'  ~ "C/D",
               Prevision == 'Prais/A'  ~ "A/B",
               Prevision == 'Prais/B'  ~ "A/B",
               Prevision == 'Prais/C'  ~ "C/D",
               Prevision == 'Prais/D' ~ "C/D"
             )),
         comorbidities = ifelse(`¿Tienes actualmente alguna de las siguientes condiciones crónicas?...24` == 'Ninguna.', 0,1),
         migrant = ifelse(Nacionalidad == 'Chilena', 0,1),
         education.level=
           as.factor(
             case_when(
               education == 'basica completa' ~ "primary",
               education == 'basica incompleta' ~ "primary",
               education == 'basica sin dato' ~ "primary",
               education == 'media completa' ~ "secondary",
               education == 'media incompleta' ~ "secondary", 
               education == 'media sin dato'    ~ "secondary",
               education == 'sd sin dato'    ~ NA,
               education == 'sin dato sin dato'    ~ NA,
               education == 'tecnica completa'  ~ "Terciary",
               education == 'tecnica incompleta'  ~ "Terciary",
               education == 'tecnico incompleta'  ~ "Terciary",
               education == 'tecnica sin dato'  ~ "Terciary",
               education == 'tecnico sin dato'  ~ "Terciary",
               education == 'universitaria completa' ~ "Terciary",
               education == 'universitaria incompleta' ~ "Terciary",
               education == 'universitaria sin dato' ~ "Terciary"
             )),
         timeOnART = (round(lubridate::time_length(index_date - Inicio_TAR, "years"),1)),
         stgo = ifelse(Region == 'Region Metropolitana de Santiago',1,0),
         housing = ifelse(`En relación a dónde vives` == 'Vivienda propia (arrendatario o propietario).',1,0),
         
         # These extra questions
         # Self stigma
         self.stigma1 = ifelse(
           `¿Qué tan de acuerdo estás con la siguiente frase “Yo tengo mucho cuidado al escoger a quien le cuento que tengo VIH”?` == 'Completamente de acuerdo.' | 
             `¿Qué tan de acuerdo estás con la siguiente frase “Yo tengo mucho cuidado al escoger a quien le cuento que tengo VIH”?` == 'De acuerdo.', 
           1,0),
         
         self.stigma2 = ifelse(
           `¿Qué tan de acuerdo estás con la siguiente frase “Yo siento que no soy tan bueno como otras personas porque tengo VIH”?` == 'Completamente de acuerdo.' | 
             `¿Qué tan de acuerdo estás con la siguiente frase “Yo siento que no soy tan bueno como otras personas porque tengo VIH”?` == 'De acuerdo.', 
           1,0),
         
         self.stigma3 = ifelse(
           `¿Qué tan de acuerdo estás con la siguiente frase “Mucha gente con VIH es rechazada cuando otros se enteran sobre su enfermedad”?` == 'Completamente de acuerdo.' | 
             `¿Qué tan de acuerdo estás con la siguiente frase “Mucha gente con VIH es rechazada cuando otros se enteran sobre su enfermedad”?` == 'De acuerdo.', 
           1,0),
         
         self.stigma = ifelse(self.stigma1 == 0 & self.stigma2 == 0 & self.stigma3 == 0,0,1 ),
         
         # Consumption of alcohol or drugs
         addiction1 = ifelse(`En el último año, ¿qué tan frecuentemente usaste algún tipo de droga  o alcohol para cambiar la forma en que te sentías?` == 'Nunca.', 0,1),
         addiction2 = ifelse(`En el último año, ¿qué tan frecuentemente usaste más drogas o bebiste más alcohol del que habías planeado?` == 'Nunca.', 0,1),
         addiction3 = ifelse(`En el último año, ¿qué tan frecuentemente sentiste que necesitabas o querías dejar de consumir drogas o alcohol y  que no fuiste capaz?` == 'Nunca.', 0,1),
         
         addiction = ifelse(addiction1 == 0 & addiction2 == 0 & addiction1 == 0, 0, 1),
         # Mental health
         mental.health1 = ifelse(
           `Durante las últimas dos semanas; ¿Qué tan a menudo has sentido poco interés o placer para hacer las cosas?` == 'Ningún día.', 
           0,1),
         mental.health2 = ifelse(
           `Durante las últimas dos semanas; ¿Qué tan a menudo te has sentido deprimido, irritado/enojado o sin esperanza?` == 'Ningún día.', 
           0,1),
         mental.health = ifelse(mental.health1 == 0 & mental.health2 == 0, 0, 1)
         ) %>% 
  select(index_date, id, sex, age, VL, VL.dectable, exposure, insurance, comorbidities, migrant, education.level, timeOnART, stgo,
         housing,self.stigma,addiction,mental.health  )

