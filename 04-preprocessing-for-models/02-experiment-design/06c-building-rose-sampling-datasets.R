#install.packages("curl")
#install.packages("magick")
#install.packages("lubridate")
#install.packages("summarytools")
#install.packages("data.table")
#install.packages("readr")
#install.packages("ROSE")
#install.packages("arrow")
#install.packages("h2o")


setwd("~/Dropbox/repos/super-srag-ai-covid19-google-cidacs")



library(lubridate)
library(dplyr)
library(data.table)
library(summarytools)
library(readr)
library(ROSE)
st_options(use.x11 = FALSE)
library(arrow)
# library(h2o)

#h2o.init(port = 54321, nthreads = 15, max_mem_size = '50g')

# nofilter covid
df <- read.csv('../0_global_data/super_srag_effort/trusted/super-srag_filters/super-srag_covid_nofilter.csv', header = TRUE)
not_covid <- read.csv('../0_global_data/super_srag_effort/trusted/super-srag_filters/super-srag_notcovid_nofilter.csv', header = TRUE)

df = bind_rows(df, not_covid)

names(df)


df_vars = df %>% select(NU_NOTIFIC,CS_SEXO,DT_NASC,SEM_PRI,DT_SIN_PRI,CLASSI_FIN,EVOLUCAO,
                        SYMP_GROUP1,SYMP_GROUP2,CO_MUN_NOT,AGE_AT_NOTIF,AGE_GROUP,CS_RACA,SG_UF_NOT,CS_RACA,CS_ESCOL_N,EPI_WEEK_YEAR,
                        GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                        GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG)
glimpse(df_vars)


#remove NA da variável resposta/ no sexo e no sintomas
df_vars_v3 = df_vars %>% filter(!is.na(CLASSI_FIN))
df_vars_v3 = df_vars_v3 %>% filter(!is.na(CS_SEXO))
df_vars_v3 = df_vars_v3 %>% mutate(CS_ESCOL_N =  if_else(CS_ESCOL_N %in% c(9,5) ,NA_integer_,as.integer(CS_ESCOL_N)  )   )

# freq(df_vars_v3$CS_RACA,order = 'freq')

df_vars_v3 = df_vars_v3 %>% mutate(CS_RACA =  if_else(CS_RACA == 9,NA_integer_, as.integer(CS_RACA)   )  )


#variável resposta para factor
df_vars_v4 = df_vars_v3 %>% mutate(CLASSI_FIN = as.factor(CLASSI_FIN),
                                   CS_SEXO = as.factor(CS_SEXO),
                                   CS_RACA = as.factor(CS_RACA),
                                   SYMP_GROUP1 = as.factor(SYMP_GROUP1),
                                   SYMP_GROUP2 = as.factor(SYMP_GROUP2),
                                   SG_UF_NOT = as.factor(SG_UF_NOT),
                                   CS_RACA = as.factor(CS_RACA),
                                   CS_ESCOL_N = as.factor(CS_ESCOL_N),
                                   AGE_GROUP = as.factor(AGE_GROUP),
)

glimpse(df_vars_v4)

df_vars_v4 = df_vars_v4 %>% select(CLASSI_FIN,SYMP_GROUP1,SYMP_GROUP2,AGE_GROUP,CS_SEXO,SG_UF_NOT,CS_RACA,CS_ESCOL_N,
                                   GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                   GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG,NU_NOTIFIC) #SG_UF_NOT


teste_lista = list()
# gerar amostras
for( var in 0:49 ){
  print(var)
  a <- ovun.sample(CLASSI_FIN~., data=df_vars_v4, 
                   seed=var, method="under", N = 60000)$data
  a <- a %>% select(NU_NOTIFIC, CLASSI_FIN) %>% filter(CLASSI_FIN == 1)
  nrow(a)
  teste_lista = c(teste_lista,list(a))
  write_csv(a,paste("../0_global_data/super_srag_effort/ds-1/base_rose",var,".csv",sep = "_") )}







# pcrpositive covid
df <- read.csv('../0_global_data/super_srag_effort/trusted/super-srag_filters/super-srag_covid_pcrpositive.csv', header = TRUE) %>% select(NU_NOTIFIC,CS_SEXO,DT_NASC,SEM_PRI,DT_SIN_PRI,CLASSI_FIN,EVOLUCAO,
                                                                                                                                           SYMP_GROUP1,SYMP_GROUP2,CO_MUN_NOT,AGE_AT_NOTIF,AGE_GROUP,CS_RACA,SG_UF_NOT,CS_RACA,CS_ESCOL_N,EPI_WEEK_YEAR,
                                                                                                                                           GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                                                                                                                           GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG)
#df <- df %>% select(-c(CLASSI_OUT, OUT_MORBI))
#df <- df %>% select(-c(RAIOX_RES))

not_covid <- read.csv('../0_global_data/super_srag_effort/trusted/super-srag_filters/super-srag_notcovid_nofilter.csv', header = TRUE) %>% select(NU_NOTIFIC,CS_SEXO,DT_NASC,SEM_PRI,DT_SIN_PRI,CLASSI_FIN,EVOLUCAO,
                                                                                                                                                  SYMP_GROUP1,SYMP_GROUP2,CO_MUN_NOT,AGE_AT_NOTIF,AGE_GROUP,CS_RACA,SG_UF_NOT,CS_RACA,CS_ESCOL_N,EPI_WEEK_YEAR,
                                                                                                                                                  GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                                                                                                                                  GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG)
#not_covid <- not_covid %>% select(-c(CLASSI_OUT, OUT_MORBI))
#not_covid <- not_covid %>% select(-c(RAIOX_RES))

df_vars = bind_rows(df, not_covid)

df_vars %>% group_by(CLASSI_FIN) %>% count()


names(df_vars)

glimpse(df_vars)


#remove NA da variável resposta/ no sexo e no sintomas
df_vars_v3 = df_vars %>% filter(!is.na(CLASSI_FIN))
df_vars_v3 = df_vars_v3 %>% filter(!is.na(CS_SEXO))
df_vars_v3 = df_vars_v3 %>% mutate(CS_ESCOL_N =  if_else(CS_ESCOL_N %in% c(9,5) ,NA_integer_,as.integer(CS_ESCOL_N)  )   )

# freq(df_vars_v3$CS_RACA,order = 'freq')

df_vars_v3 = df_vars_v3 %>% mutate(CS_RACA =  if_else(CS_RACA == 9,NA_integer_, as.integer(CS_RACA)   )  )


#variável resposta para factor
df_vars_v4 = df_vars_v3 %>% mutate(CLASSI_FIN = as.factor(CLASSI_FIN),
                                   CS_SEXO = as.factor(CS_SEXO),
                                   CS_RACA = as.factor(CS_RACA),
                                   SYMP_GROUP1 = as.factor(SYMP_GROUP1),
                                   SYMP_GROUP2 = as.factor(SYMP_GROUP2),
                                   SG_UF_NOT = as.factor(SG_UF_NOT),
                                   CS_RACA = as.factor(CS_RACA),
                                   CS_ESCOL_N = as.factor(CS_ESCOL_N),
                                   AGE_GROUP = as.factor(AGE_GROUP),
)

glimpse(df_vars_v4)

df_vars_v4 = df_vars_v4 %>% select(CLASSI_FIN,SYMP_GROUP1,SYMP_GROUP2,AGE_GROUP,CS_SEXO,SG_UF_NOT,CS_RACA,CS_ESCOL_N,
                                   GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                   GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG,NU_NOTIFIC) #SG_UF_NOT


teste_lista = list()
# gerar amostras
for( var in 0:49 ){
  print(var)
  a <- ovun.sample(CLASSI_FIN~., data=df_vars_v4, 
                   seed=var, method="under", N = 60000)$data
  a <- a %>% select(NU_NOTIFIC, CLASSI_FIN) %>% filter(CLASSI_FIN == 1)
  nrow(a)
  teste_lista = c(teste_lista,list(a))
  write_csv(a,paste("../0_global_data/super_srag_effort/ds-2/base_rose",var,".csv",sep = "_") )}








####### LAB POSITIVE FILTER = DS-3

df <- read.csv('../0_global_data/super_srag_effort/trusted/super-srag_filters/super-srag_covid_labpositive.csv', header = TRUE) %>% select(NU_NOTIFIC,CS_SEXO,DT_NASC,SEM_PRI,DT_SIN_PRI,CLASSI_FIN,EVOLUCAO,
                                                                                                                                           SYMP_GROUP1,SYMP_GROUP2,CO_MUN_NOT,AGE_AT_NOTIF,AGE_GROUP,CS_RACA,SG_UF_NOT,CS_RACA,CS_ESCOL_N,EPI_WEEK_YEAR,
                                                                                                                                           GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                                                                                                                           GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG)


not_covid <- read.csv('../0_global_data/super_srag_effort/trusted/super-srag_filters/super-srag_notcovid_nofilter.csv', header = TRUE) %>% select(NU_NOTIFIC,CS_SEXO,DT_NASC,SEM_PRI,DT_SIN_PRI,CLASSI_FIN,EVOLUCAO,
                                                                                                                                                  SYMP_GROUP1,SYMP_GROUP2,CO_MUN_NOT,AGE_AT_NOTIF,AGE_GROUP,CS_RACA,SG_UF_NOT,CS_RACA,CS_ESCOL_N,EPI_WEEK_YEAR,
                                                                                                                                                  GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                                                                                                                                  GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG)


df_vars = bind_rows(df, not_covid)

df_vars %>% group_by(CLASSI_FIN) %>% count()


names(df_vars)

glimpse(df_vars)


#remove NA da variável resposta/ no sexo e no sintomas
df_vars_v3 = df_vars %>% filter(!is.na(CLASSI_FIN))
df_vars_v3 = df_vars_v3 %>% filter(!is.na(CS_SEXO))
df_vars_v3 = df_vars_v3 %>% mutate(CS_ESCOL_N =  if_else(CS_ESCOL_N %in% c(9,5) ,NA_integer_,as.integer(CS_ESCOL_N)  )   )

# freq(df_vars_v3$CS_RACA,order = 'freq')

df_vars_v3 = df_vars_v3 %>% mutate(CS_RACA =  if_else(CS_RACA == 9,NA_integer_, as.integer(CS_RACA)   )  )


#variável resposta para factor
df_vars_v4 = df_vars_v3 %>% mutate(CLASSI_FIN = as.factor(CLASSI_FIN),
                                   CS_SEXO = as.factor(CS_SEXO),
                                   CS_RACA = as.factor(CS_RACA),
                                   SYMP_GROUP1 = as.factor(SYMP_GROUP1),
                                   SYMP_GROUP2 = as.factor(SYMP_GROUP2),
                                   SG_UF_NOT = as.factor(SG_UF_NOT),
                                   CS_RACA = as.factor(CS_RACA),
                                   CS_ESCOL_N = as.factor(CS_ESCOL_N),
                                   AGE_GROUP = as.factor(AGE_GROUP),
)

glimpse(df_vars_v4)

df_vars_v4 = df_vars_v4 %>% select(CLASSI_FIN,SYMP_GROUP1,SYMP_GROUP2,AGE_GROUP,CS_SEXO,SG_UF_NOT,CS_RACA,CS_ESCOL_N,
                                   GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_RESIDENTIAL_PERCENT_AVG,GMR_RESIDENTIAL_PERCENT_1WEEK_BEFORE_AVG,GMR_GROCERY_AND_PHARMACY_2WEEKS_AVG,GMR_GROCERY_AND_PHARMACY_1WEEK_BEFORE_AVG,GMR_TRANSIT_STATIONS_AVG,
                                   GMR_TRANSIT_STATIONS_1WEEK_BEFORE_AVG,NU_NOTIFIC) #SG_UF_NOT


teste_lista = list()
# gerar amostras
for( var in 0:49 ){
  print(var)
  a <- ovun.sample(CLASSI_FIN~., data=df_vars_v4, 
                   seed=var, method="under", N = 60000)$data
  a <- a %>% select(NU_NOTIFIC, CLASSI_FIN) %>% filter(CLASSI_FIN == 1)
  nrow(a)
  teste_lista = c(teste_lista,list(a))
  write_csv(a,paste("../0_global_data/super_srag_effort/ds-3/base_rose",var,".csv",sep = "_") )}





