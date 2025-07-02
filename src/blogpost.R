# Interest Rate Pass-Through Analysis Blogpost
# Autor: Italo López
# Descripción: Este script nos da outputs adicionales para el blogpost
# Fecha: Marzo 2025

# Cargar bibliotecas necesarias
library(dplyr)         # Manipulación de datos
library(ggplot2)       # Gráficos (no utilizado directamente en este script)
library(zoo)           # Manejo de series temporales
library(plotly)        # Gráficos interactivos
library(tibble)        # Manejo de tibbles
library(tseries)       # Pruebas estadísticas
library(ROracle)       # Conexión a Oracle
library(keyring)       # Gestión de contraseñas de base de datos
library(urca)          # Pruebas de raíz unitaria (ADF, KPSS)
library(lubridate)     # Manipulación de fechas
library(gap)           # Funciones económicas
library(tidyr)         # Manipulación de datos
library(timetk)        # Análisis de series temporales
library(lmtest)        # Paquete para hacer tests robustos
library(sandwich)
library(car)
library(moments)



# Cargar funciones auxiliares desde un script externo
source("utils.R")

#Este script realiza un análisis del pass-through de los tipos de interés en República Dominicana

#Extraemos data, igual que en el cálculo ordinario de construcción del indicador


#Cargando data tasas de mercado pasivas
tasas_pasivas <- read.csv("data_bcrd_tasas_pasivas.csv")
tasas_pasivas <-tasas_pasivas%>%ts(start = c(2008,1),frequency = 12)
tasas_pasivas <-tasas_pasivas%>%as.zoo()



#Cargando data tasas de mercado activas
tasas_activas <- read.csv("data_bcrd_tasas_activas.csv")
tasas_activas <-tasas_activas%>%ts(start = c(2008,1),frequency = 12)
tasas_activas <-tasas_activas%>%as.zoo()


#Cargando TPM
tpm <- read.csv("data_bcrd_tasa_de_politica_monetaria.csv")
tpm <-tpm%>%ts(start = c(2008,1),frequency = 12)
tpm <-tpm%>%as.zoo()


#Uniendo las tasas activas con las pasivas 
tasas <- tasas_pasivas %>% 
  as.data.frame() %>% 
  full_join(tasas_activas %>% as.data.frame(), by = c("ANO", "MES")) %>%
  full_join(tpm %>% as.data.frame(), by = c("ANO", "MES"))

#Formateando el data frame como una serie de tiempo
tasas <-tasas%>%ts(start = c(2008,1),frequency = 12)
tasas <-tasas%>%as.zoo()




df <- data.frame(
  fecha = index(tasas),
  coredata(tasas)
)

#Creamos plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~TPM, name = "Tasa de política monetaria", line = list(color = 'blue')) %>%
  add_lines(y = ~TIAM_PLAZO_360, name = "Tasa activa de largo plazo (360 días)", line = list(color = 'red')) %>%
  add_lines(y = ~TIPM_PLAZO_360, name = "Tasa pasiva de largo plazo (360 días)", line = list(color = 'green')) %>%
  layout(
    title = list(
      text = "Evolución de las tasas",
      font = list(size = 24),
      y = 0.95
    ),
    xaxis = list(
      title = list(text = "Fecha", font = list(size = 22)),
      tickfont = list(size = 18),
      showgrid = TRUE
    ),
    yaxis = list(
      title = list(text = "Valor (puntos porcentuales)", font = list(size = 22)),
      tickfont = list(size = 18),
      showgrid = TRUE
    ),
    legend = list(
      font = list(size = 20),
      y = 0.95,
      x = 0.75
    )
  )

fig


#Estimando relacion de largo plazo con OLS tasa pasiva
model_tipm_360 <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA,data = tasas_pasivas)

summary(model_tipm_360)


#Estimando relacion de largo plazo con OLS tasa activa
model_tiam_360 <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA,data = tasas)

summary(model_tiam_360)



###########################Aquí plot de tasas con desequilibrio
ecm_term_pasiva <- long_run_results_function(model_tipm_360,tasas_pasivas,"Muestra completa (tasa pasiva)")


ecm_term_activa <- long_run_results_function(model_tiam_360,tasas,"Muestra total (tasa activa)")


ecm_term_pasiva <-ecm_term_pasiva%>%ts(start = c(2008,1),frequency = 12)
ecm_term_pasiva <-ecm_term_pasiva%>%as.zoo()

ecm_term_activa <-ecm_term_activa%>%ts(start = c(2008,1),frequency = 12)
ecm_term_activa <-ecm_term_activa%>%as.zoo()

desequilibrio_activa_y_pasiva <- merge(ecm_term_pasiva,ecm_term_activa)



df <- data.frame(
  fecha = index(desequilibrio_activa_y_pasiva),
  coredata(desequilibrio_activa_y_pasiva)
)

# Create the plot
fig <- plot_ly(df, x = ~fecha) %>%
  add_lines(y = ~ecm_term_activa, name = "Desequilibrio de largo plazo (activa)", line = list(color = 'blue')) %>%
  add_lines(y = ~ecm_term_pasiva, name = "Desequilibrio de largo plazo (pasiva)", line = list(color = 'red')) %>%
  layout(
    title = list(
      text = "Desequilibrio de las tasas del mercado",
      font = list(size = 24),
      y = 0.95  # Optional: lowers the title slightly
    ),
    xaxis = list(
      title = list(text = "Fecha", font = list(size = 22)),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = list(text = "Valor (puntos porcentuales)", font = list(size = 22)),
      tickfont = list(size = 20)
    ),
    legend = list(
      font = list(size = 20),
      y = 0.95,
      x = 0.75# Optional: lowers the title slightly
    )
  )

fig


#Tomamos la data pre-COVID
pre_covid_data <- tasas_pasivas[index(tasas_pasivas)<as.yearmon(as.Date("2020-03-01")),]

#Estimamos el equilibrion de largo plazo para la submuestra pre-COVID
model_tipm_360_pre_covid <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA,data = pre_covid_data)

#Output del modelo
summary(model_tipm_360_pre_covid)



#Tomamos la data post-COVID
post_covid_data <- tasas_pasivas[index(tasas_pasivas)>=as.yearmon(as.Date("2020-03-01")),]

#Estimamos el equilibrion de largo plazo para la submuestra post-COVID
model_tipm_360_post_covid <- lm(TIPM_PLAZO_360~TIPM_INTERBANCARIA,data = post_covid_data)

#Output del modelo
summary(model_tipm_360_post_covid)



#Tomamos data pre-COVID
pre_covid_data_activa <- tasas[index(tasas)<as.yearmon(as.Date("2020-03-01")),]

#EStimamos equilibrio de largo plazo
model_tiam_360_pre_covid <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA,data = pre_covid_data_activa)

#Output del modelo
summary(model_tiam_360_pre_covid)


#Tomamos la data post-COVID
post_covid_data_activa <- tasas[index(tasas)>=as.yearmon(as.Date("2020-03-01")),]

#Estimamos el equilibrion de largo plazo para la submuestra post-COVID para la tasa activa
model_tipm_360_post_covid_activa <- lm(TIAM_PLAZO_360~TIPM_INTERBANCARIA,data = post_covid_data_activa)

#Output del modelo
summary(model_tipm_360_post_covid_activa)





#ECM Model para la tasa pasiva pre covid
ecm_term <- long_run_results_function(model_tipm_360_pre_covid,pre_covid_data,"Muestra pre-COVID (tasa pasiva)")


# Create First Differences
dY <- diff(pre_covid_data$TIPM_PLAZO_360)
dX1 <- diff(pre_covid_data$TIPM_INTERBANCARIA)
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_pre_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_pre_covid)


# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_pre_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_pre_covid))

#####################
# ECM model para la tasa pasiva post covid

ecm_term_post_covid <- long_run_results_function(model_tipm_360_post_covid,post_covid_data,"Muestra post-COVID (tasa pasiva)")


dY <- diff(post_covid_data$TIPM_PLAZO_360)
dX1 <- diff(post_covid_data$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term_post_covid, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_post_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_post_covid)

# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_post_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_post_covid))



####ECM tasa activa pre-COVID
ecm_term <- long_run_results_function(model_tiam_360_pre_covid,pre_covid_data_activa,"Muestra pre-COVID (tasa activa)")


dY <- diff(pre_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(pre_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_activa_pre_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_activa_pre_covid)


# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_activa_pre_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_activa_pre_covid))



# ECM model para la tasa activa post covid
ecm_term <- long_run_results_function(model_tipm_360_post_covid_activa,post_covid_data_activa,"Muestra post-COVID (tasa activa)")


dY <- diff(post_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(post_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

optimal_model_activa_post_covid <- optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(optimal_model_activa_post_covid)



# Autocorrelation (Ljung-Box test)
Box.test(residuals(optimal_model_activa_post_covid), lag = 10, type = "Ljung-Box")
#Normality test
jarque.bera.test(residuals(optimal_model_activa_post_covid))


##############Ahora hacemos el test de cointegracion asimetrica


#Primero tasa pasiva
#Muestra pre-COVID
#Extraemos desequilibrio de largo plazo en el periodo pre-COVID
resid_pre_pasiva <- residuals(model_tipm_360_pre_covid)

tar_model <- tar_model_estimation(resid_pre_pasiva,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")



#Muestra post_COVID
resid_post_pasiva <- residuals(model_tipm_360_post_covid)

tar_model <- tar_model_estimation(resid_post_pasiva,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")



#Ahora tasa activa
#Muestra pre-COVID
#Extraemos desequilibrio de largo plazo en el periodo pre-COVID
resid_pre_activa <- residuals(model_tiam_360_pre_covid)

tar_model <- tar_model_estimation(resid_pre_activa,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")



#Muestra post_COVID
resid_post_activa <- residuals(model_tipm_360_post_covid_activa)

tar_model <- tar_model_estimation(resid_post_activa,p_max = 3)

# Check the model summary
summary(tar_model)

####Now we test for cointegration
# Hypotheses: no structural break (interactions = 0)
coefs_to_test <- c("ECT_pos", "ECT_neg")

# Wald test with robust standard errors
linearHypothesis(tar_model, coefs_to_test)



linearHypothesis(tar_model, "ECT_pos = ECT_neg")



#Tasa pasiva pre-COVID

#TAR-ECM Model para la tasa pasiva pre covid
ecm_term <- long_run_results_function(model_tipm_360_pre_covid,pre_covid_data,"Muestra pre-COVID (tasa pasiva)")


# Create First Differences
dY <- diff(pre_covid_data$TIPM_PLAZO_360)
dX1 <- diff(pre_covid_data$TIPM_INTERBANCARIA)
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_pre_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_pre_covid)



#Tasa pasiva post-COVID

#TAR-ECM Model para la tasa pasiva post covid

ecm_term_post_covid <- long_run_results_function(model_tipm_360_post_covid,post_covid_data,"Muestra post-COVID (tasa pasiva)")


dY <- diff(post_covid_data$TIPM_PLAZO_360)
dX1 <- diff(post_covid_data$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term_post_covid, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_post_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_post_covid)



####ECM tasa activa pre-COVID
ecm_term <- long_run_results_function(model_tiam_360_pre_covid,pre_covid_data_activa,"Muestra pre-COVID (tasa activa)")


dY <- diff(pre_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(pre_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2008,1),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_activa_post_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_activa_post_covid)



#TAR-ECM Model para la tasa activa post-COVID
ecm_term <- long_run_results_function(model_tipm_360_post_covid_activa,post_covid_data_activa,"Muestra post-COVID (tasa activa)")


dY <- diff(post_covid_data_activa$TIAM_PLAZO_360)
dX1 <- diff(post_covid_data_activa$TIPM_INTERBANCARIA)
lag_dY <- lag_vec(dY, 1)    # Lagged first difference of Y
lag_dX1 <- lag_vec(dX1, 1)  # Lagged first difference of X2
lag_resid <- lag_vec(ecm_term, 1)

lag_resid <-lag_resid%>%ts(start = c(2020,3),frequency = 12)
lag_resid <-lag_resid%>%as.zoo()

data_model <- merge(dY,dX1,lag_dY,lag_dX1,lag_resid)

data_model <- data_model[data_model%>%complete.cases(),]

tar_optimal_model_activa_post_covid <- tar_optimal_short_run_model_function(data_model, data_model$lag_resid,data_model$dY,data_model$dX1) 

summary(tar_optimal_model_activa_post_covid)



