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



####Ahora calcularemos el numero de periodos para restablecer el equilibrio de largo plazo y su intervalo de confianza

#Primero la tasa pasiva
#Muestra pre-COVID

summary(optimal_model_pre_covid)

summary_model <- summary(optimal_model_pre_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_pasiva_pre_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_pre <- restoration_time_pasiva_pre_covid$distribution_periods



# Step 2: Compute histogram manually
hist_data_pre <- hist(data_pre, breaks = "FD", plot = FALSE)

rel_freqs <- hist_data_pre$counts / sum(hist_data_pre$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_pre$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre to include only values in valid bins
filtered_data_pre <- data_pre[sapply(data_pre, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre
dens_filtered_pre <- density(filtered_data_pre)

x_range <- range(dens_filtered_pre$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_pre <- restoration_time_pasiva_pre_covid$point_estimate 

# Step 5: Clip original data_pre to match density range for the histogram
hist_data_pre_clipped <- data_pre[data_pre >= x_range[1] & data_pre <= x_range[2]]

stats_text <- sprintf(
  "Media: %.2f\nMediana: %.2f\nDesv. Est.: %.2f\nAsimetría: %.2f\nCurtosis: %.2f",
  mean(hist_data_pre_clipped),
  median(hist_data_pre_clipped),
  sd(hist_data_pre_clipped),
  skewness(hist_data_pre_clipped),
  kurtosis(hist_data_pre_clipped)
)

hist_time_pre_pasiva <- plot_ly() %>%
  add_trace(
    x = ~hist_data_pre_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_pre$x,
    y = ~dens_filtered_pre$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_pre, vline_x_pre),
    y = c(0, max(dens_filtered_pre$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período pre-COVID (tasa pasiva)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Meses", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_pre$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18),
      range = c(0, 25)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20)),
    annotations = list(
      list(
        x = 1, y = 1,  # Top right corner in paper coordinates (relative to full plot)
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        text = stats_text,
        showarrow = FALSE,
        font = list(size = 20, family = "Arial", color = "black"),
        align = "left",
        bordercolor = "black",
        borderwidth = 1,
        borderpad = 10,
        bgcolor = "rgba(255,255,255,0.8)"
      )
    )
  )

hist_time_pre_pasiva


#Estadistica descriptiva de la simulacion para muestra pre-COVID y tasa pasiva
stats <- list(
  mean   = mean(hist_data_pre_clipped, na.rm = TRUE),
  sd     = sd(hist_data_pre_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_pre_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_pre_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_pre_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_pre_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_pre_clipped, na.rm = TRUE)
)

stats

#Muestra post-COVID

summary(optimal_model_post_covid)

summary_model <- summary(optimal_model_post_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_pasiva_post_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_post <- restoration_time_pasiva_post_covid$distribution_periods
# Step 2: Compute histogram manually
hist_data_post <- hist(data_post, breaks = "FD", plot = FALSE)
rel_freqs <- hist_data_post$counts / sum(hist_data_post$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_post$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_post to include only values in valid bins
filtered_data_post <- data_post[sapply(data_post, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_post
dens_filtered_post <- density(filtered_data_post)

x_range <- range(dens_filtered_post$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_post <- restoration_time_pasiva_post_covid$point_estimate 

# Step 5: Clip original data_post to match density range for the histogram
hist_data_post_clipped <- data_post[data_post >= x_range[1] & data_post <= x_range[2]]

stats_text <- sprintf(
  "Media: %.2f\nMediana: %.2f\nDesv. Est.: %.2f\nAsimetría: %.2f\nCurtosis: %.2f",
  mean(hist_data_post_clipped),
  median(hist_data_post_clipped),
  sd(hist_data_post_clipped),
  skewness(hist_data_post_clipped),
  kurtosis(hist_data_post_clipped)
)

hist_time_post_pasiva  <- plot_ly() %>%
  add_trace(
    x = ~hist_data_post_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_post$x,
    y = ~dens_filtered_post$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_post, vline_x_post),
    y = c(0, max(dens_filtered_post$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período post-COVID (tasa pasiva)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Meses", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_post$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18),
      range = c(0, 25)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20)),
    annotations = list(
      list(
        x = 1, y = 1,  # Top right corner in paper coordinates (relative to full plot)
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        text = stats_text,
        showarrow = FALSE,
        font = list(size = 20, family = "Arial", color = "black"),
        align = "left",
        bordercolor = "black",
        borderwidth = 1,
        borderpad = 10,
        bgcolor = "rgba(255,255,255,0.8)"
      )
    )
  )

hist_time_post_pasiva

# Combine vertically with shared x-axis
fig <- subplot(hist_time_pre_pasiva, hist_time_post_pasiva, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "",#Tiempo de Restablecimiento del Equilibrio (tasas pasivas)
         annotations = list(
           list(
             x = 0.5,
             y = 0.95,
             text = "Período Pre-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           ),
           list(
             x = 0.5,
             y = 0.45,
             text = "Período Post-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           )
         ))

fig


#Tasa activa

#Primero período pre-COVID
summary(optimal_model_activa_pre_covid)


summary_model <- summary(optimal_model_activa_pre_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_activa_pre_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)



data_pre_activa <- restoration_time_activa_pre_covid$distribution_periods
# Step 2: Compute histogram manually
hist_data_pre_activa <- hist(data_pre_activa, breaks = "FD", plot = FALSE)
rel_freqs <- hist_data_pre_activa$counts / sum(hist_data_pre_activa$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_pre_activa$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_pre_activa to include only values in valid bins
filtered_data_pre_activa <- data_pre_activa[sapply(data_pre_activa, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_pre_activa
dens_filtered_pre_activa <- density(filtered_data_pre_activa)

x_range <- range(dens_filtered_pre_activa$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_pre_activa <- restoration_time_activa_pre_covid$point_estimate 

# Step 5: Clip original data_pre_activa to match density range for the histogram
hist_data_pre_activa_clipped <- data_pre_activa[data_pre_activa >= x_range[1] & data_pre_activa <= x_range[2]]

stats_text <- sprintf(
  "Media: %.2f\nMediana: %.2f\nDesv. Est.: %.2f\nAsimetría: %.2f\nCurtosis: %.2f",
  mean(hist_data_pre_activa_clipped),
  median(hist_data_pre_activa_clipped),
  sd(hist_data_pre_activa_clipped),
  skewness(hist_data_pre_activa_clipped),
  kurtosis(hist_data_pre_activa_clipped)
)


hist_time_pre_activa <- plot_ly() %>%
  add_trace(
    x = ~hist_data_pre_activa_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_pre_activa$x,
    y = ~dens_filtered_pre_activa$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_pre_activa, vline_x_pre_activa),
    y = c(0, max(dens_filtered_pre_activa$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),  # now dashed
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período pre-COVID (tasa activa)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Meses", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_pre_activa$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20)),  # 👈 makes "Estimador puntual" larger
    annotations = list(
      list(
        x = 1, y = 1,  # Top right corner in paper coordinates (relative to full plot)
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        text = stats_text,
        showarrow = FALSE,
        font = list(size = 20, family = "Arial", color = "black"),
        align = "left",
        bordercolor = "black",
        borderwidth = 1,
        borderpad = 10,
        bgcolor = "rgba(255,255,255,0.8)"
      )
    )
  )

hist_time_pre_activa


#Estadistica descriptiva de la simulacion para muestra pre-COVID y tasa activa
stats <- list(
  mean   = mean(hist_data_pre_activa_clipped, na.rm = TRUE),
  sd     = sd(hist_data_pre_activa_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_pre_activa_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_pre_activa_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_pre_activa_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_pre_activa_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_pre_activa_clipped, na.rm = TRUE)
)


stats

#Período post-COVID
summary(optimal_model_activa_post_covid)


summary_model <- summary(optimal_model_activa_post_covid)

alpha_hat <- summary_model$coefficients["lag_resid","Estimate"]

se_alpha_hat <- summary_model$coefficients["lag_resid","Std. Error"]

restoration_time_activa_post_covid <- simulate_restoration_time_ci(alpha_hat,se_alpha_hat)




data_post_activa <- restoration_time_activa_post_covid$distribution_periods
# Step 2: Compute histogram manually
hist_data_post_activa <- hist(data_post_activa, breaks = "FD", plot = FALSE)
rel_freqs <- hist_data_post_activa$counts / sum(hist_data_post_activa$counts)

# Step 3: Identify bins with relative frequency >= 0.005 (0.5%)
valid_bins <- which(rel_freqs >= 0.005)

# Get the bin ranges that meet the criterion
bin_edges <- hist_data_post_activa$breaks
bin_mins <- bin_edges[valid_bins]
bin_maxs <- bin_edges[valid_bins + 1]

# Step 4: Filter original data_post_activa to include only values in valid bins
filtered_data_post_activa <- data_post_activa[sapply(data_post_activa, function(x) any(x >= bin_mins & x < bin_maxs))]

# Step 5: Compute density on filtered data_post_activa
dens_filtered_post_activa <- density(filtered_data_post_activa)

x_range <- range(dens_filtered_post_activa$x)
buffer <- diff(x_range) * 0.05
x_range <- c(x_range[1] - buffer, x_range[2] + buffer)


vline_x_post_activa <- restoration_time_activa_post_covid$point_estimate 

# Step 5: Clip original data_post_activa to match density range for the histogram
hist_data_post_activa_clipped <- data_post_activa[data_post_activa >= x_range[1] & data_post_activa <= x_range[2]]

stats_text <- sprintf(
  "Media: %.2f\nMediana: %.2f\nDesv. Est.: %.2f\nAsimetría: %.2f\nCurtosis: %.2f",
  mean(hist_data_post_activa_clipped),
  median(hist_data_post_activa_clipped),
  sd(hist_data_post_activa_clipped),
  skewness(hist_data_post_activa_clipped),
  kurtosis(hist_data_post_activa_clipped)
)

hist_time_post_activa <- plot_ly() %>%
  add_trace(
    x = ~hist_data_post_activa_clipped,
    type = "histogram",
    histnorm = "percent",
    marker = list(color = 'lightblue'),
    autobinx = FALSE,
    xbins = list(size = 1),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = ~dens_filtered_post_activa$x,
    y = ~dens_filtered_post_activa$y * 100,
    type = 'scatter',
    mode = 'lines',
    name = 'Density',
    line = list(color = '#072030', width = 2),
    showlegend = FALSE   
  ) %>%
  add_trace(
    x = c(vline_x_post_activa, vline_x_post_activa),
    y = c(0, max(dens_filtered_post_activa$y * 100)),
    type = 'scatter',
    mode = 'lines',
    name = 'Estimador puntual',
    line = list(color = 'darkblue', dash = 'dash', width = 2),  # now dashed
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = "Histograma del Tiempo de Restablecimiento del Equilibrio período post-COVID (tasa activa)",
      font = list(size = 24)
    ),
    xaxis = list(
      title = list(text = "Meses", font = list(size = 22)),
      tickfont = list(size = 18),
      range = dens_filtered_post_activa$x
    ),
    yaxis = list(
      title = list(text = "Probabilidad", font = list(size = 22)),
      tickfont = list(size = 18)
    ),
    barmode = "overlay",
    showlegend = TRUE,
    legend = list(font = list(size = 20)),  # Emphasizes "Estimador puntual"
    annotations = list(
      list(
        x = 1, y = 1,  # Top right corner in paper coordinates (relative to full plot)
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        text = stats_text,
        showarrow = FALSE,
        font = list(size = 20, family = "Arial", color = "black"),
        align = "left",
        bordercolor = "black",
        borderwidth = 1,
        borderpad = 10,
        bgcolor = "rgba(255,255,255,0.8)"
      )
    )
  )

hist_time_post_activa



# Combine vertically with shared x-axis
fig <- subplot(hist_time_pre_activa, hist_time_post_activa, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "",#Tiempo de Restablecimiento del Equilibrio (tasas activas)
         annotations = list(
           list(
             x = 0.6,
             y = 0.95,
             text = "Período Pre-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           ),
           list(
             x = 0.5,
             y = 0.45,
             text = "Período Post-COVID",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom",
             font = list(size = 20)
           )
         ))

fig


#Estadistica descriptiva de la simulacion para muestra post-COVID y tasa activa
stats <- list(
  mean   = mean(hist_data_post_activa_clipped, na.rm = TRUE),
  sd     = sd(hist_data_post_activa_clipped, na.rm = TRUE),
  q25    = quantile(hist_data_post_activa_clipped, 0.25, na.rm = TRUE),
  median = median(hist_data_post_activa_clipped, na.rm = TRUE),
  q75    = quantile(hist_data_post_activa_clipped, 0.75, na.rm = TRUE),
  skewness = skewness(hist_data_post_activa_clipped, na.rm = TRUE),
  kurtosis = kurtosis(hist_data_post_activa_clipped, na.rm = TRUE)
)


stats


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



