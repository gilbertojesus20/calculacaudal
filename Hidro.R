'''
El modelo tiene varios pasos:

    Carga los datos necesarios de precipitación, evapotranspiración y caudal observado. También solicita al usuario los parámetros del modelo.
    Establece el tamaño del paso de tiempo.
    Calcula la infiltración utilizando el método del índice de infiltración.
    Calcula la evapotranspiración utilizando el método de la curva de tendencia de evapotranspiración.
    Calcula el almacenamiento en el embalse utilizando el método de la curva de tendencia de almacenamiento.
    Calcula el flujo de agua a través del canal utilizando el método de Muskingum.

El código utiliza subrutinas (funciones) para cada uno de los pasos, y utiliza bucles para recorrer los datos de entrada y calcular los resultados. El modelo se basa en ecuaciones hidrológicas y los parámetros del modelo son ingresados por el usuario.

'''
# Modelo de lluvia-escorrentía para una cuenca tropical

# 1. Cargue los datos necesarios

# Solicitar al usuario los datos de precipitación, evapotranspiración y caudal observado
precipitacion <- read.csv(file.choose(), header = TRUE)
evapotranspiracion <- read.csv(file.choose(), header = TRUE)
caudal_observado <- read.csv(file.choose(), header = TRUE)

# Solicitar al usuario los parámetros del modelo
# Parámetro de infiltración
k_infiltracion <- as.numeric(readline(prompt = "Ingresa el parámetro de infiltración (en mm/h): "))
# Parámetro de almacenamiento en el embalse
k_almacenamiento <- as.numeric(readline(prompt = "Ingresa el parámetro de almacenamiento (en mm): "))
# Parámetro de evapotranspiración
k_evapotranspiracion <- as.numeric(readline(prompt = "Ingresa el parámetro de evapotranspiración (en mm/h): "))
# Parámetro del canal
k_canal <- as.numeric(readline(prompt = "Ingresa el parámetro del canal (en mm): "))

# 2. Establezca el tamaño del paso de tiempo
paso_de_tiempo <- as.numeric(readline(prompt = "Ingresa el tamaño del paso de tiempo (en horas): "))

# 3. Calcule la infiltración
  # Subrutina para calcular la infiltración
  infiltracion <- function(precipitacion, k_infiltracion, paso_de_tiempo) {
    
    # Definir un vector para almacenar los resultados
    infiltracion <- rep(0, length(precipitacion))
    
    # Bucle para calcular la infiltración utilizando el método del índice de infiltración 
    for (i in 1:length(precipitacion)) {
      if (i == 1) {
        infiltracion[i] <- k_infiltracion / 60 * precipitacion[i] * paso_de_tiempo
      } 
      else {
        infiltracion[i] <- k_infiltracion / 60 * (precipitacion[i] + precipitacion[i-1]) * paso_de_tiempo / 2
      }
    }
    
    return(infiltracion)
  }

# 4. Calcule la evapotranspiración
  # Subrutina para calcular la evapotranspiración
  evapotranspiracion <- function(evapotranspiracion, k_evapotranspiracion, paso_de_tiempo) {
    
    # Definir un vector para almacenar los resultados
    evapotranspiracion <- rep(0, length(evapotranspiracion))
    
    # Bucle para calcular la evapotranspiración utilizando el método de la curva de tendencia de evapotranspiración
    for (i in 1:length(evapotranspiracion)) {
      evapotranspiracion[i] <- k_evapotranspiracion / 60 * evapotranspiracion[i] * paso_de_tiempo
    }
    
    return(evapotranspiracion)
  }

# 5. Calcule el almacenamiento en el embalse
  # Subrutina para calcular el almacenamiento en el embalse
  almacenamiento <- function(precipitacion, evapotranspiracion, k_almacenamiento, paso_de_tiempo) {
    
    # Definir un vector para almacenar los resultados
    almacenamiento <- rep(0, length(precipitacion))
    
    # Bucle para calcular el almacenamiento en el embalse utilizando el método de la curva de tendencia de almacenamiento
    for (i in 1:length(precipitacion)) {
      if (i == 1) {
        almacenamiento[i] <- k_almacenamiento / 60 * (precipitacion[i] - evapotranspiracion[i]) * paso_de_tiempo
      }
      else {
        almacenamiento[i] <- k_almacenamiento / 60 * (precipitacion[i] - evapotranspiracion[i] + precipitacion[i-1] - evapotranspiracion[i-1]) * paso_de_tiempo / 2
      }
    }
    
    return(almacenamiento)
  }

# 6. Calcule el flujo de agua a través del canal
  # Subrutina para calcular el flujo de agua a través del canal utilizando el método de Muskingum
  flujo <- function(precipitacion, evapotranspiracion, almacenamiento, k_canal, paso_de_tiempo) {
    
    # Definir un vector para almacenar los resultados
    flujo <- rep(0, length(precipitacion))
    
    # Bucle para calcular el flujo de agua a través del canal utilizando el método de Muskingum
    for (i in 1:length(precipitacion)) {
      if (i == 1) {
        flujo[i] <- k_canal / 60 * (precipitacion[i] - evapotranspiracion[i] + almacenamiento[i]) * paso_de_tiempo
      }
      else {
        flujo[i] <- k_canal / 60 * (precipitacion[i] - evapotranspiracion[i] + almacenamiento[i] + precipitacion[i-1] - evapotranspiracion[i-1] + almacenamiento[i-1]) * paso_de_tiempo / 2
      }
    }
    
    return(flujo)
  }

# 7. Calcule los índices de desempeño del modelo
  # Subrutina para calcular los índices de desempeño del modelo 
  indices_desempeno <- function(caudal_observado, flujo) {
    
    # Calcular el porcentaje de error absoluto medio
    pct_error_absoluto_medio <- mean(abs(caudal_observado - flujo)) / mean(caudal_observado) * 100
    
    # Calcular el coeficiente de determinación
    coeficiente_determinacion <- cor(caudal_observado, flujo) ^ 2
    
    # Calcular el índice de Nash-Sutcliffe
    nash_sutcliffe <- 1 - sum((caudal_observado - flujo) ^ 2) / sum((caudal_observado - mean(caudal_observado)) ^ 2)
    
    # Calcular el índice de Kling-Gupta
    kling_gupta <- 1 - (sum((caudal_observado - flujo) ^ 2) / sum((caudal_observado - mean(caudal_observado)) ^ 2)) * (length(caudal_observado) / (length(caudal_observado) - 1))
    
    # Almacenar los resultados en una lista
    indices_desempeno <- list(pct_error_absoluto_medio = pct_error_absoluto_medio, coeficiente_determinacion = coeficiente_determinacion, nash_sutcliffe = nash_sutcliffe, kling_gupta = kling_gupta)
    
    return(indices_desempeno)
  }

# 8. Ejecutar el modelo
  # Calcular la infiltración
  infiltracion <- infiltracion(precipitacion, k_infiltracion, paso_de_tiempo)
  
  # Calcular la evapotranspiración
  evapotranspiracion <- evapotranspiracion(evapotranspiracion, k_evapotranspiracion, paso_de_tiempo)
  
  # Calcular el almacenamiento en el embalse
  almacenamiento <- almacenamiento(precipitacion, evapotranspiracion, k_almacenamiento, paso_de_tiempo)
  
  # Calcular el flujo de agua a través del canal
  flujo <- flujo(precipitacion, evapotranspiracion, almacenamiento, k_canal, paso_de_tiempo)
  
  # Calcular los índices de desempeño del modelo
  indices_desempeno <- indices_desempeno(caudal_observado, flujo)
  
  # Imprimir los índices de desempeño del modelo
  print("Índices de desempeño del modelo:")
  print(indices_desempeno)