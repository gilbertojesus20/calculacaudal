# Importar paquetes
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.integrate as integrate

# Definir parámetros
porosidad_suelo = 0.3   # Porosidad del suelo
Ks_suelo = 0.1          # Conductividad hidráulica saturada del suelo
Kf_suelo = 0.01         # Conductividad hidráulica freatica del suelo
Kc_canal = 0.02         # Conductividad hidráulica del canal
evap_pot = 0.3          # Evapotranspiración potencial
vol_embalse = 10        # Volumen del embalse

# Ingresar datos al usuario
print("Ingrese los datos de precipitación, evapotranspiración y caudal observado:")
precipitacion = float(input("Precipitación (mm): "))
evapotranspiracion = float(input("Evapotranspiracion (mm): "))
caudal_observado = float(input("Caudal Observado (m3/s): "))

# Cálculo de la infiltración
infiltracion = porosidad_suelo*precipitacion - evapotranspiracion

# Cálculo de la evapotranspiración
evap_real = np.minimum(evap_pot*precipitacion, infiltracion)

# Cálculo del flujo superficial
almacenamiento = np.minimum(infiltracion - evap_real, vol_embalse)
flujo_superficial = np.maximum(infiltracion - evap_real - almacenamiento, 0)

# Cálculo de la escorrentía
escorrentía = Ks_suelo*(infiltracion - evap_real - almacenamiento) + Kf_suelo*flujo_superficial

# Cálculo del flujo en el canal
def flujo_canal(escorrentía, Kc_canal, caudal_observado):
    tiempo = np.arange(0, len(escorrentía))
    c1 = (Kc_canal*Kc_canal)/(Kc_canal*Kc_canal - 4*Kc_canal + 4)
    c2 = (2*Kc_canal - 2)/(Kc_canal*Kc_canal - 4*Kc_canal + 4)
    c3 = (Kc_canal*Kc_canal - 2*Kc_canal)/(Kc_canal*Kc_canal - 4*Kc_canal + 4)
    flujo_canal = integrate.cumtrapz(escorrentía, tiempo, initial=caudal_observado)
    flujo_canal = c1*flujo_canal + c2*escorrentía[0] + c3*escorrentía
    return flujo_canal

flujo_canal = flujo_canal(escorrentía, Kc_canal, caudal_observado)

# Cálculo de los índices de desempeño
RMSE = np.sqrt(np.mean((flujo_canal - caudal_observado)**2))
NSE = 1 - (np.sum((flujo_canal - caudal_observado)**2))/np.sum((flujo_canal - np.mean(caudal_observado))**2)

# Mostrar resultados
print("RMSE = ", RMSE)
print("NSE = ", NSE)

# Graficar resultados
plt.plot(flujo_canal, label="Flujo del canal")
plt.plot(caudal_observado, label="Caudal Observado")
plt.xlabel("Tiempo (días)")
plt.ylabel("Caudal (m3/s)")
plt.legend()
plt.show()