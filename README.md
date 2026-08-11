# 📊 Predicción de atenciones de urgencia por causas respiratorias (2014–2025)

Publicado oficialmente en [datos.gob.cl](https://datos.gob.cl/data_reuse/9be39c49-ce4e-4d73-b43a-5f0aed4c4bfa)

Este proyecto analiza datos históricos de atenciones de urgencia por causas respiratorias en Chile desde 2014 hasta 2025, y utiliza un modelo de redes neuronales autorregresivas para predecir la evolución de los casos por semana epidemiológica. Los datos provienen del Sistema de Atención Diaria de Urgencias (SADU), disponibles en [datos.gob.cl](https://datos.gob.cl).

El objetivo es explorar hasta qué punto un modelo relativamente simple puede anticipar los peaks estacionales de invierno, información relevante para la planificación de camas y personal en servicios de urgencia.

---

## 📁 Datos

- **Fuente:** [datos.gob.cl](https://datos.gob.cl) — Sistema de Atención Diaria de Urgencias (SADU)
- **Cobertura:** semanas epidemiológicas desde 2014 hasta 2025
- **Enfermedades analizadas:** Neumonía (J12–J18) e Influenza (J09–J11)
- **Filtro aplicado:** solo atenciones en establecimientos tipo `Hospital` (no incluye SAPU, SAR ni otros niveles de urgencia de la red)
- **Carga:** el script descarga el dataset en formato `.parquet` directamente desde la URL pública de datos.gob.cl al ejecutarse — no requiere archivos locales ni pasos manuales de descarga

---

## ⚙️ Metodología

- Se construyen dos series temporales semanales (Neumonía e Influenza) con `frequency = 52`.
- Se ajusta un modelo **`nnetar`** (red neuronal autorregresiva, paquete `forecast` en R) para cada causa.
- Se generan predicciones a 20 semanas.
- Se evalúa el ajuste con **MAPE** (Error Porcentual Absoluto Medio) y **MAE** (Error Absoluto Medio) usando `accuracy()`.

**Nota metodológica:** las métricas reportadas se calculan sobre el ajuste del modelo a los propios datos de entrenamiento (in-sample), no sobre un conjunto de prueba separado. Es una medida de qué tan bien el modelo explica el pasado, no una validación definitiva de su capacidad predictiva a futuro. Una validación out-of-sample (split temporal train/test o backtesting con rolling origin) queda como mejora pendiente — ver [Limitaciones](#-limitaciones).

---

## 📈 Resultados

| Causa | MAE | MAPE |
|---|---|---|
| Neumonía | 120,21 | 8,79% |
| Influenza | 67,93 | 20,43% |

La influenza es sistemáticamente más difícil de predecir que la neumonía: su MAPE casi duplica al de neumonía, reflejo de peaks más abruptos y de mayor variabilidad interanual.

Para la temporada 2025, el modelo proyectó:

- **Neumonía:** un peak cercano a los **3.200 casos** hacia la semana epidemiológica 24–25 (≈ fines de junio).
- **Influenza:** un peak superior a los **4.100 casos** hacia la semana epidemiológica 24, con una curva más angosta y pronunciada que la de neumonía.

### Visualizaciones

**Comportamiento histórico por semana epidemiológica (2014–2025)**

![Casos de Neumonía e Influenza por semana epidemiológica](img/casos_neumonia_influenza_animado.gif)

**Predicción 2025 — Neumonía** (observado vs. predicho)

![Predicción Neumonía 2025](img/prediccion_neumonia_2025.gif)

**Predicción 2025 — Influenza** (observado vs. predicho)

![Predicción Influenza 2025](img/prediccion_influenza_2025.gif)

**Predicción con intervalo de confianza (salida nativa de `forecast()`)**

| Neumonía | Influenza |
|---|---|
| ![Intervalo Neumonía](img/prediccion_neumonia_intervalo.png) | ![Intervalo Influenza](img/prediccion_influenza_intervalo.png) |

---

## ⚠️ Limitaciones

- **Validación in-sample:** como se explica en Metodología, el MAPE/MAE reportado no proviene de un backtest con datos no vistos por el modelo. Los resultados deben leerse como una medida de bondad de ajuste, no como una garantía de precisión futura.
- **Efecto pandemia (2020–2021):** la serie muestra una caída drástica y sostenida de casos durante ese período, atribuible a las medidas de distanciamiento social y uso de mascarillas durante la pandemia de COVID-19. El modelo no incorpora ningún tratamiento explícito de esta discontinuidad estructural, lo que puede afectar cómo pondera la estacionalidad de años recientes.
- **Cobertura parcial:** el análisis considera solo establecimientos tipo `Hospital`. No representa la totalidad de la red de urgencia (SAPU, SAR, etc.).
- **Aproximación de frecuencia semanal:** se fija `frequency = 52` para todos los años. Los años con 53 semanas epidemiológicas pueden introducir un leve desfase estacional acumulado a lo largo de una serie de más de 10 años.
- **Sin comparación contra modelos base:** no se contrastó `nnetar` frente a alternativas más simples (ARIMA, ETS, estacional ingenuo) dentro de este script, por lo que no hay evidencia publicada de que la red neuronal sea superior a un enfoque más simple para esta serie.

---

## 🧰 Tecnologías y librerías

- `R`
- `forecast`
- `ggplot2`
- `gganimate`
- `dplyr`
- `arrow`
- `janitor`
- `scales`

---

## ▶️ Cómo ejecutar

```r
# Instalar dependencias (una vez)
install.packages(c("tidyverse", "arrow", "janitor", "forecast",
                    "tseries", "gganimate", "scales", "gifski"))

# Ejecutar el script completo
Rscript series_de_tiempo_enfermerdades_respiratorias.R
```

`gganimate` requiere el paquete `gifski` instalado para renderizar las animaciones a `.gif`. El script descarga los datos directamente desde datos.gob.cl, por lo que no depende de rutas locales.

---

## 📁 Estructura del proyecto

```
├── series_de_tiempo_enfermerdades_respiratorias.R   # Script principal
├── img/                                              # Visualizaciones generadas
└── README.md
```

---

## 🔗 Fuente de datos

- [Atenciones de urgencia respiratoria semanal — SADU](https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4)

---

## 👤 Autor

**Felipe Muñoz**
Tecnólogo Médico | Codificador & Auditor GRD Senior | Health Data Science
[LinkedIn](https://www.linkedin.com/in/felipe-m-92123990) · [Proyecto en datos.gob.cl](https://datos.gob.cl/data_reuse/9be39c49-ce4e-4d73-b43a-5f0aed4c4bfa)

---

## 📌 Licencia

Este proyecto está bajo licencia MIT/CC BY-SA. Incluye datos públicos de libre acceso, procesados con fines educativos y analíticos.
