# 📊 Predicción de Atenciones de Urgencia por Causas Respiratorias (2014–2025)

Este proyecto analiza datos históricos de atenciones de urgencia por causas respiratorias desde el año 2014 hasta 2025, utilizando modelos de series temporales y redes neuronales para predecir la evolución de los casos en semanas epidemiológicas. Los datos provienen del Sistema de Atención Diaria de Urgencias (SADU), disponibles en [datos.gob.cl](https://datos.gob.cl).

---

## 📁 Datos

- **Fuente:** [datos.gob.cl](https://datos.gob.cl)
- **Cobertura:** Semanas epidemiológicas desde el año 2014 hasta 2025
- **Sistema:** Sistema de Atención Diaria de Urgencias (SADU)
- **Enfermedades:** Causas respiratorias seleccionadas

---

## ⚙️ Metodología

Se empleó una estrategia basada en:

- Modelado de series temporales.
- Uso del modelo **`nnetar`** (red neuronal autoregresiva) del paquete `forecast` en R.
- Evaluación de precisión con métricas como:
  - **MAPE** (Mean Absolute Percentage Error)
  - **MAE** (Mean Absolute Error)

El análisis permitió evaluar la capacidad predictiva del modelo para distintos tipos de enfermedades respiratorias.

---

## 📈 Resultados

- Predicciones generadas para el año 2025.
- Comparación de la precisión entre enfermedades respiratorias.
- Visualizaciones temporales del comportamiento semanal de las atenciones.

---

## 🧰 Tecnologías y Librerías

- `R`
- `forecast`
- `ggplot2`
- `dplyr`
- `lubridate`

---

## 🧠 Autor

Felipe Muñoz  
📧 Contacto: *(si quieres, puedes poner un correo o tu LinkedIn)*

---

## 📌 Licencia

Este proyecto está bajo una licencia MIT / CC BY-SA (o la que prefieras).  
Incluye datos públicos de libre acceso, procesados con fines educativos y analíticos.
