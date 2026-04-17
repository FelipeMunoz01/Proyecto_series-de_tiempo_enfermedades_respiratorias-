# 📊 Predicción de atenciones de urgencia por causas respiratorias (2014–2025)

Publicado oficialmente en [datos.gob.cl](https://datos.gob.cl/data_reuse/9be39c49-ce4e-4d73-b43a-5f0aed4c4bfa)

Este proyecto analiza datos históricos de atenciones de urgencia por causas respiratorias desde el año 2014 hasta 2025, utilizando modelos de series temporales y redes neuronales para predecir la evolución de los casos en semanas epidemiológicas. Los datos provienen del Sistema de Atención Diaria de Urgencias (SADU), disponibles en [datos.gob.cl](https://datos.gob.cl).

---

## 📁 Datos

- **Fuente:** datos.gob.cl
- **Cobertura:** Semanas epidemiológicas desde el año 2014 hasta 2025
- **Sistema:** Sistema de Atención Diaria de Urgencias (SADU)
- **Enfermedades analizadas:** Neumonía (J12-J18) e Influenza (J09-J11)

---

## ⚙️ Metodología

Se empleó una estrategia basada en:

- Modelado de series temporales
- Uso del modelo `nnetar` (red neuronal autorregresiva) del paquete `forecast` en R
- Evaluación de precisión con métricas:
  - MAPE (Error Porcentual Absoluto Medio)
  - MAE (Error Absoluto Medio)

El análisis permitió evaluar la capacidad predictiva del modelo para distintos tipos de enfermedades respiratorias.

---

## 📈 Resultados

- Predicciones generadas para el año 2025
- Comparación de la precisión entre neumonía e influenza
- Visualizaciones animadas del comportamiento semanal de las atenciones

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

## 📁 Estructura del proyecto
├── series_de_tiempo_enfermerdades_respiratorias.R  # Script principal
└── README.md

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
