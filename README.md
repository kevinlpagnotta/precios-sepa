# Precios SEPA: los precios en el AMBA

Kevin Pagnotta. Noviembre de 2026.

Este repositorio reúne el artículo web, el documento técnico y el código de dos análisis hechos con la base de
Precios Claros (SEPA) de la Secretaría de Industria y Comercio.

**Artículo:** [Busque, compare y después compre: los precios en el AMBA](https://kevinlpagnotta.github.io/precios-sepa/)

## Contenido

| Carpeta | Qué hay |
|---|---|
| `index.html`, `graficos/` | El artículo web, publicado con GitHub Pages, y sus gráficos interactivos |
| `paper/` | Documento técnico: «¿Existe pass-through de los impuestos municipales a los precios? Evidencia para el caso de Lanús». Fuente LaTeX (Overleaf) y PDF |
| `did_lanus/` | Diferencias en diferencias y estudio de eventos del documento técnico, con los datos para replicarlo |
| `brecha_lgbm/` | Notebook del modelo LightGBM + SHAP de la sección 2.2 del artículo |

## Diferencias en diferencias (`did_lanus/`)

Produce exactamente las cuatro tablas y los cuatro gráficos del Anexo I del documento técnico, y nada más.

| Ruta | Contenido |
|---|---|
| `did_lanus.ipynb` | Notebook de R, ejecutada |
| `did_lanus.R` | El mismo código como script (celdas marcadas con `## MD` / `## CODE`) |
| `replication_data/panel_formato_abr24_oct25.csv.gz` | Panel balanceado: producto (EAN) × formato × partido × mes, abr-24 a oct-25, con el precio promedio del mes |
| `replication_data/metricas_dispersion_*.csv` | Métricas de dispersión de precios (Tabla 1), dentro de la misma cadena y entre cadenas |
| `scripts/construir_notebook.py` | Convierte `did_lanus.R` en `did_lanus.ipynb` |
| `output/` | Tablas (`.tex`) y gráficos (`.pdf`) que usa el documento |

El panel se arma desde los microdatos SEPA por sucursal: precio promedio mensual de las sucursales de cada formato
(Coto, Vea, DIA y Carrefour Express) en cada partido, con la llave de sucursales corregida.
Se conservan solo los productos con precio en todos los meses y en todos los partidos donde opera el formato.

### Resultados principales

| | Efecto (pp) | p-valor de permutación | Traslado |
|---|---|---|---|
| Hipermercados (Coto y Vea) | 3,22 | 0,048 | 83% |
| Coto | 3,42 | 0,048 | 89% |
| Vea | 2,30 | 0,125 (mínimo posible con 8 partidos) | 59% |
| Supermercados, placebo (DIA y Carrefour Express) | −0,02 | 0,667 | — |

El traslado se mide contra el traslado completo de la tasa: 100 · ln(0,977/0,94) = 3,86 pp.

### Cómo reproducir

```r
install.packages(c("fixest", "data.table", "ggplot2", "IRkernel")); IRkernel::installspec()
```

```bash
cd did_lanus
jupyter nbconvert --to notebook --execute --inplace did_lanus.ipynb   # unos 3 minutos
```

Versiones usadas: R 4.3.3, fixest 0.14.2, data.table 1.14.10 y ggplot2 3.4.4.

## Modelo de brecha de precios (`brecha_lgbm/`)

LightGBM que predice la brecha de precios de cada sucursal frente a sus rivales del mismo formato (celda: sucursal ×
categoría × origen × mes), entrenado de febrero de 2025 a septiembre de 2026 y evaluado en octubre de 2026, con el 70%
de las sucursales de cada bandera y 10 semillas. En octubre, RMSE 3,03 y MAE 1,47 (persistencia: 3,71 y 1,52).
El SHAP sobre todas las celdas de octubre da los dos gráficos del artículo. Los insumos no se incluyen: la notebook se
comparte para mostrar las decisiones técnicas.

## Fuente

Kevin Pagnotta en base a SEPA (Precios Claros), Secretaría de Industria y Comercio.
