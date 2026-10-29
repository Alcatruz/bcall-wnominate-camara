# Medir dos veces

B-Call comparado con las estimaciones ideológicas de Fábrega (2025) para la Cámara de Diputados de Chile, 2002-2026, más lo que va de 2026-2030.

* [Leer el análisis →](https://alcatruz.github.io/bcall-wnominate-camara/)**

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `datos/datos.js` | Los datos que lee la página, incluidos los votos de los diez casos de la Figura 4 |
| `R/00_wnominate_2026.R` | Estima W-NOMINATE para 2026-2030 con la especificación de Fábrega |
| `R/01_comparar.R` | Cruza las dos bases y calcula las rupturas con el propio bloque |
| `R/02_votos.R` | Arma los votos de los diez casos de la Figura 4 |
| `R/03_pagina.R` | Arma `datos/datos.js` |

## Datos y decisiones

- Estimaciones de Fábrega del depósito en Harvard Dataverse, versión 2.0 ([doi:10.7910/DVN/FOXOIT](https://doi.org/10.7910/DVN/FOXOIT)).
- Estimaciones de B-Call por período del [análisis anterior](https://github.com/Alcatruz/bcall-camara-2002-2026), sobre las votaciones de proyectos de ley con número de boletín de opendata.camara.cl.
- Se compara dentro de cada período, diputado por diputado, con el identificador de la Cámara. La base de Fábrega no cubre 2026-2030. Para ese período se usa un W-NOMINATE estimado con su código sobre los votos hasta el 9 de septiembre de 2026 (`R/00_wnominate_2026.R`).
- El script se corre desde la raíz de un proyecto local que guarda el depósito de Fábrega en `data/fabrega_2025/` y la base de votos de B-Call en `outputs/data/post_bcall/`. Esos archivos no están en este repositorio.

## Licencia

`datos/datos.js` contiene estimaciones de Fábrega (2025) y se distribuye con su misma licencia, [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).

## Referencias

Fábrega, J. (2025). Ideological positions in the Chilean Chamber of Deputies (2002–2026). A legislative roll-call dataset. *Data in Brief, 63*, 112163. https://doi.org/10.1016/j.dib.2025.112163

Toro-Maureira, S., Reutter, J., Valenzuela, L., Alcatruz, D. y Valenzuela, M. (2025). B-Call: integrating ideological position and voting cohesion in legislative behavior. *Frontiers in Political Science, 7*, 1670089. https://doi.org/10.3389/fpos.2025.1670089
