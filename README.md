# Generador de Coeficientes de Wilson de la SMEFT

Interfaz gráfica en **Wolfram Mathematica** que, dado un proceso físico, identifica
de forma automática qué **coeficientes de Wilson** de la *Standard Model Effective
Field Theory* (SMEFT) son relevantes para ese observable.

La herramienta actúa como una capa de automatización sobre las librerías
[FeynArts](https://feynarts.de/) y [FeynCalc](https://feyncalc.github.io/),
orquestando toda la cadena de cálculo —generación de diagramas, construcción de la
amplitud, aplicación de hipótesis físicas y análisis de la amplitud al cuadrado—
y extrayendo al final la lista de coeficientes que contribuyen al proceso.

Desarrollada como parte de un Trabajo Fin de Grado del Grado de Física
(Universidad de Córdoba).

---

## ¿Qué hace?

- Genera los diagramas de Feynman del proceso (a nivel árbol o de un bucle).
- Construye la amplitud y la convierte a FeynCalc para su manipulación simbólica.
- Aplica simetrías y simplificaciones físicas: conservación de CP, coeficientes
  reales, truncamiento lineal en los WCs, aproximación de leptones sin masa y
  restricción al sector top.
- Calcula la amplitud al cuadrado `|M|^2` en tres modos (directo, estructural o
  físico completo con sumas de espín, polarización y color).
- Extrae automáticamente los coeficientes de Wilson presentes en el resultado.
- Soporta colisiones hadrónicas, expandiendo el protón en sus canales partónicos.
- Exporta los diagramas a PDF.

Como post-procesado, un módulo auxiliar **traduce** los nombres de los
coeficientes de SMEFTsim a otras bases (SMEFT@NLO, dim6top), muestra el operador
asociado a cada uno y los renderiza en LaTeX.

---

## Requisitos

| Componente | Necesario | Notas |
|---|---|---|
| Wolfram Mathematica | Sí | Probada en la versión 15.0. |
| FeynCalc | Sí | Se carga junto con el add-on FeynArts. |
| FeynArts | Sí | Se carga automáticamente como add-on de FeynCalc. |
| Modelo SMEFTsim `topU3l` | Sí | Incluido en `Models/` (`.mod`, `.gen`, `.pars`). |
| [MaTeX](https://github.com/szhorvat/MaTeX) | Opcional | Renderiza los WCs y operadores en LaTeX; requiere una instalación de LaTeX. Si no está disponible, se usa la notación nativa de Mathematica. |

Si no tienes FeynCalc/FeynArts instalados, sigue las instrucciones de la
[web oficial de FeynCalc](https://feyncalc.github.io/). Existe una versión de
prueba de Mathematica de 15 días en
[wolfram.com/mathematica/trial](https://www.wolfram.com/mathematica/trial/).

---

## Estructura del repositorio

```
InterfazBuena/
├── WilsonUI_SMEFTsim.wl           # Módulo principal: GUI + flujo de cálculo
├── SMEFTsim_WC_traducciones.wl    # Traductor auxiliar de WCs (post-procesado)
├── NotebookLlamadaInterfaz.nb     # Notebook lanzador
├── Models/                        # Modelo de FeynArts (SMEFTsim topU3l)
│   ├── SMEFTsimtopU3l.mod
│   ├── SMEFTsimtopU3l.gen
│   └── SMEFTsimtopU3l.pars
├── diagramas_canales/             # PDFs de ejemplo (diagramas exportados)
└── docs/
    ├── WilsonUI_SMEFTsim.md
    └── SMEFTsim_WC_traducciones.md
```

> **Importante:** la carpeta `Models/` debe estar siempre al mismo nivel que el
> `.wl` de la interfaz (o que el notebook que lo carga). La interfaz localiza esa
> carpeta de forma automática a partir de su propia ubicación.

---

## Uso rápido

1. Abre `NotebookLlamadaInterfaz.nb` en Mathematica (o crea un notebook propio).
2. Carga los módulos:
   ```mathematica
   Get["WilsonUI_SMEFTsim.wl"];          (* lanza la GUI *)
   Get["SMEFTsim_WC_traducciones.wl"];   (* opcional: traductor *)
   ```
3. Aparecerá la interfaz **Generador de Coeficientes de Wilson**.
4. Selecciona el proceso (p. ej. `e- e+ -> t tbar`), las opciones de cálculo y las
   simetrías deseadas, y pulsa **Calcular los Coeficientes de Wilson**.
5. Una vez terminado, pulsa **Exportar diagramas a PDF** si quieres guardar los
   diagramas.

La interfaz incluye tres botones de ayuda integrados: **Guía de uso**, **Guía de
modificaciones** (qué tocar para cambiar de modelo) y **Guía de errores**.

---

## Salida

El cálculo deja la lista de coeficientes relevantes en las variables globales
`lastWCNames` (nombres) y `lastWCResult` (símbolos), además de mostrarla en
pantalla. Sobre ellas se puede aplicar directamente el traductor:

```mathematica
translateWCListLatexTable[lastWCNames]   (* tabla WC -> operador -> LaTeX *)
wcCoverageReport[lastWCNames]            (* cobertura de la traducción   *)
```

---

## Documentación

- [`docs/WilsonUI_SMEFTsim.md`](docs/WilsonUI_SMEFTsim.md) — módulo principal.
- [`docs/SMEFTsim_WC_traducciones.md`](docs/SMEFTsim_WC_traducciones.md) — traductor.

---

## Créditos

Trabajo Fin de Grado, Grado de Física, Universidad de Córdoba.
Basado en FeynArts, FeynCalc, el modelo SMEFTsim (variante `topU3l`) y MaTeX.
