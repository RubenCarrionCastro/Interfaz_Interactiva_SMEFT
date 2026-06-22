# `WilsonUI_SMEFTsim.wl` — Módulo principal

Contiene la interfaz gráfica completa y toda la lógica de cálculo. Al evaluarlo,
carga FeynCalc/FeynArts, define todas las funciones internas y lanza la GUI
mediante `WilsonUIManualCatalog[]` (última línea del fichero).

## Responsabilidades

Este módulo se encarga de:

- **Localizar y validar el modelo.** Detecta automáticamente la carpeta `Models/`
  situada junto al `.wl` (o junto al notebook) y comprueba que existan los
  ficheros `.mod`, `.gen` y `.pars` del modelo indicado.
- **Cargar los coeficientes de Wilson** desde el fichero `.pars`, filtrando los
  parámetros físicos del Modelo Estándar para quedarse solo con los WCs.
- **Construir el proceso** en la notación interna de FeynArts a partir de las
  partículas seleccionadas, incluyendo la expansión del protón en canales
  partónicos para colisiones hadrónicas.
- **Ejecutar el flujo de cálculo** (ver más abajo).
- **Gestionar la interfaz**: menús, casillas, barra de progreso, mensajes de
  estado, exportación a PDF y botones de ayuda.

## Flujo de cálculo

Al pulsar *Calcular los Coeficientes de Wilson* se ejecuta, para cada canal:

1. Validación de la configuración y del modelo.
2. Inicialización del modelo en FeynArts (`InitializeModel`).
3. Construcción del proceso (y expansión partónica si procede).
4. Generación de topologías (`CreateTopologies`).
5. Inserción de campos (`InsertFields`).
6. Construcción de la amplitud (`CreateFeynAmp`).
7. Conversión a FeynCalc (`FCFAConvert`).
8. Aplicación de simetrías y simplificaciones.
9. Análisis de la amplitud al cuadrado `|M|^2`.
10. Extracción de los coeficientes de Wilson.

Todo el flujo se ejecuta de forma defensiva: cada llamada costosa se envuelve en
`TimeConstrained` y captura de errores, de modo que un fallo en un canal o en una
simplificación no detiene el cálculo completo, sino que se registra y se continúa.

## Opciones principales de la GUI

- **Proceso:** número y tipo de partículas entrantes/salientes (leptones,
  neutrinos, quarks, vectores, escalares, hadrones). Botón *Usar colisión p p*
  para colisiones hadrónicas.
- **InsertionLevel / LoopOrder / Topologías:** nivel de inserción (partículas o
  clases), orden perturbativo (árbol o 1-loop) y tipo de topologías.
- **Simetrías y simplificaciones:** CP conservante, WCs reales, lineal en WCs,
  leptones sin masa y restricción al sector top.
- **Modo de simplificación:** Ninguna / Ligera / Completa, con *timeout*.
- **Amplitud al cuadrado `|M|^2`:** Off (busca WCs en `M`), Estructural
  (interferencia lineal) o Físico (cálculo completo con espín, polarización y,
  opcionalmente, color).

## Entradas y salidas

- **Entrada:** los ficheros del modelo en `Models/` y las opciones de la GUI.
- **Salida:** la lista de WCs relevantes (en pantalla y en las variables globales
  `lastWCNames` y `lastWCResult`), y opcionalmente un PDF con los diagramas.

## Personalización

Para cambiar de modelo o de valores por defecto, consulta el botón **Guía de
modificaciones** de la propia interfaz. Los nombres de modelo por defecto
(`modelName`, `genName`) se fijan dentro del `DynamicModule` de
`WilsonUIManualCatalog`.

## Dependencias

FeynCalc + FeynArts (obligatorias) y los ficheros del modelo SMEFTsim `topU3l`.
Este módulo es autónomo: no requiere el traductor para funcionar.
