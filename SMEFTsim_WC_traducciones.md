# `SMEFTsim_WC_traducciones.wl` — Traductor de coeficientes de Wilson

Módulo auxiliar de **post-procesado** para interpretar y traducir los coeficientes
 de Wilson obtenidos con modelos SMEFTsim. No modifica el cálculo ni requiere la
interfaz gráfica: puede cargarse y usarse de forma independiente en cualquier
notebook de Mathematica.

Su propósito es relacionar la nomenclatura de **SMEFTsim**
(`top`, `topU3l` y variantes generales) con otras convenciones de uso frecuente,
en particular **SMEFT@NLO** y **dim6top**, e identificar el operador asociado en
la base de Warsaw.

## Qué proporciona

- **Traducciones de coeficientes de Wilson** entre SMEFTsim, SMEFT@NLO y dim6top.
  Las relaciones pueden ser directas, contener un signo convencional o corresponder
  a una combinación lineal o rotación de base.
- **Operador asociado** a cada coeficiente, con su notación de Warsaw, dimensión
  canónica y orden en \(\Lambda\).
- **Tablas renderizadas** con notación matemática: utiliza MaTeX cuando está
  disponible y, en caso contrario, una representación nativa de Mathematica.
- **Exportación a LaTeX** de tablas resumidas o detalladas en un archivo `.tex`.
- **Exportación CSV** de la tabla de traducciones simplificada.

## Carga del módulo

```mathematica
Get["SMEFTsim_WC_traducciones_simplificado_v4.wl"];
```

El archivo puede cargarse antes o después de la interfaz principal. Por sí solo no
necesita `lastWCNames`; basta con pasarle una lista explícita de nombres de
coeficientes.

## Funciones principales

| Función | Uso |
|---|---|
| `wcTranslationsDataset[]` | Devuelve la tabla general de traducciones. |
| `translateFromSMEFTsim["ctWRe"]` | Busca las traducciones de un coeficiente concreto. |
| `translateWCList[{"ctWRe", "cHQ3"}]` | Traduce una lista de coeficientes. |
| `translateToTarget["dim6top"]` | Muestra las relaciones disponibles hacia una base de destino. |
| `wcCoverageReport[wcs]` | Informa de los coeficientes traducidos y de los que no están cubiertos. |
| `untranslatedWCs[wcs]` | Devuelve únicamente los coeficientes sin traducción disponible. |
| `translateWCListSummaryTable[wcs]` | Genera una tabla resumida, con una fila por coeficiente. |
| `translateWCListRenderedTable[wcs]` | Genera una tabla detallada, con una fila por relación de traducción. |
| `translateLastWCResult[]` | Atajo para usar `lastWCNames` o `lastWCResult` de la interfaz, cuando existan. |
| `exportWCTranslationsCSV[ruta]` | Exporta la tabla simplificada a CSV. |
| `exportWCTranslationsTeX[wcs, ruta]` | Exporta tablas de traducción a un archivo `.tex`. |

## Uso habitual con la interfaz

Tras completar un cálculo en la interfaz principal:

```mathematica
Get["SMEFTsim_WC_traducciones_simplificado_v4.wl"];

wcCoverageReport[lastWCNames]
translateWCListSummaryTable[lastWCNames]
translateWCListRenderedTable[lastWCNames, "SMEFT@NLO"]
```

También puede emplearse sin haber ejecutado la interfaz:

```mathematica
wcs = {"ctWRe", "ctBRe", "ctHRe", "cHQ3"};

translateWCListSummaryTable[wcs]
wcCoverageReport[wcs]
```

## Exportación a LaTeX

La función `exportWCTranslationsTeX` crea un archivo de texto `.tex` con tablas
`longtable`, preparado para documentos extensos y para saltar de página cuando
sea necesario. Es completamente independiente de la interfaz: recibe la lista
de WCs como primer argumento y no utiliza variables globales salvo que se le pase
expresamente `lastWCNames`.

### Exportación básica

```mathematica
exportWCTranslationsTeX[
  {"ctWRe", "ctBRe", "ctHRe", "cHQ3"},
  "tabla_wilson.tex"
]
```

Por defecto genera una **tabla resumida** y devuelve la ruta del archivo creado.
La tabla contiene una fila por WC con las columnas:

```text
SMEFTsim | Operator | Dim. | Lambda order | SMEFT@NLO | dim6top
```

El archivo resultante incluye el preámbulo de LaTeX y puede compilarse
directamente, por ejemplo con:

```bash
pdflatex tabla_wilson.tex
```

### Tabla detallada

Para obtener una fila por cada relación entre el WC y una base de destino:

```mathematica
exportWCTranslationsTeX[
  {"ctWRe", "ctBRe", "ctHRe", "cHQ3"},
  "tabla_wilson_detallada.tex",
  "TableType" -> "Detailed"
]
```

La tabla detallada incluye:

```text
SMEFTsim | Operator | Target | Target expression
```

### Resumen y detalle en el mismo archivo

```mathematica
exportWCTranslationsTeX[
  lastWCNames,
  "resultado_wilson.tex",
  "TableType" -> "Both"
]
```

### Filtrar por base de destino

```mathematica
exportWCTranslationsTeX[
  lastWCNames,
  "resultado_smeftnlo.tex",
  "Target" -> "SMEFT@NLO"
]

exportWCTranslationsTeX[
  lastWCNames,
  "resultado_dim6top.tex",
  "Target" -> "dim6top",
  "TableType" -> "Detailed"
]
```

### Exportar todas las relaciones conocidas

```mathematica
exportWCTranslationsTeX[
  All,
  "traducciones_completas.tex",
  "TableType" -> "Both"
]
```

### Opciones disponibles

| Opción | Valor por defecto | Descripción |
|---|---:|---|
| `"Target"` | `All` | Base de destino: `All`, `"SMEFT@NLO"` o `"dim6top"`. |
| `"TableType"` | `"Summary"` | Tipo de tabla: `"Summary"`, `"Detailed"` o `"Both"`. |
| `"Title"` | `"SMEFTsim Wilson-coefficient translations"` | Título mostrado al inicio del documento LaTeX. |
| `"IncludePreamble"` | `True` | Incluye un documento LaTeX completo y compilable. |

Por ejemplo, para insertar solamente las tablas en un documento LaTeX que ya
contiene su propio preámbulo:

```mathematica
exportWCTranslationsTeX[
  lastWCNames,
  "tablas_para_incluir.tex",
  "TableType" -> "Both",
  "IncludePreamble" -> False
]
```

En ese caso, el documento principal debe cargar los paquetes `amsmath`, `amssymb`,
`array`, `booktabs`, `longtable` y `pdflscape`.

Si no se indica una ruta, la función genera automáticamente un archivo con nombre
`SMEFTsim_WC_traducciones_summary.tex`, `SMEFTsim_WC_traducciones_detailed.tex` o
`SMEFTsim_WC_traducciones_both.tex` en el directorio del notebook; si no hay un
notebook activo, usa el directorio del propio archivo o el directorio de trabajo
actual.

## Exportación CSV

Para exportar la tabla de traducciones simplificada:

```mathematica
exportWCTranslationsCSV["SMEFTsim_WC_traducciones.csv"]
```

Sin argumento, se crea `SMEFTsim_WC_traducciones_UFO_COMPLETO.csv` en el directorio
del notebook.

## Notas de uso

- Algunas traducciones representan cambios de base o combinaciones lineales. En
  esos casos, la expresión mostrada debe interpretarse como una relación entre
  convenciones, no necesariamente como un nuevo parámetro independiente.
- Las traducciones se fundamentan en las tablas de SMEFTsim 3.0 y en la
  documentación de SMEFT@NLO y dim6top incorporada al módulo.
- MaTeX es opcional. Su ausencia no afecta a las búsquedas ni a las exportaciones
  `.tex`; solo modifica el aspecto de las tablas mostradas dentro de Mathematica.

## Dependencias

- **Mathematica** para cargar y utilizar el módulo.
- **Una distribución LaTeX** —por ejemplo TeX Live o MiKTeX— solo si se desea
  compilar los archivos `.tex` generados.
- **MaTeX** es opcional y únicamente mejora el renderizado en el notebook.
