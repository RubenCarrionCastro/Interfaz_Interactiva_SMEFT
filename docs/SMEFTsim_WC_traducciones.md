# `SMEFTsim_WC_traducciones.wl` — Traductor de coeficientes de Wilson

Módulo auxiliar de **post-procesado**. No forma parte del cálculo y no modifica la
interfaz: únicamente define datos y funciones para interpretar y traducir la lista
de coeficientes de Wilson que produce el módulo principal.

Su utilidad es que los coeficientes que devuelve la interfaz están en la
nomenclatura de **SMEFTsim** (variante `top`/`topU3l`/general), y a menudo interesa
relacionarlos con otras bases de uso común o con sus operadores de la base de
Warsaw.

## Qué proporciona

- **Tabla de traducción** entre los coeficientes de SMEFTsim y los de otras bases:
  - SMEFT@NLO
  - dim6top

  Cada entrada indica el coeficiente o combinación equivalente, su clase, el tipo
  de correspondencia (uno-a-uno, con signo, rotación de base, no mapeado, etc.) y
  notas sobre convenios.
- **Operador asociado** a cada coeficiente (notación de la base de Warsaw), con su
  expresión en LaTeX.
- **Renderizado en LaTeX/MaTeX.** Si MaTeX está instalado y operativo, los nombres
  y operadores se muestran formateados; en caso contrario se usa una notación
  nativa de Mathematica.
- **Exportación a CSV** de las tablas de traducción.

## Funciones principales

| Función | Uso |
|---|---|
| `wcTranslationsDataset[]` | Devuelve la tabla completa de traducciones. |
| `translateFromSMEFTsim["ctWRe"]` | Traduce un coeficiente concreto a todas las bases. |
| `translateWCList[lastWCNames]` | Traduce una lista de coeficientes. |
| `translateToTarget["dim6top"]` | Filtra la tabla por base de destino. |
| `wcCoverageReport[lastWCNames]` | Informe de cobertura: qué WCs se traducen y cuáles no. |
| `untranslatedWCs[lastWCNames]` | Lista de coeficientes sin traducción directa. |
| `translateWCListLatexTable[lastWCNames]` | Tabla con columnas en LaTeX. |
| `translateWCListRenderedTable[lastWCNames]` | Tabla renderizada (MaTeX si está disponible). |
| `translateLastWCResult[]` | Atajo que opera directamente sobre `lastWCNames`/`lastWCResult`. |
| `exportWCTranslationsCSV[ruta]` | Exporta la tabla a CSV. |

## Flujo de uso típico

```mathematica
(* tras ejecutar un cálculo en la interfaz *)
Get["SMEFTsim_WC_traducciones.wl"];

wcCoverageReport[lastWCNames]              (* ¿se traducen todos? *)
translateWCListLatexTable[lastWCNames]     (* WC -> operador -> LaTeX *)
translateWCList[lastWCNames, "dim6top"]    (* solo a la base dim6top *)
```

## Notas

- Es un módulo **independiente y opcional**: la interfaz funciona sin él.
- Algunas correspondencias no son uno-a-uno, sino rotaciones de base o
  combinaciones lineales (p. ej. el dipolo neutro del top `ctZ`); la tabla lo
  indica explícitamente en la columna de tipo.
- Las traducciones se basan en las tablas de los apéndices de la guía práctica de
  SMEFTsim 3.0 y de las referencias de SMEFT@NLO y dim6top.
- El renderizado LaTeX/MaTeX solo afecta a la **visualización**; no altera la tabla
  algebraica de traducción.

## Dependencias

Mathematica. MaTeX (con una instalación de LaTeX) es **opcional** y solo se usa
para el renderizado; sin él, el módulo funciona con notación nativa.
