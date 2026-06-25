(* ::Package:: *)

(* ========================================================= *)
(* Interfaz para diagramas, amplitudes y coeficientes Wilson  *)
(* Versi\[OAcute]n con estado inicial p p expandido a canales part\[OAcute]nicos *)
(* Correcci\[OAcute]n: vista previa segura, exportaci\[OAcute]n PDF independiente y actualizaci\[OAcute]n forzada del FrontEnd *)
(* Validaci\[OAcute]n solo en la carpeta Models situada junto al .wl de la interfaz *)
(* ========================================================= *)

(* ===================== CARGA DE PAQUETES ===================== *)

If[! MemberQ[$Packages, "FeynCalc`"],
  $LoadAddOns = {"FeynArts"};
  << FeynCalc`,
  Print["FeynCalc ya est\[AAcute] cargado. Si necesitas recargar FeynArts/FeynCalc, reinicia el kernel."]
];

SetOptions[FourVector, FeynCalcInternal -> False];

(* Carpeta base de la interfaz.
  Si ejecutas este c\[OAcute]digo como archivo .wl, usa la carpeta donde est\[AAcute] ese .wl.
  Si lo eval\[UAcute]as desde un notebook .nb, usa la carpeta donde est\[AAcute] guardado el notebook.
  Como \[UAcute]ltimo recurso usa Directory[]. *)

ClearAll[directorioInterfaz, faModelsDir];

directorioInterfaz[] := Module[{inputFile, nbDir},
  inputFile = Quiet @ Check[$InputFileName, ""];
  
  If[StringQ[inputFile] && inputFile =!= "" && FileExistsQ[inputFile],
  Return[DirectoryName[inputFile]]
  ];
  
  nbDir = Quiet @ Check[NotebookDirectory[], $Failed];
  
  If[StringQ[nbDir] && DirectoryQ[nbDir],
  Return[nbDir]
  ];
  
  Directory[]
];

(* \[CapitalUAcute]NICA carpeta Models que se va a comprobar.
  Debe estar al mismo nivel que el .wl de la interfaz.

  Ejemplo:

  MiInterfaz/
  WilsonUIManualCatalog.wl
  Models/
  SMEFTsimtopU3l.mod
  SMEFTsimtopU3l.gen
  SMEFTsimtopU3l.pars
*)

faModelsDir[] := FileNameJoin[{directorioInterfaz[], "Models"}];

ClearAll[registrarDirectorioModelosFA];
registrarDirectorioModelosFA[dir_] := Module[{},
  If[! StringQ[dir] || ! DirectoryQ[dir], Return[]];
  
  If[! ValueQ[$ModelPath] || ! ListQ[$ModelPath],
  $ModelPath = {};
  ];
  
  $ModelPath = DeleteDuplicates @ Append[$ModelPath, dir];
  
  If[! ValueQ[$FeynArtsModelPath],
  $FeynArtsModelPath = {dir},
  If[ListQ[$FeynArtsModelPath],
  $FeynArtsModelPath = DeleteDuplicates @ Append[$FeynArtsModelPath, dir],
  $FeynArtsModelPath = DeleteDuplicates @ {$FeynArtsModelPath, dir}
  ]
  ];
];

registrarDirectorioModelosFA[faModelsDir[]];

(* ===================== VALIDACI\[CapitalOAcute]N DE MODELOS ===================== *)

ClearAll[
  limpiarNombreFA,
  directoriosModelosFA,
  ficheroFA,
  validarModelosFA,
  cargarParametrosModeloFA
];

limpiarNombreFA[nombre_, extension_] := Module[{s},
  s = StringTrim @ ToString[nombre, InputForm];
  s = StringReplace[s, "\"" -> ""];
  
  If[StringEndsQ[s, extension],
  s = StringDrop[s, -StringLength[extension]];
  ];
  
  StringTrim[s]
];

(* Solo se comprueba la carpeta Models situada junto al .wl/.nb de la interfaz.
  No se buscan otros paths ni subcarpetas. *)
directoriosModelosFA[] := If[DirectoryQ[faModelsDir[]], {faModelsDir[]}, {}];

ficheroFA[nombre_, extension_] := Module[{n, encontrados},
  n = limpiarNombreFA[nombre, extension];
  
  If[n == "", Return[Missing["NombreVacio"]]];
  
  If[StringContainsQ[n, "/"] || StringContainsQ[n, "\\"],
  Return[Missing["RutaNoPermitida", n]];
  ];
  
  (* B\[UAcute]squeda directa solo dentro de faModelsDir[], sin recorrer subcarpetas. *)
  encontrados = FileNames[n <> extension, directoriosModelosFA[]];
  
  If[encontrados === {},
  Missing["NoEncontrado", n <> extension],
  First[encontrados]
  ]
];

validarModelosFA[model_, genericModel_] := Module[
  {m, gm, modFile, genFile, dirsTxt},
  
  (* Por si la carpeta Models se cre\[OAcute] despu\[EAcute]s de evaluar el archivo. *)
  registrarDirectorioModelosFA[faModelsDir[]];
  
  m = limpiarNombreFA[model, ".mod"];
  gm = limpiarNombreFA[genericModel, ".gen"];
  dirsTxt = StringRiffle[directoriosModelosFA[], "\n"];
  
  If[m == "",
  Return[<|
  "OK" -> False,
  "Mensaje" -> "Debe introducir un valor en el campo Model."
  |>]
  ];
  
  If[gm == "",
  Return[<|
  "OK" -> False,
  "Mensaje" -> "Debe introducir un valor en el campo GenericModel."
  |>]
  ];
  
  If[StringContainsQ[m, "/"] || StringContainsQ[m, "\\"],
  Return[<|
  "OK" -> False,
  "Mensaje" -> "En el campo Model debe escribir solo el nombre del modelo, no una ruta.\n\nEjemplo correcto: SMEFTsimtopU3l"
  |>]
  ];
  
  If[StringContainsQ[gm, "/"] || StringContainsQ[gm, "\\"],
  Return[<|
  "OK" -> False,
  "Mensaje" -> "En el campo GenericModel debe escribir solo el nombre del modelo gen\[EAcute]rico, no una ruta.\n\nEjemplo correcto: SMEFTsimtopU3l"
  |>]
  ];
  
  modFile = ficheroFA[m, ".mod"];
  
  If[! StringQ[modFile],
  Return[<|
  "OK" -> False,
  "Mensaje" -> "No se ha encontrado el modelo indicado en la carpeta Models de FeynArts.\n\nModel introducido: " <> m <>
  "\n\nSe esperaba encontrar el fichero:\n" <> m <> ".mod" <>
  "\n\nCarpetas revisadas:\n" <> dirsTxt
  |>]
  ];
  
  genFile = ficheroFA[gm, ".gen"];
  
  If[! StringQ[genFile],
  Return[<|
  "OK" -> False,
  "Mensaje" -> "No se ha encontrado el GenericModel indicado en la carpeta Models de FeynArts.\n\nGenericModel introducido: " <> gm <>
  "\n\nSe esperaba encontrar el fichero:\n" <> gm <> ".gen" <>
  "\n\nCarpetas revisadas:\n" <> dirsTxt
  |>]
  ];
  
  <|
  "OK" -> True,
  "ModelName" -> m,
  "GenericName" -> gm,
  "ModelFile" -> modFile,
  "GenericFile" -> genFile,
  "Mensaje" -> "Modelo validado correctamente."
  |>
];

(* ===================== CAT\[CapitalAAcute]LOGO DE PART\[CapitalIAcute]CULAS ===================== *)

particleCatalog = <|
  "Leptones" -> {F[2, {1}] -> "e-", -F[2, {1}] -> "e+", 
  F[2, {2}] -> "\[Mu]-", -F[2, {2}] -> "\[Mu]+", 
  F[2, {3}] -> "\[Tau]-", -F[2, {3}] -> "\[Tau]+"}, 
  "Neutrinos" -> {F[1, {1}] -> "\[Nu]e", -F[1, {1}] -> "\[Nu]\:02c9e", 
  F[1, {2}] -> "\[Nu]\[Mu]", -F[1, {2}] -> "\[Nu]\:02c9\[Mu]", 
  F[1, {3}] -> "\[Nu]\[Tau]", -F[1, {3}] -> "\[Nu]\:02c9\[Tau]"}, 
  "Quarks" -> {F[3, {1}] -> "u", -F[3, {1}] -> "u\:0304", 
  F[3, {2}] -> "c", -F[3, {2}] -> "c\:0304", 
  F[4, {1}] -> "d", -F[4, {1}] -> "d\:0304", 
  F[4, {2}] -> "s", -F[4, {2}] -> "s\:0304", 
  F[30] -> "t", -F[30] -> "t\:0304", 
  F[40] -> "b", -F[40] -> "b\:0304"}, 
  "Vectores" -> {V[1] -> "\[Gamma]", V[2] -> "Z", 
  V[3] -> "W+", -V[3] -> "W-", V[4] -> "g"}, 
  "Escalares" -> {S[1] -> "H", S[2] -> "G0", 
  S[3] -> "G+", -S[3] -> "G-"}, 
  "Hadrones" -> {PP -> "p"}
  |>;

menuRules[cat_] := Prepend[(#[[1]] -> #[[2]]) & /@ particleCatalog[cat], "\[LongDash]" -> None];

(* Categor\[IAcute]as disponibles en cada men\[UAcute].
  Hadrones solo se permite como estado entrante, para representar p p.
  No se permite como part\[IAcute]cula saliente porque FeynArts no recibe protones finales. *)
inputCategoryRules[] := Thread[Keys[particleCatalog] -> Keys[particleCatalog]];
outputCategoryRules[] := Module[{cats},
  cats = DeleteCases[Keys[particleCatalog], "Hadrones"];
  Thread[cats -> cats]
];

(* ===================== PROTONES Y CANALES PART\[CapitalOAcute]NICOS ===================== *)

(*
  FeynArts no trata el prot\[OAcute]n como part\[IAcute]cula fundamental.
  Aqu\[IAcute] PP es un s\[IAcute]mbolo interno para que el usuario pueda elegir "p" en la interfaz.
  Antes de llamar a InsertFields, PP PP se expande a canales part\[OAcute]nicos.

  Opciones incluidas:
  - "ttbarLO": gg y q qbar con q = u,d,s,c. En pp no se duplica q qbar y qbar q.
  - "4FS": todos los pares part\[OAcute]nicos de {u,d,s,c, anti-u, anti-d, anti-s, anti-c, g}.
  - "5FS": igual que 4FS, pero a\[NTilde]ade b y anti-b.
  - "6FSFormal": igual que 5FS, pero a\[NTilde]ade t y anti-t. No representa un PDF f\[IAcute]sico est\[AAcute]ndar del prot\[OAcute]n; es solo una opci\[OAcute]n formal.
*)

ClearAll[
  PP,
  lightQuarksSMEFTsimtop,
  bottomPartonsSMEFTsimtop,
  topPartonsSMEFTsimtop,
  gluonParton,
  protonPartons,
  qqbarLightInitialStates,
  ppInitialStates,
  canonicalInitialChannelFA,
  uniqueInitialChannelsFA,
  expandInitialStatesFA,
  particleLabelFA,
  channelLabelFA
];

lightQuarksSMEFTsimtop = {
  F[3, {1}],  (* u *)
  F[4, {1}],  (* d *)
  F[4, {2}],  (* s *)
  F[3, {2}]  (* c *)
};

bottomPartonsSMEFTsimtop = {F[40], -F[40]};
topPartonsSMEFTsimtop = {F[30], -F[30]};
gluonParton = {V[4]};

protonPartons[scheme_] := Switch[
  scheme,
  "4FS",
  Join[lightQuarksSMEFTsimtop, -lightQuarksSMEFTsimtop, gluonParton],
  
  "5FS",
  Join[lightQuarksSMEFTsimtop, -lightQuarksSMEFTsimtop, bottomPartonsSMEFTsimtop, gluonParton],
  
  "6FSFormal",
  Join[lightQuarksSMEFTsimtop, -lightQuarksSMEFTsimtop, bottomPartonsSMEFTsimtop, topPartonsSMEFTsimtop, gluonParton],
  
  _,
  Join[lightQuarksSMEFTsimtop, -lightQuarksSMEFTsimtop, bottomPartonsSMEFTsimtop, gluonParton]
];

(*
  En una colisi\[OAcute]n p p los dos protones son haces id\[EAcute]nticos.
  Para el objetivo de esta interfaz \[LongDash]identificar los coeficientes de Wilson
  que aparecen\[LongDash] no necesitamos evaluar a la vez q qbar y qbar q, ni q g y g q.
  Esta funci\[OAcute]n ordena can\[OAcute]nicamente cada canal inicial y permite eliminar
  duplicados bajo intercambio de los dos partones entrantes.
*)

canonicalInitialChannelFA[channel_List] := SortBy[channel, ToString[#, InputForm] &];

uniqueInitialChannelsFA[channels_List] := DeleteDuplicatesBy[
  channels,
  canonicalInitialChannelFA
];

qqbarLightInitialStates[] := uniqueInitialChannelsFA @ Join[
  ({#, -#} & /@ lightQuarksSMEFTsimtop),
  {{V[4], V[4]}}
];

ppInitialStates[scheme_] := uniqueInitialChannelsFA @ Tuples[protonPartons[scheme], 2];

expandInitialStatesFA[inList_, ppScheme_] := Module[{channels, partons, expandedLists, nProtons},
  
  (* Si no hay protones, el proceso se pasa tal cual a FeynArts. *)
  If[! MemberQ[inList, PP],
  Return[<|"OK" -> True, "Channels" -> {inList}, "Mensaje" -> "Proceso part\[OAcute]nico ordinario."|>]
  ];
  
  nProtons = Count[inList, PP];
  
  (*
  Cada PP se expande a partones del prot\[OAcute]n, pero el resto de part\[IAcute]culas
  iniciales se preservan. As\[IAcute] se permiten procesos como:
  p t  -> {u t, d t, ..., g t}
  p e-  -> {u e-, d e-, ..., g e-}
  p p  -> pares part\[OAcute]nicos del prot\[OAcute]n
  FeynArts nunca recibe PP; siempre recibe quarks/gluones/part\[IAcute]culas elementales.
  *)
  
  channels = If[inList === {PP, PP},
  Switch[
  ppScheme,
  "ttbarLO", qqbarLightInitialStates[],
  "4FS", ppInitialStates["4FS"],
  "5FS", ppInitialStates["5FS"],
  "6FSFormal", ppInitialStates["6FSFormal"],
  _, qqbarLightInitialStates[]
  ],
  
  partons = Switch[
  ppScheme,
  "ttbarLO", protonPartons["4FS"],
  "4FS", protonPartons["4FS"],
  "5FS", protonPartons["5FS"],
  "6FSFormal", protonPartons["6FSFormal"],
  _, protonPartons["5FS"]
  ];
  
  expandedLists = If[# === PP, partons, {#}] & /@ inList;
  Tuples[expandedLists]
  ];
  
  <|
  "OK" -> True,
  "Channels" -> channels,
  "Mensaje" -> "Estado inicial con " <> ToString[nProtons] <> " prot\[OAcute]n(es) expandido a " <> ToString[Length[channels]] <> " canales part\[OAcute]nicos."
  |>
];

particleLabelFA[x_] := Module[{rules, lab},
  rules = Flatten[Values[particleCatalog]];
  lab = x /. rules;
  If[StringQ[lab], lab, ToString[x, InputForm]]
];

channelLabelFA[ch_List] := StringRiffle[particleLabelFA /@ ch, " "];

(*
  La opci\[OAcute]n de esquema p p solo debe mostrarse si el usuario est\[AAcute] usando
  hadrones en las part\[IAcute]culas entrantes. No tiene sentido mostrarla en procesos
  puramente part\[OAcute]nicos como t tbar, leptones, etc.
*)
usesHadronInitialQ[cats_, picks_, n_] := Module[{activeCats, activePicks},
  activeCats = Take[cats, n];
  activePicks = Take[picks, n];
  MemberQ[activeCats, "Hadrones"] || MemberQ[activePicks, PP]
];

(* ===================== FUNCIONES AUXILIARES ===================== *)

(* Utilidades seguras para la interfaz din\[AAcute]mica y la vista previa.
  Evitan que objetos abortados/terminados por el FrontEnd acaben dentro
  de la zona donde se dibujan los diagramas. *)

ClearAll[
  safeProgressValueFA,
  safeMessageStringFA,
  safeStatusFA,
  forceFrontEndUpdateFA,
  withoutNotebookPrintsFA,
  fcfaOptionIfExistsFA,
  fcfaConvertExtraOptionsFA,
  safeSimplifyFA
];

safeProgressValueFA[x_] := Module[{y},
  y = Quiet @ Check[N[x], 0.];
  If[! NumericQ[y], y = 0.];
  Clip[y, {0., 1.}]
];

safeMessageStringFA[msg_] := If[StringQ[msg], msg, Quiet @ Check[ToString[msg, InputForm], ""]];

safeStatusFA[msg_, color_: Black] := Style[safeMessageStringFA[msg], color];

(* Listas de estado seguras: evitan errores de StringRiffle cuando una lista
  contiene objetos no string, $Failed, Nothing, etc. *)
ClearAll[safeStringListFA, safeShortListFA, safeCountFA];

safeStringListFA[x_] := Module[{lst},
  lst = Quiet @ Check[Flatten @ List[x], {}];
  lst = DeleteCases[lst, Null | Nothing | $Failed | $Aborted | $Canceled];
  DeleteDuplicates @ (safeMessageStringFA /@ lst)
];

safeShortListFA[x_, max_: 8] := Module[{lst = safeStringListFA[x]},
  If[lst === {}, "", StringRiffle[Take[lst, UpTo[max]], ", "]]
];

safeCountFA[x_] := Length @ safeStringListFA[x];


(*
  Fuerza al FrontEnd a procesar objetos Dynamic antes de entrar en llamadas
  largas como CreateFeynAmp o FCFAConvert.

  Limitaci\[OAcute]n importante: mientras el kernel est\[AAcute] dentro de una llamada pesada,
  Mathematica no puede refrescar la interfaz. Esta funci\[OAcute]n asegura que el \[UAcute]ltimo
  mensaje de estado quede pintado antes de comenzar esa llamada.
*)
forceFrontEndUpdateFA[] := Quiet @ Check[
  FrontEndExecute[
  FrontEnd`UpdateDynamicObjects[EvaluationNotebook[]]
  ],
  Null
  ];

SetAttributes[withoutNotebookPrintsFA, HoldFirst];
withoutNotebookPrintsFA[expr_] := Block[{Print}, Print[___] := Null; expr];

(* Opci\[OAcute]n compatible entre versiones de FeynCalc:
  en versiones antiguas puede no existir DropIndexSum. *)
fcfaOptionIfExistsFA[opt_Symbol, value_] := If[
  MemberQ[First /@ Options[FCFAConvert], opt],
  {opt -> value},
  {}
];

fcfaConvertExtraOptionsFA[] := DeleteDuplicatesBy[
  Flatten @ {
  fcfaOptionIfExistsFA[DropIndexSum, False],
  fcfaOptionIfExistsFA[DropSumOver, True],
  fcfaOptionIfExistsFA[SMP, True],
  fcfaOptionIfExistsFA[Contract, True],
  fcfaOptionIfExistsFA[UndoChiralSplittings, True]
  },
  First
];

(*
  Simplificaci\[OAcute]n segura.

  En procesos 2 -> 3 como p p -> t tbar H, las expresiones despu\[EAcute]s de
  FCFAConvert pueden ser muy grandes. Una llamada directa a Simplify puede
  agotar el tiempo de c\[AAcute]lculo y emitir mensajes como Simplify::time.

  Esta funci\[OAcute]n permite tres modos:
  - "None" : no simplifica.
  - "Light": Simplify con tiempo limitado.
  - "Full" : FullSimplify con tiempo limitado.

  Si la simplificaci\[OAcute]n falla o expira, devuelve la expresi\[OAcute]n original
  para que el c\[AAcute]lculo pueda continuar.
*)
safeSimplifyFA[expr_, mode_: "Light", seconds_: 30] := Module[
  {sec, res},

  sec = Quiet @ Check[N[seconds], 30];
  If[! NumericQ[sec] || sec <= 0, sec = 30];

  Switch[
  mode,

  "None",
  expr,

  "Full",
  res = TimeConstrained[
  Quiet @ Check[
  FullSimplify[expr, TimeConstraint -> sec],
  $Failed
  ],
  sec + 5,
  $TimedOut
  ];

  If[res === $TimedOut || res === $Failed || res === $Aborted,
  expr,
  res
  ],

  _, (* "Light" por defecto *)
  res = TimeConstrained[
  Quiet @ Check[
  Simplify[expr, TimeConstraint -> sec],
  $Failed
  ],
  sec + 5,
  $TimedOut
  ];

  If[res === $TimedOut || res === $Failed || res === $Aborted,
  expr,
  res
  ]
  ]
];

(* Se eliminan las funciones de rasterizaci\[OAcute]n/NotebookWrite de Paint.
  La exportaci\[OAcute]n estable usa directamente Paint[...] + Export[..., "PDF"],
  igual que en la interfaz preliminar que funcionaba. *)



(* ===================== COEFICIENTES DE WILSON ===================== *)

(*
  WC2 contiene exclusivamente los parametros externos e independientes del
  modelo. No se incorporan los aliases internos complejos de M$IntParams,
  porque no son grados de libertad adicionales: por ejemplo

      ctH = ctHRe + I ctHIm .

  Esto es importante tanto para el filtro lineal como para la extraccion de
  resultados: contar simultaneamente ctHRe, ctHIm y ctH como WCs distintos
  puede producir falsos grados cuadraticos o resultados duplicados.
*)
WCext = {};
WCint = {};
WC1 = {};
WC2 = {};
wcAliasRulesFA = {};
wcRealComponentRulesFA = {};

ClearAll[
  wcQ,
  wcLikeQ,
  symbolFromModelNameFA,
  complexWCBaseNamesFA,
  buildWCModelAliasRulesFA,
  normalizarWCsModeloFA,
  cpViolatingWCQFA,
  imaginaryWCComponentQFA
];

wcQ[x_] := MemberQ[WC2, x];

(* Igual que wcQ pero reconoce las tres convenciones de conjugacion que
  pueden aparecer antes o despues de FCFAConvert. *)
wcLikeQ[s_Symbol] := wcQ[s];
wcLikeQ[HC[s_]] := wcQ[s];
wcLikeQ[Conjugate[s_]] := wcQ[s];
wcLikeQ[ComplexConjugate[s_]] := wcQ[s];
wcLikeQ[_] := False;

(* Construye de manera controlada el simbolo que FeynArts usa en el .mod.
  Los nombres proceden exclusivamente de M$ExtParams ya cargado, por lo que
  no se evalua texto arbitrario introducido por el usuario. *)
symbolFromModelNameFA[name_String] := If[
  StringMatchQ[name, RegularExpression["^[A-Za-z$][A-Za-z0-9$]*$"]],
  Symbol[name],
  $Failed
];

(* Bases complejas que el modelo parametriza mediante pares baseRe/baseIm. *)
complexWCBaseNamesFA[] := Module[{names, candidateBases},
  names = SymbolName /@ WC2;
  candidateBases = DeleteDuplicates @ (
    StringReplace[
      Select[names, StringEndsQ[#, "Re"] || StringEndsQ[#, "Im"] &],
      RegularExpression["(Re|Im)$"] -> ""
    ]
  );
  Select[
    candidateBases,
    MemberQ[names, # <> "Re"] && MemberQ[names, # <> "Im"] &
  ]
];

(*
  SMEFTsim usa pares externos Re/Im para los WCs complejos, mientras que el
  .mod puede contener el alias complejo correspondiente. Esto incluye la
  inconsistencia exportada ctH/ctHH: el .mod usa ctH y los parametros de
  entrada son ctHRe y ctHIm. Las reglas se derivan de los pares Re/Im, por
  tanto corrigen ctH sin codificar una lista fragil de casos especiales.
*)
buildWCModelAliasRulesFA[] := Module[
  {bases, triples, hcRules, ccRules, conjRules, bareRules},

  bases = complexWCBaseNamesFA[];
  triples = DeleteCases[
    Function[base,
      Module[{raw, rePart, imPart},
        raw = symbolFromModelNameFA[base];
        rePart = SelectFirst[WC2, SymbolName[#] === base <> "Re" &, Missing["NoRe"]];
        imPart = SelectFirst[WC2, SymbolName[#] === base <> "Im" &, Missing["NoIm"]];
        If[raw === $Failed || MissingQ[rePart] || MissingQ[imPart],
          Nothing,
          {raw, rePart, imPart}
        ]
      ]
    ] /@ bases,
    Nothing
  ];

  (* Las reglas de HC/Conjugate deben preceder a la regla desnuda para que
     HC[ctH] se convierta directamente en ctHRe - I ctHIm. *)
  hcRules = (HoldPattern[HC[#[[1]]]] :> #[[2]] - I #[[3]] &) /@ triples;
  ccRules = (HoldPattern[ComplexConjugate[#[[1]]]] :> #[[2]] - I #[[3]] &) /@ triples;
  conjRules = (HoldPattern[Conjugate[#[[1]]]] :> #[[2]] - I #[[3]] &) /@ triples;
  bareRules = (#[[1]] -> #[[2]] + I #[[3]] &) /@ triples;

  wcAliasRulesFA = Join[hcRules, ccRules, conjRules, bareRules];

  (* Re/Im son parametros de entrada reales. Esta identidad es estructural,
     no una hipotesis adicional sobre CP. *)
  wcRealComponentRulesFA = Join[
    (HoldPattern[HC[#]] :> # &) /@ WC2,
    (HoldPattern[ComplexConjugate[#]] :> # &) /@ WC2,
    (HoldPattern[Conjugate[#]] :> # &) /@ WC2
  ];
];

normalizarWCsModeloFA[expr_] := Module[{rules},
  rules = Join[wcAliasRulesFA, wcRealComponentRulesFA];
  If[rules === {},
    expr,
    Quiet @ Check[expr /. rules, expr]
  ]
];

cpViolatingWCQFA[wc_] := Module[{n = SymbolName[Unevaluated[wc]]},
  StringEndsQ[n, "Im"] || StringEndsQ[n, "til"]
];

(* "WCs reales" en esta parametrizacion equivale a anular las componentes
   independientes que terminan en Im. Los operadores con sufijo til no se
   anulan aqui: son CP-odd pero sus coeficientes son parametros reales. *)
imaginaryWCComponentQFA[wc_] := StringEndsQ[SymbolName[Unevaluated[wc]], "Im"];

cargarParametrosModeloFA[model_] := Module[
  {m, parsFile, coefNamesExtLocal, coefNamesIntLocal},

  m = limpiarNombreFA[model, ".mod"];
  parsFile = ficheroFA[m, ".pars"];

  If[! StringQ[parsFile],
    WCext = {};
    WCint = {};
    WC1 = {};
    WC2 = {};
    wcAliasRulesFA = {};
    wcRealComponentRulesFA = {};
    Return[<|
      "OK" -> False,
      "Mensaje" -> "No se ha encontrado el fichero .pars del modelo. Los diagramas pueden generarse, pero no se podran extraer WCs automaticamente.",
      "ParsFile" -> Missing["NoEncontrado"]
    |>]
  ];

  Quiet @ Check[Get[parsFile],
    WCext = {};
    WCint = {};
    WC1 = {};
    WC2 = {};
    wcAliasRulesFA = {};
    wcRealComponentRulesFA = {};
    Return[<|
      "OK" -> False,
      "Mensaje" -> "Se encontro el fichero .pars, pero fallo su carga.",
      "ParsFile" -> parsFile
    |>]
  ];

  coefNamesExtLocal = If[ValueQ[M$ExtParams] && ListQ[M$ExtParams], First /@ M$ExtParams, {}];
  coefNamesIntLocal = If[ValueQ[M$IntParams] && ListQ[M$IntParams], First /@ M$IntParams, {}];

  WCext = DeleteCases[coefNamesExtLocal,
    aS | Gf | LambdaSMEFT | linearPropCorrections | MW | ymb | ymc |
    ymdo | yme | ymm | yms | ymt | ymtau | ymup
  ];

  (* Se conserva la lista interna solo para diagnostico/documentacion. No se
     mezcla con WC2: los alias internos se normalizan a parametros externos. *)
  WCint = DeleteCases[coefNamesIntLocal,
    yup | yc | ydo | ys | yt | yb | MWsm | aEW | vevhat | lam | dkH |
    cth | dMH2 | G | ee | ye | ym | ytau | dGf | vevT | barlam | vev |
    sth2 | sth | dMZ2 | gw | g1 | dg1 | dgw | gwsh | g1sh | propCorr |
    MZ1 | MW1 | MH1 | MT1 | WZ1 | WW1 | WH1 | WT1 | dWZ | dWW | dWH |
    dWT | yt0 | yb0 | gHgg1 | gHgg2 | gHgg3 | gHgg4 | gHgg5 | gHaa |
    gHza
  ];

  WC1 = WCext;
  WC2 = DeleteDuplicates[WC1];
  buildWCModelAliasRulesFA[];

  <|
    "OK" -> True,
    "Mensaje" -> "Parametros del modelo cargados correctamente.",
    "ParsFile" -> parsFile,
    "NumeroWC" -> Length[WC2]
  |>
];

(* Carga inicial de par\[AAcute]metros del modelo por defecto, si existe. *)
cargarParametrosModeloFA["SMEFTsimtopU3l"];

(* ===================== TOGGLES PARA REGLAS DE SIMETR\[CapitalIAcute]A ===================== *)

(*
  Bloque de simplificaciones recuperado de WilsonUI.wl.

  Motivo:
  La versi\[OAcute]n m\[AAcute]s "segura" usaba una linealizaci\[OAcute]n mediante
  Expand + Series en todos los WCs. Para procesos grandes, especialmente
  procesos hadr\[OAcute]nicos y 2 -> 3, eso puede hacer que la amplitud crezca
  demasiado y que el c\[AAcute]lculo tarde mucho o aborte.

  Reglas disponibles:
  - CP: pone a cero s\[IAcute]mbolos terminados en Im o til.
  - WCs reales: anula los parametros independientes terminados en Im; las conjugaciones de componentes Re/Im se normalizan de forma estructural.
  - Leptones sin masa: anula masas y Yukawas lept\[OAcute]nicos (l\[IAcute]mite de alta energ\[IAcute]a).
  - Lineal en WCs: conserva solo grado <= 1 en WCs (interferencia con el SM).
  Se aplica la \[UAcute]ltima y como selecci\[OAcute]n de t\[EAcute]rminos por grado (r\[AAcute]pido).

  Nota:
  No se aplica Simplify dentro de applySelectedSymmetries, porque la interfaz
  ya tiene safeSimplifyFA antes y despu\[EAcute]s de aplicar las reglas.
*)

useCpRules = True;
useRealRules = True;
useMasslessLeptonRules = True;
useLinearWCRules = True;
useTopSectorRules = True;

ClearAll[
  topSectorWCQFA,
  topSectorWCsFA,
  activeWCsFA,
  inactiveTopSectorWCRulesFA,
  restrictToActiveWCSectorFA,
  buildSymmetryRules,
  applySelectedSymmetries,
  wcAppearsInExprFA,
  wcDegreeFA,
  keepLinearWCFA
];

(* ------------------------------------------------------------------------- *)
(* Sector activo de Wilson coefficients  *)
(* ------------------------------------------------------------------------- *)
(*
  Top-sector no es un filtro posterior de resultados. Cuando se activa,
  los WCs que no pertenecen al sector top se ponen a cero ANTES de construir
  |M|^2. Esto reproduce la idea de trabajar con un submodelo/restriction card:
  la amplitud se genera y procesa dentro del espacio de parametros elegido.

  Criterio practico para SMEFTsim top/topU3l:
  - cHQ*, cHt*, cHbq, cHtb* : corrientes Higgs-top/bottom pesadas
  - ct*                    : top Yukawa/dipolos/four-fermion con t_R
  - cQ*                    : operadores con doblete pesado Q
  Se excluyen automaticamente bosonicos universales (cHbox, cHDD, cHWB),
  leptonicos puros (cHl*, cHe*, ceB, ceW, ceH) y escalares/tensores
  semileptonicos cle* que no pertenecen al subespacio vectorial top.
*)

topSectorWCQFA[wc_] := Module[{n = SymbolName[Unevaluated[wc]]},
  StringStartsQ[n, "cHQ"] ||
  StringStartsQ[n, "cHt"] ||
  n === "cHbq" ||
  StringStartsQ[n, "cHtb"] ||
  StringStartsQ[n, "ct"] ||
  StringStartsQ[n, "cQ"]
];

topSectorWCsFA[] := Select[WC2, topSectorWCQFA];

activeWCsFA[] := If[TrueQ[useTopSectorRules], topSectorWCsFA[], WC2];

inactiveTopSectorWCRulesFA[] := Module[{inactive},
  inactive = Complement[WC2, topSectorWCsFA[]];
  Join[
    Thread[inactive -> 0],
    Thread[(HC /@ inactive) -> 0],
    Thread[(ComplexConjugate /@ inactive) -> 0],
    Thread[(Conjugate /@ inactive) -> 0]
  ]
];

restrictToActiveWCSectorFA[expr_] := If[TrueQ[useTopSectorRules],
  Quiet @ Check[normalizarWCsModeloFA[expr] /. inactiveTopSectorWCRulesFA[], expr],
  normalizarWCsModeloFA[expr]
];

(* Grado total en WCs de un monomio: suma de exponentes de los factores que son
   WCs (o WCs conjugados). Cuenta tanto C_i C_j como C_i^2. *)
wcDegreeFA[term_] := Module[{factors},
  factors = If[Head[term] === Times, List @@ term, {term}];
  Total[
    Map[
      Function[f,
        Which[
          wcLikeQ[f], 1,
          MatchQ[f, Power[g_, n_Integer] /; (Positive[n] && wcLikeQ[g])], f[[2]],
          True, 0
        ]
      ],
      factors
    ]
  ]
];

(* Truncamiento lineal en WCs (interferencia SM x dim-6): conserva solo los
   monomios con grado en WCs <= 1. Implementado como seleccion de terminos en
   tiempo lineal sobre la expresion expandida. *)
keepLinearWCFA[expr_, seconds_: 20] := Module[{sec, res},
  sec = Quiet @ Check[N[seconds], 20];
  If[! NumericQ[sec] || sec <= 0, sec = 20];

  res = TimeConstrained[
    Quiet @ Check[
      Module[{e},
        e = Expand[normalizarWCsModeloFA[expr]];
        If[Head[e] === Plus,
          Total @ Select[List @@ e, wcDegreeFA[#] <= 1 &],
          If[wcDegreeFA[e] <= 1, e, 0]
        ]
      ],
      expr
    ],
    sec,
    expr
  ];

  res
];

buildSymmetryRules[] := Module[
  {
    cpOddWCs,
    imagWCs,
    cpRulesLocal,
    realRulesLocal,
    leptonMasslessRulesLocal,
    topSectorRulesLocal
  },

  (* Se limita CP a WCs independientes del modelo. Asi no se altera por
     nombre ningun simbolo cinemático o auxiliar ajeno a SMEFTsim. *)
  cpOddWCs = Select[WC2, cpViolatingWCQFA];
  imagWCs = Select[WC2, imaginaryWCComponentQFA];

  cpRulesLocal = Join[
    Thread[cpOddWCs -> 0],
    Thread[(HC /@ cpOddWCs) -> 0],
    Thread[(ComplexConjugate /@ cpOddWCs) -> 0],
    Thread[(Conjugate /@ cpOddWCs) -> 0]
  ];

  (* En SMEFTsim los WCs complejos se introducen como pares Re/Im. Imponer
     WCs reales significa fijar a cero solo los parametros independientes Im;
     no elimina los operadores CP-odd reales de tipo c...til. *)
  realRulesLocal = Join[
    Thread[imagWCs -> 0],
    Thread[(HC /@ imagWCs) -> 0],
    Thread[(ComplexConjugate /@ imagWCs) -> 0],
    Thread[(Conjugate /@ imagWCs) -> 0]
  ];

  (* Leptones sin masa (limite de alta energia): se anulan las masas y los
     Yukawas de los leptones cargados. F[1] son neutrinos sin masa; F[2] son
     leptones cargados en SMEFTsimtopU3l, segun M$ClassesDescription. *)
  leptonMasslessRulesLocal = {
    Me -> 0, MMU -> 0, MTA -> 0,
    yme -> 0, ymm -> 0, ymtau -> 0,
    ye -> 0, ym -> 0, ytau -> 0,
    (* En SMEFTsimtopU3l los vertices con leptones usan la matriz
       de Yukawa indexada yl[i,j], no solo los eigenvalores ye/ym/ytau. *)
    HoldPattern[yl[___]] :> 0,
    Mass[F[2, {_}]] -> 0,
    Mass[F[1, {_}]] -> 0,
    Mass[F[2, {_}, ___]] -> 0,
    Mass[F[1, {_}, ___]] -> 0
  };

  topSectorRulesLocal = inactiveTopSectorWCRulesFA[];

  <|
    "CP" -> cpRulesLocal,
    "Real" -> realRulesLocal,
    "LeptonMassless" -> leptonMasslessRulesLocal,
    "TopSector" -> topSectorRulesLocal
  |>
];

applySelectedSymmetries[expr_, skipLinear_: False] := Module[{tmp, rules},
  tmp = normalizarWCsModeloFA[expr];
  rules = buildSymmetryRules[];

  If[useMasslessLeptonRules, tmp = tmp /. rules["LeptonMassless"]];
  If[useCpRules, tmp = tmp /. rules["CP"]];
  If[useRealRules, tmp = tmp /. rules["Real"]];
  If[useTopSectorRules, tmp = tmp /. rules["TopSector"]];

  (* Si |M|^2 se ha construido ya como interferencia lineal SM x dim-6, no se
     vuelve a aplicar la linealidad: evitaria un Expand enorme y duplicado. *)
  If[useLinearWCRules && ! TrueQ[skipLinear],
    tmp = keepLinearWCFA[tmp, 20]
  ];

  tmp
];

(* Deteccion de aparicion de WCs en una amplitud procesada. *)
wcAppearsInExprFA[expr_, wc_] :=
  ! FreeQ[expr, wc] ||
  ! FreeQ[expr, HC[wc]] ||
  ! FreeQ[expr, ComplexConjugate[wc]] ||
  ! FreeQ[expr, Conjugate[wc]];


(* ===================== AMPLITUD AL CUADRADO |M|^2 ===================== *)

(*
  Construcci\[OAcute]n de |M|^2 a partir de la amplitud M ya convertida a FeynCalc.

  Objetivo de la interfaz: identificar qu\[EAcute] coeficientes de Wilson contribuyen
  al observable. Por eso las simetr\[IAcute]as/filtros y la b\[UAcute]squeda de WCs se aplican
  sobre |M|^2, no sobre M. En |M|^2 aparecen:
  - t\[EAcute]rminos de interferencia SM x dim-6 (lineales en los WCs),
  - t\[EAcute]rminos cuadr\[AAcute]ticos dim-6 x dim-6 (C_i C_j).
  El filtro "Lineal en WCs" deja solo la interferencia (truncamiento lineal SMEFT).

  Dos modos:
  - "Estructural": |M|^2 = M ComplexConjugate[M] con denominadores expl\[IAcute]citos
  y Expand. R\[AAcute]pido y robusto. No eval\[UAcute]a trazas de Dirac, as\[IAcute] que conserva
  todo el contenido en WCs (puede ser ligeramente sobre-inclusivo si alguna
  estructura se cancelar\[IAcute]a tras sumar esp\[IAcute]n).
  - "Fisico": suma de esp\[IAcute]n de fermiones externos, suma de polarizaciones de
  vectores externos, trazas de Dirac evaluadas y, opcionalmente, suma de color.
  M\[AAcute]s caro; identifica cancelaciones reales. Si falla o expira, cae a
  "Estructural" y, en \[UAcute]ltimo extremo, a M ComplexConjugate[M] sin procesar.

  Notas:
  - Es la suma sobre esp\[IAcute]n/polarizaci\[OAcute]n/color, SIN promediar por los estados
  iniciales. El factor de promedio es una constante por proceso que no cambia
  qu\[EAcute] WCs aparecen.
  - Para vectores no masivos (fot\[OAcute]n/glu\[OAcute]n) se usa el reemplazo -g_{mu nu}
  (DoPolarizationSums[..., k, 0]). El conjunto de WCs es invariante gauge, as\[IAcute]
  que basta para identificarlos aunque para gluones no sea el c\[AAcute]lculo f\[IAcute]sico
  completo (en gauge covariante faltar\[IAcute]an los fantasmas).
*)

ClearAll[
  fieldKindFA,
  fieldMassFA,
  externalVectorDataFA,
  externalIncomingMomentaFA,
  externalOutgoingMomentaFA,
  zeroAllWCsFA,
  preprocessAmplitudeForM2FA,
  linearAmplitudePartFA,
  tryFeynAmpDenominatorExplicitFA,
  set2to2KinematicsIfPossibleFA,
  computeAmplitudeSquaredFA
];

$m2PhysicalIncompleteFA = False;
$m2ActualModeFA = "Unknown";
$m2LinearBuiltFA = False;

(* Usar s\[IAcute]mbolos at\[OAcute]micos como momentos externos. Es m\[AAcute]s robusto para
  FermionSpinSum/SetMandelstam que p[1], p[2], ... *)
externalIncomingMomentaFA[n_Integer] := Table[ToExpression["pIn" <> ToString[i]], {i, 1, n}];
externalOutgoingMomentaFA[n_Integer] := Table[ToExpression["pOut" <> ToString[i]], {i, 1, n}];

fieldKindFA[field_] := Module[{f},
  f = field /. Times[-1, x_] :> x;
  Which[
  MatchQ[f, V[1]],
  <|"isVector" -> True,  "massless" -> True,  "colored" -> False, "isFermion" -> False|>,
  MatchQ[f, V[4]],
  <|"isVector" -> True,  "massless" -> True,  "colored" -> True,  "isFermion" -> False|>,
  MatchQ[f, V[2] | V[3]],
  <|"isVector" -> True,  "massless" -> False, "colored" -> False, "isFermion" -> False|>,
  MatchQ[f, F[3, _] | F[4, _] | F[30] | F[40]],
  <|"isVector" -> False, "massless" -> False, "colored" -> True,  "isFermion" -> True|>,
  MatchQ[f, F[1, _] | F[2, _]],
  <|"isVector" -> False, "massless" -> False, "colored" -> False, "isFermion" -> True|>,
  True,
  <|"isVector" -> False, "massless" -> False, "colored" -> False, "isFermion" -> False|>
  ]
];

fieldMassFA[field_] := Module[{f},
  f = field /. Times[-1, x_] :> x;
  Which[
  MatchQ[f, F[1, _]], 0,
  MatchQ[f, F[2, {1}]], If[TrueQ[useMasslessLeptonRules], 0, Me],
  MatchQ[f, F[2, {2}]], If[TrueQ[useMasslessLeptonRules], 0, MMU],
  MatchQ[f, F[2, {3}]], If[TrueQ[useMasslessLeptonRules], 0, MTA],
  MatchQ[f, F[3, {1}]], 0,
  MatchQ[f, F[3, {2}]], 0,
  MatchQ[f, F[4, {1}]], 0,
  MatchQ[f, F[4, {2}]], 0,
  MatchQ[f, F[30]], MT,
  MatchQ[f, F[40]], MB,
  MatchQ[f, V[1] | V[4]], 0,
  MatchQ[f, V[2]], MZ,
  MatchQ[f, V[3]], MW,
  MatchQ[f, S[1]], MH,
  True, 0
  ]
];

externalVectorDataFA[fields_List, momenta_List] := DeleteCases[
  MapThread[
  Function[{fld, mom},
  With[{k = fieldKindFA[fld]},
  If[TrueQ[k["isVector"]], {mom, TrueQ[k["massless"]]}, Nothing]
  ]
  ],
  {fields, momenta}
  ],
  Nothing
];

zeroAllWCsFA[expr_] := Module[{tmp},
  tmp = normalizarWCsModeloFA[expr];
  Quiet @ Check[
    tmp /. Join[
      Thread[WC2 -> 0],
      Thread[(HC /@ WC2) -> 0],
      Thread[(ComplexConjugate /@ WC2) -> 0],
      Thread[(Conjugate /@ WC2) -> 0]
    ],
    tmp
  ]
];

preprocessAmplitudeForM2FA[amp_] := Module[{tmp, rules},
  tmp = normalizarWCsModeloFA[amp];
  rules = buildSymmetryRules[];

  (* Reglas baratas antes de construir productos grandes. *)
  If[useMasslessLeptonRules, tmp = tmp /. rules["LeptonMassless"]];
  If[useCpRules, tmp = tmp /. rules["CP"]];
  If[useRealRules, tmp = tmp /. rules["Real"]];
  If[useTopSectorRules, tmp = tmp /. rules["TopSector"]];

  tmp
];

linearAmplitudePartFA[amp_, seconds_: 20] := Module[
  {sec, eps, rules, res, sm, tmpAmp},
  sec = Quiet @ Check[N[seconds], 20];
  If[! NumericQ[sec] || sec <= 0, sec = 20];

  tmpAmp = normalizarWCsModeloFA[amp];
  sm = zeroAllWCsFA[tmpAmp];
  eps = Unique["epsWC$"];

  rules = Join[
    Thread[activeWCsFA[] -> eps activeWCsFA[]],
    Thread[(HC /@ activeWCsFA[]) -> eps (HC /@ activeWCsFA[])],
    Thread[(ComplexConjugate /@ activeWCsFA[]) -> eps (ComplexConjugate /@ activeWCsFA[])],
    Thread[(Conjugate /@ activeWCsFA[]) -> eps (Conjugate /@ activeWCsFA[])]
  ];

  res = TimeConstrained[
    Quiet @ Check[
      Module[{tmp},
        tmp = tmpAmp /. rules;
        (* Series suele ser mas barato que expandir toda la amplitud cuadrada. *)
        Coefficient[Normal @ Series[tmp, {eps, 0, 1}], eps, 1] /. eps -> 1
      ],
      $Failed
    ],
    sec,
    $TimedOut
  ];

  If[res === $TimedOut || res === $Failed || res === $Aborted,
    (* En SMEFTsim a arbol cada insercion NP es lineal en un WC. Como fallback,
       amp - SM es conservador y evita abortar. *)
    Quiet @ Check[tmpAmp - sm, tmpAmp],
    res
  ]
];

tryFeynAmpDenominatorExplicitFA[expr_, seconds_: 20] := Module[{sec, res},
  sec = Quiet @ Check[N[seconds], 20];
  If[! NumericQ[sec] || sec <= 0, sec = 20];

  res = TimeConstrained[
  Quiet @ Check[FeynAmpDenominatorExplicit[expr], expr],
  sec,
  expr
  ];

  res
];

(* Intento de cinem\[AAcute]tica 2 -> 2. No es imprescindible para identificar WCs,
  pero puede ayudar a FeynCalc. Si no encaja con la versi\[OAcute]n/modelo, falla en silencio. *)
set2to2KinematicsIfPossibleFA[inFields_List, outFields_List, momIn_List, momOut_List] := Quiet @ Check[
  If[Length[momIn] == 2 && Length[momOut] == 2,
  FCClearScalarProducts[];
  SetMandelstam[
  s, t, u,
  momIn[[1]], momIn[[2]], -momOut[[1]], -momOut[[2]],
  fieldMassFA[inFields[[1]]],
  fieldMassFA[inFields[[2]]],
  fieldMassFA[outFields[[1]]],
  fieldMassFA[outFields[[2]]]
  ];
  ],
  Null
];


(* ================================================================ *)
(* |M|^2 FISICA OPTIMIZADA  *)
(* ================================================================ *)

ClearAll[
  exprTermsFA,
  safeNonZeroExprQFA,
  reduceSpinColorChunkFA,
  physicalM2DirectFA,
  physicalM2ChunkedLinearFA,
  computeAmplitudeSquaredFA
];

exprTermsFA[expr_] := Module[{e},
  e = Quiet @ Check[Expand[expr], expr];
  If[Head[e] === Plus, List @@ e, {e}]
];

safeNonZeroExprQFA[expr_] := Quiet @ Check[
  !(expr === 0 || TrueQ[PossibleZeroQ[expr]]),
  Module[{w = activeWCsFA[]}, w =!= {} && ! FreeQ[expr, Alternatives @@ w]]
];

(*
  Reduccion fisica de un bloque peque\[NTilde]o de la amplitud cuadrada.
  Punto clave: NO usamos FeynAmpDenominatorExplicit antes de FermionSpinSum.
  En los ejemplos oficiales de FeynCalc se hace primero:
  amp ComplexConjugate[amp] // FermionSpinSum // DiracSimplify // Factor
  Explicitar denominadores antes de sumar espines suele agrandar mucho SMEFTsim.
*)
reduceSpinColorChunkFA[chunk_, vecData_List, doColor_: False, seconds_: 20] := Module[
  {sec, e, j},

  sec = Quiet @ Check[N[seconds], 20];
  If[! NumericQ[sec] || sec <= 0, sec = 20];

  TimeConstrained[
  Quiet @ Check[
  e = preprocessAmplitudeForM2FA[chunk];
  e = FCI[e];
  e = FermionSpinSum[e];

  Do[
  e = Quiet @ Check[
  If[vecData[[j, 2]],
  DoPolarizationSums[e, vecData[[j, 1]], 0],
  DoPolarizationSums[e, vecData[[j, 1]]]
  ],
  e
  ],
  {j, 1, Length[vecData]}
  ];

  e = DiracSimplify[e];
  e = Contract[e];

  If[TrueQ[doColor],
  e = Quiet @ Check[SUNSimplify[e, Explicit -> True, SUNNToCACF -> True], e]
  ];

  preprocessAmplitudeForM2FA[e],
  $Failed
  ],
  sec,
  $TimedOut
  ]
];

physicalM2DirectFA[raw_, vecData_List, doColor_: False, seconds_: 60] := Module[
  {res, sec},
  sec = Quiet @ Check[N[seconds], 60];
  If[! NumericQ[sec] || sec <= 0, sec = 60];

  (* Un intento directo, pero sin FADExplicit previo. Si no termina, se intenta por bloques. *)
  res = reduceSpinColorChunkFA[raw, vecData, doColor, sec];
  res
];

(*
  Interferencia lineal fisica calculada por bloques.
  No es un filtro manual de WCs: para cada WC que aparece en M_lineal se construye
  C_i M_i M_SM^* + h.c., se reduce fisicamente con FermionSpinSum y solo se conserva
  si la expresion reducida no se anula.
*)
physicalM2ChunkedLinearFA[
  ampSM_, ampLin_, vecData_List, doColor_: False, totalSeconds_: 120, chunkSize_: 8
  ] := Module[
  {sec, started, smTerms, candidateWCs, result = 0, undecided = {}, failed = {},
  wc, ampWC, wcPiece, wtTerms, smConjTerms, buffer, partial, chunk, perChunk,
  contribution, i, j, elapsed, remaining, maxChunkTime, nPairs, nDone},

  sec = Quiet @ Check[N[totalSeconds], 120];
  If[! NumericQ[sec] || sec <= 0, sec = 120];
  started = AbsoluteTime[];

  smTerms = exprTermsFA[ampSM];
  smConjTerms = ComplexConjugate /@ smTerms;

  candidateWCs = Select[activeWCsFA[], ! FreeQ[ampLin, #] || ! FreeQ[ampLin, HC[#]] || ! FreeQ[ampLin, ComplexConjugate[#]] || ! FreeQ[ampLin, Conjugate[#]] &];
  candidateWCs = SortBy[DeleteDuplicates[candidateWCs], SymbolName];

  $lastM2PhysicalCandidates = SymbolName /@ candidateWCs;
  $lastM2PhysicalUndecided = {};
  $lastM2PhysicalFailed = {};

  Do[
  elapsed = AbsoluteTime[] - started;
  remaining = sec - elapsed;
  If[remaining <= 0,
  AppendTo[undecided, SymbolName[wc]];
  Continue[];
  ];

  ampWC = TimeConstrained[
  Quiet @ Check[Coefficient[ampLin, wc], $Failed],
  Min[Max[remaining/4, 5], 30],
  $TimedOut
  ];

  If[ampWC === $TimedOut,
  AppendTo[undecided, SymbolName[wc]];
  Continue[];
  ];

  If[ampWC === $Failed || TrueQ[PossibleZeroQ[ampWC]],
  AppendTo[failed, SymbolName[wc]];
  Continue[];
  ];

  wtTerms = exprTermsFA[ampWC];
  buffer = {};
  contribution = 0;
  nPairs = Length[wtTerms] Length[smTerms];
  nDone = 0;

  Do[
  AppendTo[buffer,
  (wc wtTerms[[i]]) smConjTerms[[j]] + smTerms[[j]] ComplexConjugate[wc wtTerms[[i]]]
  ];
  nDone++;

  If[Length[buffer] >= chunkSize || nDone == nPairs,
  chunk = Plus @@ buffer;
  buffer = {};

  elapsed = AbsoluteTime[] - started;
  remaining = sec - elapsed;
  If[remaining <= 0,
  AppendTo[undecided, SymbolName[wc]];
  Break[];
  ];

  maxChunkTime = Min[Max[remaining/10, 3], 20];
  partial = reduceSpinColorChunkFA[chunk, vecData, doColor, maxChunkTime];

  If[partial === $TimedOut,
  AppendTo[undecided, SymbolName[wc]];
  Break[];
  ];

  If[partial === $Failed || partial === $Aborted,
  AppendTo[failed, SymbolName[wc]];
  Break[];
  ];

  contribution = contribution + partial;
  ],
  {i, 1, Length[wtTerms]}, {j, 1, Length[smTerms]}
  ];

  If[! MemberQ[undecided, SymbolName[wc]] && ! MemberQ[failed, SymbolName[wc]],
  contribution = preprocessAmplitudeForM2FA[contribution];
  contribution = Quiet @ Check[DiracSimplify[Contract[contribution]], contribution];
  contribution = preprocessAmplitudeForM2FA[contribution];
  If[safeNonZeroExprQFA[contribution],
  result = result + contribution
  ];
  ];
  ,
  {wc, candidateWCs}
  ];

  $lastM2PhysicalUndecided = DeleteDuplicates[undecided];
  $lastM2PhysicalFailed = DeleteDuplicates[failed];

  If[result === 0 && Length[$lastM2PhysicalUndecided] > 0,
  $TimedOut,
  result
  ]
];

computeAmplitudeSquaredFA[
  ampM_, inFields_List, outFields_List,
  momIn_List, momOut_List,
  m2Mode_, doColor_, simpMode_, simpTimeout_, sqTimeout_
  ] := Module[
  {allFields, allMom, vecData, ts, ampPrep, ampSM, ampLin, raw,
  linearlyBuilt, mSq, direct, chunked},

  ts = Quiet @ Check[N[sqTimeout], 120];
  If[! NumericQ[ts] || ts <= 0, ts = 120];

  $m2PhysicalIncompleteFA = False;
  $m2ActualModeFA = m2Mode;
  $m2LinearBuiltFA = False;
  $lastM2PhysicalCandidates = {};
  $lastM2PhysicalUndecided = {};
  $lastM2PhysicalFailed = {};

  allFields = Join[inFields, outFields];
  allMom  = Join[momIn, momOut];
  vecData  = externalVectorDataFA[allFields, allMom];

  ampPrep = preprocessAmplitudeForM2FA[ampM];

  linearlyBuilt = False;
  If[TrueQ[useLinearWCRules],
  ampSM  = zeroAllWCsFA[ampPrep];
  ampLin = linearAmplitudePartFA[ampPrep, Min[30, ts]];
  raw = ampLin ComplexConjugate[ampSM] + ampSM ComplexConjugate[ampLin];
  linearlyBuilt = True;
  $m2LinearBuiltFA = True,
  raw = ampPrep ComplexConjugate[ampPrep]
  ];

  raw = preprocessAmplitudeForM2FA[raw];

  If[m2Mode === "Estructural",
  mSq = TimeConstrained[
  Quiet @ Check[tryFeynAmpDenominatorExplicitFA[raw, Min[20, ts]], raw],
  ts,
  raw
  ];
  $m2ActualModeFA = If[linearlyBuilt, "EstructuralLineal", "Estructural"];
  Return[mSq]
  ];

  (* Modo fisico estricto: primero intento directo estilo FeynCalc; si no termina,
  intento por bloques. No se devuelven WCs estructurales como si fueran fisicos. *)
  set2to2KinematicsIfPossibleFA[inFields, outFields, momIn, momOut];

  If[TrueQ[linearlyBuilt],
  direct = physicalM2DirectFA[raw, vecData, doColor, Min[ts/3, 90]];

  If[direct =!= $TimedOut && direct =!= $Failed && direct =!= $Aborted,
  mSq = direct;
  $m2ActualModeFA = "FisicoLinealDirecto",

  chunked = physicalM2ChunkedLinearFA[ampSM, ampLin, vecData, doColor, ts, 6];
  If[chunked === $TimedOut || chunked === $Failed || chunked === $Aborted,
  $m2ActualModeFA = "FisicoLinealNoConclusivo";
  mSq = 0,
  $m2ActualModeFA = "FisicoLinealPorBloques";
  mSq = chunked
  ]
  ],

  (* Si no se pide linealidad, el cuadrado fisico completo puede ser muy grande.
  Se intenta directo y si no termina queda no conclusivo, sin fallback estructural. *)
  direct = physicalM2DirectFA[raw, vecData, doColor, ts];
  If[direct === $TimedOut || direct === $Failed || direct === $Aborted,
  $m2ActualModeFA = "FisicoCompletoNoConclusivo";
  mSq = 0,
  $m2ActualModeFA = "FisicoCompleto";
  mSq = direct
  ]
  ];

  mSq = preprocessAmplitudeForM2FA[mSq];
  mSq = safeSimplifyFA[mSq, simpMode, simpTimeout];
  mSq
];

(* ===================== CONTROL DE TOPOLOG\[CapitalIAcute]AS ===================== *)

ClearAll[topologiesToExcludeFA, topologyModeLabelFA];

(*
  IMPORTANTE:
  No usar ExcludeTopologies -> Reducible si se quieren diagramas de intercambio
  s/t/u con bosones internos. En 2 -> 2 esos diagramas son reducibles al cortar
  una l\[IAcute]nea interna, as\[IAcute] que FeynArts los elimina y deja casi solo v\[EAcute]rtices de
  contacto.
*)

topologiesToExcludeFA[mode_] := Switch[mode,
  
  "ContactOnly",
  Reducible,
  
  "OneLoop1PI",
  {Tadpoles, WFCorrections, SelfEnergies},
  
  "TreeFull",
  {Tadpoles, WFCorrections},
  
  _,
  {Tadpoles, WFCorrections}
];

topologyModeLabelFA[mode_] := Switch[mode,
  "TreeFull", "\[CapitalAAcute]rbol completo: contacto + intercambio",
  "ContactOnly", "Solo contacto: excluye reducibles",
  "OneLoop1PI", "1-loop 1PI: sin tadpoles/WF/self-energies",
  _, ToString[mode]
];




(* ===================== GU\[CapitalIAcute]AS INTERACTIVAS ===================== *)

ClearAll[
  guiaCodeFA,
  guiaBulletFA,
  guiaWarnFA,
  guiaNoteFA,
  guiaOpenFA,
  mostrarGuiaUso,
  mostrarGuiaModificaciones,
  mostrarGuiaErrores
];

guiaCodeFA[x_] := Style[ToString[x], FontFamily -> "Courier", Bold];

guiaBulletFA[x_] := Row[{"\[Bullet] ", x}];

guiaWarnFA[x_] := Style[x, Darker[Orange], Bold];

guiaNoteFA[x_] := Style[x, GrayLevel[0.35]];

guiaOpenFA[title_, body_List, open_: False] := OpenerView[
  {
  Style[title, 13, Bold],
  Column[body, Spacings -> 0.75]
  },
  open
];

mostrarGuiaUso[] := CreateDialog[
  Pane[
  DynamicModule[{},
  Column[
  {
  Style["Gu\[IAcute]a de uso", 18, Bold],
  guiaNoteFA[
  "Esta gu\[IAcute]a resume el flujo recomendado de la interfaz de arriba hacia abajo."
  ],

  guiaOpenFA[
  "1. Preparar la carpeta de trabajo",
  {
  "La interfaz est\[AAcute] pensada para funcionar de forma portable.",
  guiaBulletFA[
  Row[{"El archivo ", guiaCodeFA[".wl"], " de la interfaz debe estar en una carpeta propia."}]
  ],
  guiaBulletFA[
  Row[{"En esa misma carpeta debe existir una subcarpeta llamada ", guiaCodeFA["Models"], "."}]
  ],
  guiaBulletFA[
  Row[{"Dentro de ", guiaCodeFA["Models"], " deben estar los archivos del modelo: ", guiaCodeFA[".mod"], ", ", guiaCodeFA[".gen"], " y, para extraer WCs autom\[AAcute]ticamente, ", guiaCodeFA[".pars"], "."}]
  ],
  guiaBulletFA[
  Row[{"Ejemplo: si escribes ", guiaCodeFA["SMEFTsimtopU3l"], ", la interfaz buscar\[AAcute] ", guiaCodeFA["Models/SMEFTsimtopU3l.mod"], ", ", guiaCodeFA["Models/SMEFTsimtopU3l.gen"], " y ", guiaCodeFA["Models/SMEFTsimtopU3l.pars"], "."}]
  ],
  guiaWarnFA["No escribas rutas completas en la interfaz. Escribe solo el nombre del modelo."]
  }
  ],

  guiaOpenFA[
  "2. Introducir Model y GenericModel",
  {
  guiaBulletFA[
  Row[{guiaCodeFA["Model"], ": nombre del modelo de FeynArts, sin extensi\[OAcute]n ", guiaCodeFA[".mod"], "."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["GenericModel"], ": nombre del modelo gen\[EAcute]rico, sin extensi\[OAcute]n ", guiaCodeFA[".gen"], "."}]
  ],
  guiaBulletFA[
  Row[{"En algunos modelos ambos nombres coinciden; en otros, el ", guiaCodeFA["GenericModel"], " puede llamarse ", guiaCodeFA["Lorentz"], " u otro nombre."}]
  ],
  guiaBulletFA[
  "Al calcular, la interfaz valida que ambos archivos existan dentro de la carpeta local Models."
  ]
  }
  ],

  guiaOpenFA[
  "3. Elegir InsertionLevel, LoopOrder y topolog\[IAcute]as",
  {
  guiaBulletFA[
  Row[{guiaCodeFA["InsertionLevel -> {Particles}"], ": calcula con part\[IAcute]culas concretas. Es el modo m\[AAcute]s expl\[IAcute]cito y suele ser el recomendado para identificar WCs."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["InsertionLevel -> {Classes}"], ": agrupa por clases de campos. Puede ser m\[AAcute]s compacto, pero menos detallado."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["LoopOrder = 0"], ": \[AAcute]rbol. Es el modo recomendado para empezar."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["LoopOrder = 1"], ": un loop. Puede ser muy costoso, especialmente con protones o procesos 2 -> 3."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["\[CapitalAAcute]rbol completo: contacto + intercambio"], ": opci\[OAcute]n normal. Permite v\[EAcute]rtices de contacto y diagramas con propagadores internos."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["Solo v\[EAcute]rtices de contacto"], ": usa exclusi\[OAcute]n de reducibles; sirve para aislar contactos, pero elimina diagramas de intercambio."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["1-loop 1PI controlado"], ": pensado para c\[AAcute]lculos de loop evitando ciertas topolog\[IAcute]as problem\[AAcute]ticas."}]
  ]
  }
  ],

  guiaOpenFA[
  "4. Elegir modo de simplificaci\[OAcute]n",
  {
  guiaBulletFA[
  Row[{guiaCodeFA["Ninguna"], ": no aplica simplificaci\[OAcute]n algebraica pesada. Recomendado para procesos muy grandes, como ", guiaCodeFA["p p -> t tbar H"], "."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["Ligera"], ": aplica una simplificaci\[OAcute]n limitada por tiempo. Es el compromiso recomendado."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["Completa"], ": intenta simplificar m\[AAcute]s. \[CapitalUAcute]til para procesos peque\[NTilde]os 2 -> 2, pero puede bloquear procesos grandes."}]
  ],
  guiaBulletFA[
  Row[{"El ", guiaCodeFA["Timeout"], " limita el tiempo de simplificaci\[OAcute]n. Si se supera, la interfaz contin\[UAcute]a con la expresi\[OAcute]n sin simplificar."}]
  ]
  }
  ],

  guiaOpenFA[
  "5. Seleccionar part\[IAcute]culas entrantes y salientes",
  {
  guiaBulletFA[
  "Primero elige el n\[UAcute]mero de part\[IAcute]culas entrantes y salientes."
  ],
  guiaBulletFA[
  "Despu\[EAcute]s, para cada posici\[OAcute]n, elige la categor\[IAcute]a y la part\[IAcute]cula concreta."
  ],
  guiaBulletFA[
  "Las part\[IAcute]culas disponibles se definen en el cat\[AAcute]logo interno de la interfaz."
  ],
  guiaBulletFA[
  "Los hadrones solo aparecen como part\[IAcute]culas entrantes; no tiene sentido seleccionarlos como salientes en FeynArts."
  ],
  guiaBulletFA[
  Row[{"Ejemplo lept\[OAcute]nico: ", guiaCodeFA["e- e+ -> t tbar"], "."}]
  ],
  guiaBulletFA[
  Row[{"Ejemplo hadr\[OAcute]nico: ", guiaCodeFA["p p -> t tbar"], "."}]
  ],
  guiaBulletFA[
  Row[{"Ejemplo asociado: ", guiaCodeFA["p p -> t tbar H"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "6. Usar colisi\[OAcute]n p p y procesos hadr\[OAcute]nicos",
  {
  "FeynArts no entiende el prot\[OAcute]n como part\[IAcute]cula fundamental. Por eso la interfaz lo expande internamente a partones.",
  guiaBulletFA[
  Row[{guiaCodeFA["Usar colisi\[OAcute]n p p"], " configura autom\[AAcute]ticamente ", guiaCodeFA["in[1] = p"], " e ", guiaCodeFA["in[2] = p"], "."}]
  ],
  guiaBulletFA[
  Row[{"Despu\[EAcute]s, al calcular, ", guiaCodeFA["p p"], " se transforma en canales como ", guiaCodeFA["u ubar"], ", ", guiaCodeFA["d dbar"], ", ", guiaCodeFA["s sbar"], ", ", guiaCodeFA["c cbar"], " y ", guiaCodeFA["g g"], " seg\[UAcute]n el esquema seleccionado."}]
  ],
  guiaBulletFA[
  Row[{"Tambi\[EAcute]n se permiten procesos mixtos, por ejemplo ", guiaCodeFA["p t -> ..."], ". En ese caso solo se expande el prot\[OAcute]n."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["ttbarLO"], ": usa los canales principales para producci\[OAcute]n de pares top a \[AAcute]rbol: ", guiaCodeFA["q qbar"], " y ", guiaCodeFA["g g"], "."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["4FS"], ": usa quarks ligeros u, d, s, c, antiquarks y glu\[OAcute]n."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["5FS"], ": a\[NTilde]ade b y bbar."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["6FSFormal"], ": a\[NTilde]ade t y tbar como partones formales. No representa un PDF est\[AAcute]ndar del prot\[OAcute]n."}]
  ]
  }
  ],

  guiaOpenFA[
  "7. Seleccionar simetr\[IAcute]as y filtros",
  {
  guiaBulletFA[
  Row[{Style["CP conservante", Bold], ": pone a cero WCs CP-violantes detectados en la lista WC2, por ejemplo nombres terminados en Im o til."}]
  ],
  guiaBulletFA[
  Row[{Style["WCs reales", Bold], ": anula las componentes independientes terminadas en ", guiaCodeFA["Im"], ". Las conjugaciones de Re/Im se simplifican estructuralmente."}]
  ],
  guiaBulletFA[
  Row[{Style["Lineal en WCs", Bold], ": conserva solo los t\[EAcute]rminos de grado <= 1 en los coeficientes de Wilson, es decir la interferencia SM x dim-6. Se aplica la \[UAcute]ltima y selecciona t\[EAcute]rminos por su grado en WCs (r\[AAcute]pido). Sobre |M|^2 elimina los t\[EAcute]rminos cuadr\[AAcute]ticos C_i C_j y C_i^2."}]
  ],
  guiaBulletFA[
  Row[{Style["Leptones sin masa", Bold], ": pone a cero las masas y los Yukawas de los leptones (l\[IAcute]mite de alta energ\[IAcute]a). Sustituye a los antiguos filtros de escalar/tensor y de subespacio EW."}]
  ],
  guiaWarnFA[
  "CP y WCs reales son hip\[OAcute]tesis generales. Leptones sin masa es una aproximaci\[OAcute]n de alta energ\[IAcute]a. Lineal en WCs es el truncamiento est\[AAcute]ndar a interferencia."
  ]
  }
  ],

  guiaOpenFA[
  "8. Calcular los coeficientes de Wilson",
  {
  "El bot\[OAcute]n principal realiza el flujo completo.",
  guiaBulletFA["Limpia resultados anteriores."],
  guiaBulletFA["Expande el estado inicial si hay protones."],
  guiaBulletFA["Valida Model y GenericModel."],
  guiaBulletFA[Row[{"Carga el archivo ", guiaCodeFA[".pars"], " y construye la lista ", guiaCodeFA["WC2"], "."}]],
  guiaBulletFA["Inicializa el modelo en FeynArts."],
  guiaBulletFA["Crea topolog\[IAcute]as."],
  guiaBulletFA["Genera diagramas canal por canal."],
  guiaBulletFA["Calcula amplitudes con CreateFeynAmp."],
  guiaBulletFA["Convierte a FeynCalc con FCFAConvert."],
  guiaBulletFA["Aplica simetr\[IAcute]as y filtros."],
  guiaBulletFA["Extrae los WCs presentes en las amplitudes procesadas."]
  }
  ],

  guiaOpenFA[
  "9. Leer los resultados",
  {
  guiaBulletFA[
  Row[{guiaCodeFA["WCs obtenidos"], ": lista de coeficientes de Wilson que aparecen tras las reglas seleccionadas."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["Canales part\[OAcute]nicos evaluados"], ": canales que la interfaz ha intentado calcular."}]
  ],
  guiaBulletFA[
  "En procesos hadr\[OAcute]nicos, los WCs finales son la uni\[OAcute]n de los WCs encontrados en los canales part\[OAcute]nicos calculados."
  ],
  guiaBulletFA[
  "Si aparece Nothing donde esperabas una lista de WCs, normalmente significa que no se ha encontrado ning\[UAcute]n WC tras aplicar filtros, o que no se ha llegado a calcular ninguna amplitud v\[AAcute]lida."
  ]
  }
  ],

  guiaOpenFA[
  "10. Exportar diagramas a PDF",
  {
  guiaBulletFA[
  "El bot\[OAcute]n exporta los diagramas generados previamente."
  ],
  guiaBulletFA[
  Row[{"Para un \[UAcute]nico canal usa directamente ", guiaCodeFA["Paint[...]"], " y ", guiaCodeFA["Export[..., \"PDF\"]"], "."}]
  ],
  guiaBulletFA[
  "Para varios canales part\[OAcute]nicos, exporta los canales individualmente y despu\[EAcute]s intenta recombinarlos."
  ],
  guiaBulletFA[
  "Si la recombinaci\[OAcute]n falla, la interfaz conserva los PDFs separados en una carpeta temporal indicada en el Status."
  ],
  guiaWarnFA[
  "No cambies la exportaci\[OAcute]n si funciona. Las rutas con DisplayFunction -> Identity, ExpressionCell o NotebookWrite pueden hacer que el PDF muestre expresiones internas en vez de diagramas."
  ]
  }
  ],

  guiaOpenFA[
  "11. Resetear, limpiar y cancelar",
  {
  guiaBulletFA[
  Row[{"Si no hay c\[AAcute]lculo en marcha, ", guiaCodeFA["Resetear/Limpiar todo"], " borra diagramas, canales, amplitudes, WCs, selecci\[OAcute]n de part\[IAcute]culas y progreso."}]
  ],
  guiaBulletFA[
  Row[{"Si hay un c\[AAcute]lculo en marcha, el bot\[OAcute]n se comporta como ", guiaCodeFA["Cancelar y limpiar"], "."}]
  ],
  guiaBulletFA[
  "La cancelaci\[OAcute]n puede no ser instant\[AAcute]nea si Mathematica est\[AAcute] dentro de una llamada pesada de FeynArts/FeynCalc."
  ],
  guiaBulletFA[
  "Al terminar la cancelaci\[OAcute]n, conviene revisar que el Status indique que la interfaz se ha limpiado."
  ]
  }
  ],

  "",
  DefaultButton["Cerrar"]
  },
  Spacings -> 1.05
  ]
  ],
  {820, 620},
  Scrollbars -> True
  ],
  WindowTitle -> "Gu\[IAcute]a de uso",
  Modal -> True
];

mostrarGuiaModificaciones[] := CreateDialog[
  Pane[
  DynamicModule[{},
  Column[
  {
  Style["Gu\[IAcute]a de modificaciones", 18, Bold],
  guiaNoteFA[
  "Esta gu\[IAcute]a resume d\[OAcute]nde tocar el c\[OAcute]digo si se quiere adaptar la interfaz a otro modelo o a otra convenci\[OAcute]n de WCs."
  ],

  guiaOpenFA[
  "1. Mantener una copia estable",
  {
  guiaBulletFA[
  "Antes de modificar nada, guarda una versi\[OAcute]n estable del .wl."
  ],
  guiaBulletFA[
  "Cambia una sola cosa cada vez y prueba un proceso sencillo antes de continuar."
  ],
  guiaBulletFA[
  Row[{"Proceso de prueba recomendado: ", guiaCodeFA["e- e+ -> t tbar"], " a \[AAcute]rbol."}]
  ],
  guiaBulletFA[
  "Reinicia el kernel si cambias funciones relacionadas con FeynArts, FeynCalc o la carga del modelo."
  ]
  }
  ],

  guiaOpenFA[
  "2. Usar otro modelo de FeynArts",
  {
  guiaBulletFA[
  Row[{"Copia los archivos ", guiaCodeFA[".mod"], ", ", guiaCodeFA[".gen"], " y, si existe, ", guiaCodeFA[".pars"], " en la carpeta local ", guiaCodeFA["Models"], "."}]
  ],
  guiaBulletFA[
  Row[{"En la interfaz cambia los campos ", guiaCodeFA["Model"], " y ", guiaCodeFA["GenericModel"], "."}]
  ],
  guiaBulletFA[
  Row[{"Si quieres cambiar los valores por defecto, modifica dentro de ", guiaCodeFA["DynamicModule"], " las variables ", guiaCodeFA["modelName"], " y ", guiaCodeFA["genName"], "."}]
  ],
  guiaBulletFA[
  Row[{"La validaci\[OAcute]n se hace con ", guiaCodeFA["validarModelosFA[modelName, genName]"], "."}]
  ],
  guiaBulletFA[
  Row[{"La carpeta usada se obtiene con ", guiaCodeFA["faModelsDir[]"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "3. Modificar la obtenci\[OAcute]n de WCs desde el .pars",
  {
  "La extracci\[OAcute]n autom\[AAcute]tica de WCs se hace en la funci\[OAcute]n:",
  guiaCodeFA["cargarParametrosModeloFA[model_]"],
  guiaBulletFA[
  Row[{"Carga el archivo ", guiaCodeFA[".pars"], " con ", guiaCodeFA["Get[parsFile]"], "."}]
  ],
  guiaBulletFA[
  Row[{"Lee par\[AAcute]metros externos desde ", guiaCodeFA["M$ExtParams"], "."}]
  ],
  guiaBulletFA[
  Row[{"Lee par\[AAcute]metros internos desde ", guiaCodeFA["M$IntParams"], "."}]
  ],
  guiaBulletFA[
  Row[{"Construye ", guiaCodeFA["WCext"], ", ", guiaCodeFA["WCint"], ", ", guiaCodeFA["WC1"], " y finalmente ", guiaCodeFA["WC2"], "."}]
  ],
  guiaBulletFA[
  Row[{"El criterio actual elimina par\[AAcute]metros SM mediante ", guiaCodeFA["DeleteCases"], "."}]
  ],
  guiaWarnFA[
  "Si usas otro modelo, probablemente tendr\[AAcute]s que modificar las listas de DeleteCases para no eliminar WCs reales ni conservar par\[AAcute]metros SM como si fueran WCs."
  ],
  guiaBulletFA[
  "Si el otro modelo no usa M$ExtParams/M$IntParams, tendr\[AAcute]s que adaptar esta funci\[OAcute]n al formato real de su archivo de par\[AAcute]metros."
  ],
  guiaBulletFA[
  Row[{"La funci\[OAcute]n de pertenencia a WCs es ", guiaCodeFA["wcQ[x_] := MemberQ[WC2, x]"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "4. Modificar reglas de CP si los nombres cambian",
  {
  "La regla CP est\[AAcute] en:",
  guiaCodeFA["cpRulesLocal dentro de buildSymmetryRules[]"],
  guiaBulletFA[
  Row[{"Ahora usa la regla r\[AAcute]pida original: todo s\[IAcute]mbolo cuyo nombre termine en ", guiaCodeFA["Im"], " o ", guiaCodeFA["til"], " se pone a cero."}]
  ],
  guiaBulletFA[
  "Si otro modelo usa nombres diferentes para CP-violantes, cambia esta funci\[OAcute]n."
  ],
  guiaBulletFA[
  Row[{"Ejemplo: si el modelo usa prefijos tipo ", guiaCodeFA["CPV"], ", habr\[IAcute]a que a\[NTilde]adir ", guiaCodeFA["StringStartsQ[n, \"CPV\"]"], "."}]
  ],
  guiaWarnFA[
  "Esta versi\[OAcute]n copia la regla r\[AAcute]pida de WilsonUI.wl. Si necesitas m\[AAcute]xima seguridad para otros modelos, limita la regla CP a los s\[IAcute]mbolos contenidos en WC2, aunque puede ser menos directa."
  ]
  }
  ],

  guiaOpenFA[
  "5. Modificar WCs reales y linealizaci\[OAcute]n",
  {
  guiaBulletFA[
  Row[{guiaCodeFA["realRulesLocal dentro de buildSymmetryRules[]"], " anula los WCs independientes terminados en ", guiaCodeFA["Im"], ". Las conjugaciones de las componentes Re/Im se tratan en ", guiaCodeFA["normalizarWCsModeloFA"], "."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["linearWCRulesLocal dentro de buildSymmetryRules[]"], " elimina productos simples ", guiaCodeFA["WC_i WC_j"], " mediante una regla r\[AAcute]pida."}]
  ],
  guiaBulletFA[
  "Estas reglas son r\[AAcute]pidas, pero dependen de que los nombres de los WCs del nuevo modelo sigan una nomenclatura compatible."
  ],
  guiaWarnFA[
  "Si quieres una linealizaci\[OAcute]n m\[AAcute]s rigurosa, puedes volver a una versi\[OAcute]n con Series/eps, pero ser\[AAcute] m\[AAcute]s lenta en procesos grandes."
  ]
  }
  ],

  guiaOpenFA[
  "6. Modificar filtros fenomenol\[OAcute]gicos",
  {
  "Los filtros no universales est\[AAcute]n en:",
  guiaCodeFA["scalarTensorRulesLocal dentro de buildSymmetryRules[]"],
  guiaCodeFA["ewLeptonRulesLocal dentro de buildSymmetryRules[]"],
  guiaBulletFA[
  Row[{guiaCodeFA["scalarTensorRulesLocal dentro de buildSymmetryRules[]"], " usa prefijos como ", guiaCodeFA["cleQt"], ", ", guiaCodeFA["clebQ"], ", ", guiaCodeFA["cledj"], ", ", guiaCodeFA["cleju"], "."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["ewLeptonRulesLocal dentro de buildSymmetryRules[]"], " define listas como ", guiaCodeFA["keep2q2l"], " y ", guiaCodeFA["ewInputToZero"], "."}]
  ],
  guiaBulletFA[
  "Para otro modelo, cambia esos prefijos/listas para que coincidan con sus nombres de WCs."
  ],
  guiaWarnFA[
  "Estos filtros no son simetr\[IAcute]as universales. \[CapitalUAcute]salos solo para reproducir un an\[AAcute]lisis o analisis externo concreto."
  ]
  }
  ],

  guiaOpenFA[
  "7. Modificar c\[OAcute]mo se detectan los WCs obtenidos",
  {
  "La detecci\[OAcute]n final se hace con:",
  guiaCodeFA["wcAppearsInExprFA[expr_, wc_]"],
  guiaBulletFA[
  Row[{"Comprueba ", guiaCodeFA["Coefficient[expr, wc]"], ", ", guiaCodeFA["Coefficient[expr, ComplexConjugate[wc]]"], " y ", guiaCodeFA["Coefficient[expr, Conjugate[wc]]"], "."}]
  ],
  guiaBulletFA[
  "Si otro modelo representa WCs con otra estructura, esta funci\[OAcute]n puede necesitar cambios."
  ],
  guiaBulletFA[
  "Si quieres reproducir exactamente un analisis externo, puedes a\[NTilde]adir un filtro posterior que seleccione solo los WCs de ese analisis externo."
  ],
  guiaBulletFA[
  Row[{"Ejemplo conceptual: ", guiaCodeFA["wcResult = Select[wcResult, analisis externoPresetQ]"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "8. A\[NTilde]adir un preset o subespacio externo",
  {
  "Si un analisis externo usa una base reducida o combinaciones de WCs, no lo metas como simetr\[IAcute]a general.",
  guiaBulletFA[
  "Crea una casilla o selector separado llamado, por ejemplo, Filtro de analisis externo."
  ],
  guiaBulletFA[
  "Define una lista de nombres permitidos o una funci\[OAcute]n de selecci\[OAcute]n."
  ],
  guiaBulletFA[
  Row[{"Aplica el filtro despu\[EAcute]s de obtener ", guiaCodeFA["wcResult"], "."}]
  ],
  guiaBulletFA[
  "Si el analisis externo usa combinaciones como CtZ = cW CtW - sW CtB, a\[NTilde]ade una capa de traducci\[OAcute]n de nombres para mostrarlas correctamente."
  ]
  }
  ],

  guiaOpenFA[
  "9. Modificar el cat\[AAcute]logo de part\[IAcute]culas",
  {
  "El cat\[AAcute]logo se define en:",
  guiaCodeFA["particleCatalog"],
  guiaBulletFA[
  "Para a\[NTilde]adir o quitar part\[IAcute]culas, modifica sus categor\[IAcute]as."
  ],
  guiaBulletFA[
  Row[{"Los men\[UAcute]s usan ", guiaCodeFA["inputCategoryRules[]"], " y ", guiaCodeFA["outputCategoryRules[]"], "."}]
  ],
  guiaBulletFA[
  Row[{guiaCodeFA["outputCategoryRules[]"], " elimina Hadrones de los salientes."}]
  ],
  guiaBulletFA[
  "Si a\[NTilde]ades categor\[IAcute]as especiales, revisa que no aparezcan donde no deben."
  ]
  }
  ],

  guiaOpenFA[
  "10. Modificar procesos hadr\[OAcute]nicos",
  {
  "La expansi\[OAcute]n de protones est\[AAcute] en funciones como:",
  guiaCodeFA["protonPartons[scheme]"],
  guiaCodeFA["ppInitialStates[scheme]"],
  guiaCodeFA["expandInitialStatesFA[inList, ppScheme]"],
  guiaBulletFA[
  "Para cambiar qu\[EAcute] partones contiene el prot\[OAcute]n, modifica protonPartons."
  ],
  guiaBulletFA[
  "Para evitar duplicados como u ubar y ubar u en p p, se usa una canonicalizaci\[OAcute]n de canales."
  ],
  guiaBulletFA[
  "Para procesos mixtos p + part\[IAcute]cula, solo se expande la posici\[OAcute]n ocupada por el prot\[OAcute]n."
  ]
  }
  ],

  guiaOpenFA[
  "11. Modificar FCFAConvert y compatibilidad",
  {
  guiaBulletFA[
  Row[{"Las opciones extra de FCFAConvert se controlan con ", guiaCodeFA["fcfaConvertExtraOptionsFA[]"], "."}]
  ],
  guiaBulletFA[
  Row[{"Esto permite a\[NTilde]adir ", guiaCodeFA["DropIndexSum -> False"], " solo si la versi\[OAcute]n de FeynCalc lo reconoce."}]
  ],
  guiaBulletFA[
  Row[{"Para depurar en otro equipo, comprueba ", guiaCodeFA["$FeynCalcVersion"], ", ", guiaCodeFA["$FeynCalcDirectory"], " y ", guiaCodeFA["Options[FCFAConvert]"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "12. Modificar la exportaci\[OAcute]n PDF",
  {
  guiaWarnFA[
  "La exportaci\[OAcute]n es una de las partes m\[AAcute]s sensibles. Mant\[EAcute]n la soluci\[OAcute]n simple si funciona."
  ],
  guiaBulletFA[
  Row[{"La v\[IAcute]a estable es ", guiaCodeFA["Paint[...]"], " seguido de ", guiaCodeFA["Export[..., \"PDF\"]"], "."}]
  ],
  guiaBulletFA[
  "Para varios canales se exportan PDFs temporales por canal y luego se intenta componer un \[UAcute]nico PDF."
  ],
  guiaBulletFA[
  Row[{"Evita ", guiaCodeFA["DisplayFunction -> Identity"], ", ", guiaCodeFA["ExpressionCell"], ", ", guiaCodeFA["NotebookWrite"], " y conversiones manuales salvo que sea imprescindible."}]
  ]
  }
  ],

  "",
  DefaultButton["Cerrar"]
  },
  Spacings -> 1.05
  ]
  ],
  {820, 620},
  Scrollbars -> True
  ],
  WindowTitle -> "Gu\[IAcute]a de modificaciones",
  Modal -> True
];

mostrarGuiaErrores[] := CreateDialog[
  Pane[
  DynamicModule[{},
  Column[
  {
  Style["Gu\[IAcute]a de errores de la interfaz", 18, Bold],
  guiaNoteFA[
  "Esta gu\[IAcute]a se refiere a los mensajes de Status de la interfaz, no a mensajes internos de Mathematica/FeynArts/FeynCalc."
  ],

  guiaOpenFA[
  "1. No existe la carpeta Models esperada",
  {
  "Significa que la interfaz no encuentra la carpeta local Models junto al archivo .wl.",
  guiaBulletFA[
  Row[{"Crea una carpeta llamada ", guiaCodeFA["Models"], " en la misma carpeta que la interfaz."}]
  ],
  guiaBulletFA[
  Row[{"Copia dentro los archivos ", guiaCodeFA[".mod"], ", ", guiaCodeFA[".gen"], " y ", guiaCodeFA[".pars"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "2. No se ha encontrado el Model o el GenericModel",
  {
  "La validaci\[OAcute]n no ha encontrado el archivo correspondiente.",
  guiaBulletFA[
  Row[{"Si Model = ", guiaCodeFA["MiModelo"], ", debe existir ", guiaCodeFA["Models/MiModelo.mod"], "."}]
  ],
  guiaBulletFA[
  Row[{"Si GenericModel = ", guiaCodeFA["MiGenerico"], ", debe existir ", guiaCodeFA["Models/MiGenerico.gen"], "."}]
  ],
  guiaBulletFA[
  "Comprueba may\[UAcute]sculas, min\[UAcute]sculas y espacios al final del nombre."
  ]
  }
  ],

  guiaOpenFA[
  "3. No se ha encontrado el fichero .pars",
  {
  "La interfaz puede generar diagramas, pero no podr\[AAcute] construir autom\[AAcute]ticamente WC2 ni extraer WCs de forma fiable.",
  guiaBulletFA[
  Row[{"A\[NTilde]ade el archivo ", guiaCodeFA[".pars"], " del modelo a la carpeta Models."}]
  ],
  guiaBulletFA[
  "Si el modelo no tiene .pars, modifica cargarParametrosModeloFA para definir WC2 manualmente."
  ]
  }
  ],

  guiaOpenFA[
  "4. Selecciona todas las part\[IAcute]culas en in y out",
  {
  "Hay alguna entrada o salida sin part\[IAcute]cula seleccionada.",
  guiaBulletFA[
  "Revisa todas las filas activas seg\[UAcute]n nIn y nOut."
  ],
  guiaBulletFA[
  "No basta con elegir la categor\[IAcute]a; tambi\[EAcute]n hay que elegir la part\[IAcute]cula concreta."
  ]
  }
  ],

  guiaOpenFA[
  "5. Canales sin diagramas/timeout",
  {
  "Algunos canales part\[OAcute]nicos no han producido diagramas o han superado el tiempo m\[AAcute]ximo.",
  guiaBulletFA[
  "Puede ser normal si el proceso no existe f\[IAcute]sicamente para ese canal."
  ],
  guiaBulletFA[
  "Prueba el canal concreto manualmente para verificarlo."
  ],
  guiaBulletFA[
  "Reduce el esquema hadr\[OAcute]nico o usa ttbarLO si procede."
  ]
  }
  ],

  guiaOpenFA[
  "6. Fallo/timeout en CreateFeynAmp",
  {
  "Los diagramas existen, pero FeynArts no ha podido construir la amplitud en el tiempo asignado o ha devuelto error.",
  guiaBulletFA[
  "Prueba con menos canales o un proceso m\[AAcute]s simple."
  ],
  guiaBulletFA[
  Row[{"Empieza con ", guiaCodeFA["LoopOrder = 0"], " y topolog\[IAcute]a de \[AAcute]rbol completo."}]
  ],
  guiaBulletFA[
  "Si es un proceso 2 -> 3, puede ser simplemente demasiado pesado."
  ]
  }
  ],

  guiaOpenFA[
  "7. Fallo/timeout en FCFAConvert",
  {
  "La amplitud de FeynArts se ha generado, pero la conversi\[OAcute]n a FeynCalc ha fallado.",
  guiaBulletFA[
  Row[{"Comprueba versiones con ", guiaCodeFA["$FeynCalcVersion"], " y ", guiaCodeFA["Options[FCFAConvert]"], "."}]
  ],
  guiaBulletFA[
  Row[{"Si el problema es ", guiaCodeFA["DropIndexSum"], ", la funci\[OAcute]n ", guiaCodeFA["fcfaConvertExtraOptionsFA[]"], " intenta evitarlo autom\[AAcute]ticamente."}]
  ],
  guiaBulletFA[
  "Prueba el mismo canal de forma aislada para localizar el problema."
  ]
  }
  ],

  guiaOpenFA[
  "8. Simplify ha superado el tiempo m\[AAcute]ximo",
  {
  "No implica necesariamente que el c\[AAcute]lculo haya fallado.",
  guiaBulletFA[
  "La interfaz contin\[UAcute]a con la expresi\[OAcute]n sin simplificar."
  ],
  guiaBulletFA[
  Row[{"Para procesos grandes usa ", guiaCodeFA["Modo de simplificaci\[OAcute]n = Ninguna"], " o ", guiaCodeFA["Ligera"], "."}]
  ],
  guiaBulletFA[
  Row[{"Para procesos peque\[NTilde]os puedes subir el ", guiaCodeFA["Timeout"], "."}]
  ]
  }
  ],

  guiaOpenFA[
  "9. No aparecen WCs obtenidos / aparece Nothing",
  {
  "Puede deberse a varias causas.",
  guiaBulletFA[
  "No se ha cargado WC2 porque falta el .pars."
  ],
  guiaBulletFA[
  "El proceso no tiene dependencia en WCs tras las reglas seleccionadas."
  ],
  guiaBulletFA[
  "Los filtros activados han puesto a cero los WCs que aparec\[IAcute]an."
  ],
  guiaBulletFA[
  "La amplitud no se ha calculado correctamente en ning\[UAcute]n canal."
  ]
  }
  ],

  guiaOpenFA[
  "10. No hay diagramas para exportar",
  {
  "Se ha pulsado Exportar antes de calcular o despu\[EAcute]s de resetear.",
  guiaBulletFA[
  "Primero pulsa el bot\[OAcute]n principal de c\[AAcute]lculo."
  ],
  guiaBulletFA[
  "Comprueba que el Status indique que hay canales con diagramas."
  ]
  }
  ],

  guiaOpenFA[
  "11. Error exportando, copiando o combinando PDF",
  {
  "La exportaci\[OAcute]n ha fallado en alguna fase.",
  guiaBulletFA[
  "Prueba con un \[UAcute]nico canal part\[OAcute]nico para confirmar que Paint + Export funciona."
  ],
  guiaBulletFA[
  "Evita rutas con caracteres raros o carpetas sin permisos de escritura."
  ],
  guiaBulletFA[
  "Si no puede recombinar, la interfaz puede dejar PDFs separados por canal en una carpeta temporal."
  ]
  }
  ],

  guiaOpenFA[
  "12. C\[AAcute]lculo cancelado o abortado",
  {
  "El usuario ha pulsado Cancelar/Limpiar o Mathematica ha abortado la evaluaci\[OAcute]n.",
  guiaBulletFA[
  "Despu\[EAcute]s de cancelar, espera a que el Status indique que la interfaz se ha limpiado."
  ],
  guiaBulletFA[
  "Si el kernel queda inestable, reinicia el kernel y vuelve a cargar la interfaz."
  ]
  }
  ],

  "",
  DefaultButton["Cerrar"]
  },
  Spacings -> 1.05
  ]
  ],
  {820, 620},
  Scrollbars -> True
  ],
  WindowTitle -> "Gu\[IAcute]a de errores",
  Modal -> True
];

(* ===================== INTERFAZ PRINCIPAL ===================== *)

WilsonUIManualCatalog[] := DynamicModule[
  {
  (* modelo *)
  modelName = "SMEFTsimtopU3l", genName = "SMEFTsimtopU3l",
  
  (* opciones f\[IAcute]sicas *)
  insertion = {Particles}, loopOrder = 0,
  ppScheme = "ttbarLO",
  topologyMode = "TreeFull", allowOneLoopPP = False,
  simplificationMode = "Light", simplificationTimeout = 30,
  m2Mode = "Estructural", m2DoColor = False, m2Timeout = 120,
  
  (* proceso *)
  nIn = 2, nOut = 2,
  catInList = ConstantArray["Leptones", 6],
  catOutList = ConstantArray["Quarks", 6],
  pickIn = ConstantArray[None, 6],
  pickOut = ConstantArray[None, 6],
  
  (* estado \[UAcute]nico *)
  running = False, cancelRequested = False,
  statusGlobal = "Listo.",
  progressValue = 0., progressText = "Esperando c\[AAcute]lculo.",
  
  (* diagramas *)
  diags = None, diagsByChannel = {}, channelLabels = {},

  (* amplitud procesada; solo se conserva lo necesario para extraer WCs *)
  ampFCprocByChannel = {},
  m2ActualModesByChannel = {},

  (* WCs *)
  wcResult = {}
  },
  
  Column[
  {
  Style["Generador de Coeficientes de Wilson", 16, Bold],
  
  (* ===================== BLOQUE 1: MODELO Y OPCIONES ===================== *)
  Column[
  {
  Row[
  {
  Button[
  "Gu\[IAcute]a de uso",
  mostrarGuiaUso[],
  Method -> "Queued"
  ],
  Spacer[10],
  Button[
  "Gu\[IAcute]a de modificaciones",
  mostrarGuiaModificaciones[],
  Method -> "Queued"
  ],
  Spacer[10],
  Button[
  "Gu\[IAcute]a de errores",
  mostrarGuiaErrores[],
  Method -> "Queued"
  ]
  }
  ],
  Spacer[6],

  Grid[
  {
  {
  "Model",
  InputField[Dynamic[modelName], String, FieldSize -> 22],
  "GenericModel",
  InputField[Dynamic[genName], String, FieldSize -> 22]
  },
  {
  "InsertionLevel",
  PopupMenu[Dynamic[insertion], {{Particles} -> "{Particles}", {Classes} -> "{Classes}"}],
  "LoopOrder",
  PopupMenu[Dynamic[loopOrder], {0 -> "0 (\[AAcute]rbol)", 1 -> "1 (1-loop)"}]
  },
  {
  "Topolog\[IAcute]as",
  PopupMenu[
  Dynamic[topologyMode],
  {
  "TreeFull" -> "\[CapitalAAcute]rbol completo: contacto + intercambio",
  "ContactOnly" -> "Solo v\[EAcute]rtices de contacto",
  "OneLoop1PI" -> "1-loop 1PI controlado"
  }
  ],
  SpanFromLeft,
  SpanFromLeft
  }
  },
  Alignment -> Left,
  Spacings -> {2, 1}
  ],
  
  Dynamic[
  If[DirectoryQ[faModelsDir[]],
  Style["Carpeta Models usada: " <> faModelsDir[], 10, Gray],
  Style["No existe la carpeta Models esperada: " <> faModelsDir[], 10, Red]
  ]
  ]
  }
  ],
  
  Delimiter,
  
  (* ===================== BLOQUE 2: SELECCI\[CapitalOAcute]N DEL PROCESO ===================== *)
  Style["Selecci\[OAcute]n del proceso", 14, Bold],
  
  Grid[
  {
  {"# Entrantes (nIn):", SetterBar[Dynamic[nIn], Range[1, 4]]},
  {"# Salientes (nOut):", SetterBar[Dynamic[nOut], Range[1, 6]]}
  },
  Alignment -> Left,
  Spacings -> {2, 1}
  ],
  
  Row[
  {
  Button[
  "Usar colisi\[OAcute]n p p",
  nIn = 2;
  catInList[[1]] = "Hadrones";
  catInList[[2]] = "Hadrones";
  pickIn[[1]] = PP;
  pickIn[[2]] = PP;
  statusGlobal = Style["Estado inicial configurado como p p. El c\[AAcute]lculo se expandir\[AAcute] a canales part\[OAcute]nicos.", Darker[Green]];
  ],
  Spacer[10],
  Style["Nota: los protones se expanden antes de llamar a FeynArts; FeynArts recibe quarks/gluones, no protones.", 10, Gray]
  }
  ],
  
  Style["Entrantes", 12, Bold],
  
  Dynamic[
  Grid[
  Table[
  With[{k = i},
  {
  "in[" <> ToString[k] <> "]",
  PopupMenu[
  Dynamic[catInList[[k]], (catInList[[k]] = #; pickIn[[k]] = None) &],
  inputCategoryRules[],
  FieldSize -> 12
  ],
  PopupMenu[Dynamic[pickIn[[k]]], menuRules[catInList[[k]]], FieldSize -> 45]
  }
  ],
  {i, 1, nIn}
  ],
  Alignment -> Left,
  Spacings -> {1.5, 1}
  ],
  TrackedSymbols :> {nIn, catInList, pickIn}
  ],
  
  
  Dynamic[
  If[usesHadronInitialQ[catInList, pickIn, nIn],
  Grid[
  {
  {
  "Partones del prot\[OAcute]n",
  PopupMenu[
  Dynamic[ppScheme],
  {
  "ttbarLO" -> "ttbar LO: gg + q qbar, q=u,d,s,c",
  "4FS" -> "4FS: u,d,s,c + antiq + g",
  "5FS" -> "5FS: u,d,s,c,b + antiq + g",
  "6FSFormal" -> "6FS formal: a\[NTilde]ade t y anti-t"
  }
  ],
  SpanFromLeft,
  SpanFromLeft
  },
  {
  "",
  Row[{Checkbox[Dynamic[allowOneLoopPP]], " Permitir 1-loop con estados hadr\[OAcute]nicos expandidos (puede ser muy lento)"}],
  SpanFromLeft,
  SpanFromLeft
  }
  },
  Alignment -> Left,
  Spacings -> {2, 1}
  ],
  Nothing
  ],
  TrackedSymbols :> {catInList, pickIn, nIn, ppScheme, allowOneLoopPP}
  ],
  Spacer[8],
  
  Style["Salientes", 12, Bold],
  
  Dynamic[
  Grid[
  Table[
  With[{k = i},
  {
  "out[" <> ToString[k] <> "]",
  PopupMenu[
  Dynamic[catOutList[[k]], (catOutList[[k]] = #; pickOut[[k]] = None) &],
  outputCategoryRules[],
  FieldSize -> 12
  ],
  PopupMenu[Dynamic[pickOut[[k]]], menuRules[catOutList[[k]]], FieldSize -> 45]
  }
  ],
  {i, 1, nOut}
  ],
  Alignment -> Left,
  Spacings -> {1.5, 1}
  ],
  TrackedSymbols :> {nOut, catOutList, pickOut}
  ],
  
  Delimiter,
  
  (* ===================== BLOQUE 3: SIMETR\[CapitalIAcute]AS ===================== *)
  Style["Simetr\[IAcute]as y simplificaciones", 14, Bold],
  
  Column[
  {
  Row[
  {
  Checkbox[Dynamic[useCpRules]], " CP conservante  ",
  Checkbox[Dynamic[useRealRules]], " WCs reales  ",
  Checkbox[Dynamic[useLinearWCRules]], " Lineal en WCs"
  }
  ],
  Row[
  {
  Checkbox[Dynamic[useMasslessLeptonRules]], " Leptones sin masa (m_l = 0, alta energ\[IAcute]a)"
  }
  ],
  Row[
  {
  Checkbox[Dynamic[useTopSectorRules]], " Top-sector: activar solo WCs del sector top antes del calculo"
  }
  ],
  Dynamic[
  If[TrueQ[useTopSectorRules],
  Style["Top-sector activo: los WCs bosonicos/leptonicos puros y escalares/tensores ajenos al sector top se ponen a cero antes de construir |M|^2.", 10, Darker[Blue]],
  Nothing
  ],
  TrackedSymbols :> {useTopSectorRules}
  ],
  Grid[
  {
  {
  "Modo de simplificaci\[OAcute]n",
  PopupMenu[
  Dynamic[simplificationMode],
  {
  "None" -> "Ninguna (m\[AAcute]s r\[AAcute]pido, menos cancelaciones)",
  "Light" -> "Ligera: Simplify con timeout",
  "Full" -> "Completa: FullSimplify con timeout"
  }
  ],
  "Timeout",
  PopupMenu[
  Dynamic[simplificationTimeout],
  {10 -> "10 s", 30 -> "30 s", 60 -> "60 s", 120 -> "120 s", 300 -> "300 s"}
  ]
  }
  },
  Alignment -> Left,
  Spacings -> {2, 1}
  ],
  Grid[
  {
  {
  "Amplitud al cuadrado |M|^2",
  PopupMenu[
  Dynamic[m2Mode],
  {
  "Off" -> "No: buscar WCs en M",
  "Estructural" -> "Estructural: interferencia lineal directa si procede",
  "Fisico" -> "F\[IAcute]sico estricto: spin/pol; bloques si procede"
  }
  ],
  "Color",
  Checkbox[Dynamic[m2DoColor]],
  "Timeout |M|^2",
  PopupMenu[
  Dynamic[m2Timeout],
  {30 -> "30 s", 60 -> "60 s", 120 -> "120 s", 300 -> "300 s", 600 -> "600 s"}
  ]
  }
  },
  Alignment -> Left,
  Spacings -> {2, 1}
  ],
  Style[
  "Las simetr\[IAcute]as/filtros y la b\[UAcute]squeda de WCs se aplican sobre |M|^2 salvo en modo Off. \"Lineal en WCs\" deja solo la interferencia (lineal en C).",
  10,
  Gray
  ],
  Style[
  "Para procesos 2 -> 3, como p p -> t tbar H, usa preferentemente Ninguna o Ligera.",
  10,
  Gray
  ]
  }
  ],
  
  Spacer[10],
  
  (* ===================== BLOQUE 4: ACCIONES ===================== *)
  Button[
  Dynamic[If[running, "Calculando\[Ellipsis]", "Calcular los Coeficientes de Wilson"]],

  If[
  running,
  Null,

  Module[
  {
  inList, outList, initialInfo, initialChannels, validChannels,
  nInEff, top, tmpDiags, tmpFA, tmpFC, tmpM2, tmpProc,
  checkModelo, modelToUse, genToUse, parsStatus, res, topologyExcl,
  setStatus, setProgress, checkCancel, i, channel, totalChannels,
  failedDiagramChannels, failedAmpChannels, nAmp, lbl, base, step,
  m2ActualModesLocal, momInEff, momOutEff, skipLinearNow
  },

  running = True;
  cancelRequested = False;
  progressValue = 0.;
  progressText = "Iniciando c\[AAcute]lculo...";
  statusGlobal = Style["Iniciando c\[AAcute]lculo...", Yellow];
  forceFrontEndUpdateFA[];

  setStatus[msg_, color_: Yellow] := (
  statusGlobal = safeStatusFA[msg, color];
  progressText = safeMessageStringFA[msg];
  forceFrontEndUpdateFA[];
  Pause[0.05];
  );

  setProgress[frac_, msg_, color_: Yellow] := (
  progressValue = safeProgressValueFA[frac];
  statusGlobal = safeStatusFA[msg, color];
  progressText = safeMessageStringFA[msg];
  forceFrontEndUpdateFA[];
  Pause[0.05];
  If[TrueQ[cancelRequested], Throw[$Canceled]];
  );

  checkCancel[] := If[TrueQ[cancelRequested], Throw[$Canceled]];

  res = CheckAbort[
  Catch[

  setProgress[0.01, "Limpiando salidas anteriores y reiniciando variables internas...", Yellow];
  NotebookDelete @ Cells[CellStyle -> "Print"];
  NotebookDelete @ Cells[CellStyle -> "Message"];

  diags = None;
  diagsByChannel = {};
  channelLabels = {};
  ampFCprocByChannel = {};
  m2ActualModesByChannel = {};
  wcResult = {};
  failedDiagramChannels = {};
  failedAmpChannels = {};
  m2ActualModesLocal = {};
  $lastM2ActualModesByChannel = {};
  $lastM2PhysicalIncomplete = False;

  inList = Take[pickIn, nIn];
  outList = Take[pickOut, nOut];

  If[MemberQ[inList, None] || MemberQ[outList, None],
  setStatus["Selecciona todas las part\[IAcute]culas en in y out.", Red];
  Throw[$Failed];
  ];

  setProgress[0.02, "Limpiando cach\[EAcute] del sistema...", Yellow];
  ClearSystemCache[];

  setProgress[0.03, "Expandiendo estado inicial...", Yellow];
  initialInfo = expandInitialStatesFA[inList, ppScheme];

  If[! TrueQ[initialInfo["OK"]],
  setStatus[initialInfo["Mensaje"], Red];
  MessageDialog[initialInfo["Mensaje"]];
  Throw[$Failed];
  ];

  checkCancel[];
  initialChannels = initialInfo["Channels"];
  totalChannels = Length[initialChannels];

  If[loopOrder > 0 && MemberQ[inList, PP] && ! TrueQ[allowOneLoopPP],
  setStatus[
  "C\[AAcute]lculo detenido: 1-loop con estados hadr\[OAcute]nicos expandidos puede generar much\[IAcute]simos diagramas. " <>
  "Activa la casilla de permiso o calcula un canal part\[OAcute]nico concreto, por ejemplo g g -> t tbar.",
  Red
  ];
  MessageDialog[
  "Has seleccionado LoopOrder = 1 con un estado inicial que contiene protones.\n\n" <>
  "La interfaz tendr\[IAcute]a que expandir el prot\[OAcute]n en varios canales part\[OAcute]nicos y calcular 1-loop para cada uno, " <>
  "lo que puede tardar mucho o bloquear el kernel.\n\n" <>
  "Recomendaci\[OAcute]n: usa LoopOrder = 0 para estados con protones, o calcula 1-loop canal a canal, por ejemplo g g -> t tbar."
  ];
  Throw[$Failed];
  ];

  If[totalChannels == 0,
  setStatus["No se ha generado ning\[UAcute]n canal inicial.", Red];
  Throw[$Failed];
  ];

  nInEff = Length[First[initialChannels]];

  (* Salvaguardas de rendimiento/f\[IAcute]sica:
  - SMEFTsim est\[AAcute] orientado a LO; 1-loop simb\[OAcute]lico con |M|^2 puede bloquear.
  - En hadrones expandidos, |M|^2 f\[IAcute]sico canal a canal puede explotar. *)
  If[loopOrder > 0 && m2Mode === "Fisico",
  m2Mode = "Estructural";
  setStatus["Aviso: LoopOrder > 0. Se cambia |M|^2 de F\[IAcute]sico a Estructural para evitar abortos.", Orange];
  ];

  If[MemberQ[inList, PP] && m2Mode === "Fisico",
  m2Mode = "Estructural";
  setStatus["Aviso: proceso hadr\[OAcute]nico expandido. Se cambia |M|^2 de F\[IAcute]sico a Estructural por seguridad.", Orange];
  ];

  setProgress[
  0.06,
  initialInfo["Mensaje"] <> "\nCanales a evaluar: " <> ToString[totalChannels],
  Yellow
  ];

  setProgress[0.08, "Validando Model y GenericModel...", Yellow];
  checkModelo = validarModelosFA[modelName, genName];

  If[! TrueQ[checkModelo["OK"]],
  setStatus[checkModelo["Mensaje"], Red];
  MessageDialog[checkModelo["Mensaje"]];
  Throw[$Failed];
  ];

  checkCancel[];
  modelToUse = checkModelo["ModelName"];
  genToUse = checkModelo["GenericName"];

  setProgress[0.10, "Model validado: " <> modelToUse <> ". GenericModel validado: " <> genToUse <> ".", Yellow];

  setProgress[0.12, "Cargando par\[AAcute]metros del modelo .pars...", Yellow];
  parsStatus = cargarParametrosModeloFA[modelToUse];

  If[! TrueQ[parsStatus["OK"]],
  setStatus[parsStatus["Mensaje"], Red];
  MessageDialog[
  parsStatus["Mensaje"] <>
  "\n\nPara obtener coeficientes de Wilson hace falta que exista el fichero .pars correspondiente en la carpeta Models."
  ];
  Throw[$Failed];
  ];

  checkCancel[];

  If[WC2 === {},
  setStatus["No se han detectado coeficientes de Wilson en el fichero .pars del modelo.", Red];
  Throw[$Failed];
  ];

  setProgress[
  0.15,
  "Par\[AAcute]metros cargados. WCs detectados: " <> ToString[Length[WC2]] <> ".",
  Yellow
  ];

  (* Carga expl\[IAcute]cita doble del modelo. Esto evita tener que pulsar dos veces. *)
  setProgress[0.18, "Cargando modelo en FeynArts, pasada 1/2...", Yellow];
  Quiet @ Check[withoutNotebookPrintsFA @ InitializeModel[modelToUse, GenericModel -> genToUse], Null];
  checkCancel[];

  setProgress[0.21, "Cargando modelo en FeynArts, pasada 2/2...", Yellow];
  Quiet @ Check[withoutNotebookPrintsFA @ InitializeModel[modelToUse, GenericModel -> genToUse], Null];
  checkCancel[];

  setProgress[0.23, "Modelo inicializado. Preparando topolog\[IAcute]as...", Yellow];

  setProgress[0.25, "Creando topolog\[IAcute]as: " <> topologyModeLabelFA[topologyMode] <> "...", Yellow];
  topologyExcl = topologiesToExcludeFA[topologyMode];

  top = TimeConstrained[
  Quiet @ Check[
  CreateTopologies[
  loopOrder,
  nInEff -> nOut,
  Adjacencies -> {3, 4},
  ExcludeTopologies -> topologyExcl
  ],
  $Failed
  ],
  120,
  $TimedOut
  ];

  checkCancel[];

  If[top === $TimedOut || top === $Failed || top === $Aborted,
  setStatus["Fallo/timeout en CreateTopologies.", Red];
  Throw[$Failed];
  ];

  setProgress[0.29, "Topolog\[IAcute]as creadas correctamente.", Yellow];

  (* Pasada de precarga con el primer canal. No se usa el resultado final;
  sirve para forzar la inicializaci\[OAcute]n completa del modelo. *)
  setProgress[0.30, "Pre-cargando diagramas con el primer canal...", Yellow];

  TimeConstrained[
  Quiet @ Check[
  withoutNotebookPrintsFA @ InsertFields[
  top,
  First[initialChannels] -> outList,
  InsertionLevel -> insertion,
  Model -> modelToUse,
  GenericModel -> genToUse
  ],
  $Failed
  ],
  60,
  $TimedOut
  ];

  checkCancel[];
  setProgress[0.33, "Precarga finalizada. Generando diagramas reales canal por canal...", Yellow];

  diagsByChannel = {};
  validChannels = {};

  Do[
  channel = initialChannels[[i]];
  lbl = channelLabelFA[channel];

  setProgress[
  0.34 + 0.16 (i - 1)/Max[1, totalChannels],
  "Generando diagramas, canal " <> ToString[i] <> "/" <> ToString[totalChannels] <> ": " <> lbl,
  Yellow
  ];

  tmpDiags = TimeConstrained[
  Quiet @ Check[
  withoutNotebookPrintsFA @ InsertFields[
  top,
  channel -> outList,
  InsertionLevel -> insertion,
  Model -> modelToUse,
  GenericModel -> genToUse
  ],
  $Failed
  ],
  120,
  $TimedOut
  ];

  checkCancel[];

  If[tmpDiags === $TimedOut || tmpDiags === $Failed || tmpDiags === $Aborted,
  AppendTo[failedDiagramChannels, lbl];
  setStatus["Sin diagramas o timeout en el canal " <> ToString[i] <> "/" <> ToString[totalChannels] <> ": " <> lbl <> ". Se contin\[UAcute]a.", Orange];
  ,
  AppendTo[diagsByChannel, tmpDiags];
  AppendTo[validChannels, channel];
  ];
  ,
  {i, 1, totalChannels}
  ];

  If[diagsByChannel === {},
  setStatus["No se han generado diagramas para ning\[UAcute]n canal part\[OAcute]nico. Revisa el proceso/modelo.", Red];
  Throw[$Failed];
  ];

  channelLabels = channelLabelFA /@ validChannels;
  diags = First[diagsByChannel];

  setProgress[
  0.52,
  "Diagramas generados: " <> ToString[Length[diagsByChannel]] <> "/" <> ToString[totalChannels] <>
  If[failedDiagramChannels === {}, "", "\nCanales sin diagramas/timeout: " <> StringRiffle[failedDiagramChannels, ", "]],
  If[failedDiagramChannels === {}, Yellow, Orange]
  ];

  (* No pintamos los diagramas durante el c\[AAcute]lculo principal.
  Los diagramas se pintar\[AAcute]n solo cuando se pulse Exportar diagramas a PDF. *)

  setProgress[0.55, "Calculando amplitudes canal por canal...", Yellow];
  ampFCprocByChannel = {};
  nAmp = Length[diagsByChannel];

  Do[
  lbl = channelLabels[[i]];
  step = 0.37/Max[1, nAmp];
  base = 0.55 + step (i - 1);

  setProgress[
  base,
  "CreateFeynAmp, canal " <> ToString[i] <> "/" <> ToString[nAmp] <> ": " <> lbl,
  Yellow
  ];

  tmpFA = TimeConstrained[
  Quiet @ Check[
  withoutNotebookPrintsFA @ CreateFeynAmp[diagsByChannel[[i]]],
  $Failed
  ],
  1200,
  $TimedOut
  ];

  checkCancel[];

  If[tmpFA === $TimedOut || tmpFA === $Failed || tmpFA === $Aborted,
  AppendTo[failedAmpChannels, "CreateFeynAmp: " <> lbl];
  setStatus["Fallo/timeout en CreateFeynAmp para el canal " <> lbl <> ". Se contin\[UAcute]a con el siguiente canal.", Orange];
  Continue[];
  ];

  setProgress[
  base + step/5,
  "FCFAConvert, canal " <> ToString[i] <> "/" <> ToString[nAmp] <> ": " <> lbl,
  Yellow
  ];

  tmpFC = TimeConstrained[
  Quiet @ Check[
  withoutNotebookPrintsFA @ FCFAConvert[
  tmpFA,
  (* Ruta estable: mantener la forma que ya funcionaba en tu version.
  Los simbolos p[1], p[2], k[1], ... son los que FCFAConvert estaba
  aceptando correctamente con SMEFTsim/FeynArts. El cambio a pIn1,
  pIn2 puede hacer fallar FCFAConvert en algunas versiones. *)
  IncomingMomenta -> Array[p, nInEff],
  OutgoingMomenta -> Array[k, nOut],
  ChangeDimension -> 4,
  Sequence @@ fcfaConvertExtraOptionsFA[],
  List -> False,
  FCFADiracChainJoin -> False
  ] // FCI,
  $Failed
  ],
  1200,
  $TimedOut
  ];

  checkCancel[];

  If[tmpFC === $TimedOut || tmpFC === $Failed || tmpFC === $Aborted,
  AppendTo[failedAmpChannels, "FCFAConvert: " <> lbl];
  setStatus["Fallo/timeout en FCFAConvert para el canal " <> lbl <> ". Se contin\[UAcute]a con el siguiente canal.", Orange];
  Continue[];
  ];

  (* Si se activa Top-sector, los WCs ajenos al sector se anulan antes
  de cualquier simplificacion o construccion de |M|^2. *)
  tmpFC = restrictToActiveWCSectorFA[tmpFC];

  (* Simplificaci\[OAcute]n temprana de M solo cuando se buscan WCs directamente en M.
  Si se va a construir |M|^2, simplificar M antes suele aumentar el tama\[NTilde]o
  y puede hacer que el producto M M* explote. *)
  If[m2Mode === "Off",
  setProgress[
  base + step/5,
  "Simplificando amplitud M, canal " <> ToString[i] <> "/" <> ToString[nAmp] <>
  " (" <> simplificationMode <> ", " <> ToString[simplificationTimeout] <> " s): " <> lbl,
  Yellow
  ];
  tmpFC = safeSimplifyFA[tmpFC, simplificationMode, simplificationTimeout];
  checkCancel[];
  ];

  (* Optimizaci\[OAcute]n (idea restrict_massless de SMEFTsim): si se piden leptones
  sin masa y se va a elevar al cuadrado, se anulan las masas/Yukawas
  lept\[OAcute]nicos YA sobre M. As\[IAcute] los espinores entran sin masa, FermionSpinSum
  usa la completitud sin masa, las trazas son m\[AAcute]s cortas y la supresi\[OAcute]n por
  quiralidad ocurre de forma exacta dentro de la traza. *)
  If[m2Mode =!= "Off" && useMasslessLeptonRules,
  tmpFC = tmpFC /. buildSymmetryRules[]["LeptonMassless"];
  checkCancel[];
  ];

  (* ===== Construcci\[OAcute]n de |M|^2 ===== *)
  If[m2Mode === "Off",
  (* Comportamiento cl\[AAcute]sico: simetr\[IAcute]as y b\[UAcute]squeda de WCs sobre M. *)
  tmpM2 = tmpFC,

  (* Nuevo: a partir de aqu\[IAcute] todo se hace sobre |M|^2. *)
  setProgress[
  base + 2 step/5,
  "Calculando |M|^2 (" <> m2Mode <> "), canal " <> ToString[i] <> "/" <> ToString[nAmp] <> ": " <> lbl,
  Yellow
  ];

  momInEff = Array[p, nInEff];
  momOutEff = Array[k, nOut];

  tmpM2 = computeAmplitudeSquaredFA[
  tmpFC,
  validChannels[[i]], outList,
  momInEff, momOutEff,
  m2Mode, m2DoColor,
  simplificationMode, simplificationTimeout, m2Timeout
  ];
  AppendTo[m2ActualModesLocal, lbl <> " -> " <> ToString[$m2ActualModeFA]];
  checkCancel[];
  ];

  (* No simplificar |M|^2 antes de aplicar reglas: primero se eliminan t\[EAcute]rminos
  por CP/reales/masa/linealidad y solo despu\[EAcute]s se simplifica la expresi\[OAcute]n
  ya reducida. En modo Off se simplific\[OAcute] M antes. *)
  checkCancel[];

  (* Simetr\[IAcute]as/filtros aplicados sobre |M|^2 (o M en modo Off). *)
  setProgress[
  base + 4 step/5,
  "Aplicando simetr\[IAcute]as/filtros sobre " <> If[m2Mode === "Off", "M", "|M|^2"] <> ", canal " <> ToString[i] <> "/" <> ToString[nAmp] <> ": " <> lbl,
  Yellow
  ];

  skipLinearNow = TrueQ[m2Mode =!= "Off" && useLinearWCRules && TrueQ[$m2LinearBuiltFA]];
  tmpProc = applySelectedSymmetries[tmpM2, skipLinearNow];
  checkCancel[];

  tmpProc = safeSimplifyFA[tmpProc, simplificationMode, simplificationTimeout];
  checkCancel[];

  AppendTo[ampFCprocByChannel, tmpProc];
  ,
  {i, 1, nAmp}
  ];

  m2ActualModesByChannel = m2ActualModesLocal;
  $lastM2ActualModesByChannel = m2ActualModesByChannel;
  $lastM2PhysicalIncomplete = TrueQ[$m2PhysicalIncompleteFA];

  If[ampFCprocByChannel === {},
  setStatus[
  "No se ha podido calcular ninguna amplitud part\[OAcute]nica." <>
  If[failedAmpChannels === {},
  "",
  "\nFases/canales fallidos: " <> StringRiffle[failedAmpChannels, ", "]
  ] <>
  "\nSugerencia: prueba primero con |M|^2 = Off o Estructural, simplificacion = Ninguna.",
  Red
  ];
  Throw[$Failed];
  ];

  setProgress[
  0.93,
  "Amplitudes calculadas: " <> ToString[Length[ampFCprocByChannel]] <> "/" <> ToString[nAmp] <>
  If[failedAmpChannels === {}, "", "\nCanales/fases fallidos: " <> StringRiffle[failedAmpChannels, ", "]],
  If[failedAmpChannels === {}, Yellow, Orange]
  ];

  setProgress[0.94, "Extrayendo coeficientes de Wilson de las amplitudes procesadas...", Yellow];

  wcResult = DeleteDuplicates @ Select[
  activeWCsFA[],
  Function[wc,
  AnyTrue[
  ampFCprocByChannel,
  wcAppearsInExprFA[#, wc] &
  ]
  ]
  ];

  setProgress[0.98, "Ordenando y preparando resultados...", Yellow];
  wcResult = SortBy[wcResult, SymbolName];

  progressValue = 1.;
  progressText = "C\[AAcute]lculo completado.";
  statusGlobal = Style[
  "C\[AAcute]lculo completado. Coeficientes de Wilson obtenidos: " <> ToString[Length[wcResult]] <>
  "\nCanales part\[OAcute]nicos con diagramas: " <> ToString[Length[diagsByChannel]] <>
  "\nCanales con amplitud calculada: " <> ToString[Length[ampFCprocByChannel]] <>
  If[TrueQ[useTopSectorRules], "\nSector de WCs activo: Top-sector (" <> ToString[Length[activeWCsFA[]]] <> " WCs activos; aplicado antes de |M|^2)", "\nSector de WCs activo: SMEFTsim completo"] <>
  If[safeCountFA[m2ActualModesByChannel] == 0, "", "\nModo real de |M|^2 usado: " <> safeShortListFA[m2ActualModesByChannel, 20]] <>
  If[m2Mode === "Fisico" && AnyTrue[safeStringListFA[m2ActualModesByChannel], StringContainsQ[#, "NoConclusivo"] &],
  "\nAviso importante: el modo fisico no ha sido concluyente en al menos un canal; los WCs no deben compararse como |M|^2 fisica completa.",
  ""
  ] <>
  If[safeCountFA[Quiet @ Check[$lastM2PhysicalCandidates, {}]] == 0, "", "\nCandidatos fisicos analizados: " <> ToString[safeCountFA[$lastM2PhysicalCandidates]] <> " (ver $lastM2PhysicalCandidates)"] <>
  If[safeCountFA[Quiet @ Check[$lastM2PhysicalUndecided, {}]] == 0, "", "\nAviso: WCs no concluyentes en |M|^2 fisica: " <> ToString[safeCountFA[$lastM2PhysicalUndecided]] <> " (ver $lastM2PhysicalUndecided). Primeros: " <> safeShortListFA[$lastM2PhysicalUndecided, 8]] <>
  If[safeCountFA[Quiet @ Check[$lastM2PhysicalFailed, {}]] == 0, "", "\nAviso: WCs con fallo tecnico en |M|^2 fisica: " <> ToString[safeCountFA[$lastM2PhysicalFailed]] <> " (ver $lastM2PhysicalFailed). Primeros: " <> safeShortListFA[$lastM2PhysicalFailed, 8]] <>
  If[safeCountFA[failedDiagramChannels] == 0, "", "\nAviso: canales sin diagramas/timeout: " <> safeShortListFA[failedDiagramChannels, 10]] <>
  If[safeCountFA[failedAmpChannels] == 0, "", "\nAviso: canales/fases de amplitud fallidos: " <> safeShortListFA[failedAmpChannels, 10]],
  Darker[Green]
  ];
  forceFrontEndUpdateFA[];

  $Succeeded
  ],
  $Aborted
  ];

  If[res === $Canceled,
  progressText = "C\[AAcute]lculo cancelado por el usuario.";
  statusGlobal = Style["C\[AAcute]lculo cancelado por el usuario.", Orange];
  forceFrontEndUpdateFA[];
  ];

  If[res === $Aborted,
  progressText = "C\[AAcute]lculo abortado.";
  statusGlobal = Style["C\[AAcute]lculo abortado. Si has pulsado Resetear/Limpiar todo, la ejecuci\[OAcute]n se ha detenido.", Orange];
  forceFrontEndUpdateFA[];
  ];

  If[res === $Failed && running,
  progressText = "C\[AAcute]lculo detenido.";
  forceFrontEndUpdateFA[];
  ];

  running = False;
  cancelRequested = False;
  res
  ]
  ],
  Method -> "Queued",
  ImageSize -> {300, 35}
  ],
  Spacer[8],
  
  Row[
  {
  Button[
  Dynamic[If[running, "Cancelar y limpiar", "Resetear/Limpiar todo"]],
  If[TrueQ[running],
  cancelRequested = True;
  statusGlobal = Style["Cancelando c\[AAcute]lculo y limpiando interfaz...", Orange];
  progressText = "Cancelando c\[AAcute]lculo...";
  forceFrontEndUpdateFA[];
  Quiet @ Check[FrontEndExecute[FrontEndToken[EvaluationNotebook[], "EvaluatorAbort"]], Null];
  Pause[0.1];
  ];
  running = False;
  cancelRequested = True;
  NotebookDelete @ Cells[CellStyle -> "Print"];
  NotebookDelete @ Cells[CellStyle -> "Message"];

  (* Resultados del c\[AAcute]lculo *)
  diags = None;
  diagsByChannel = {};
  channelLabels = {};
  ampFCprocByChannel = {};
  m2ActualModesByChannel = {};
  $lastM2ActualModesByChannel = {};
  $lastM2PhysicalIncomplete = False;
  wcResult = {};

  (* Selecci\[OAcute]n de proceso *)
  nIn = 2;
  nOut = 2;
  catInList = ConstantArray["Leptones", 6];
  catOutList = ConstantArray["Quarks", 6];
  pickIn = ConstantArray[None, 6];
  pickOut = ConstantArray[None, 6];

  (* Opciones de c\[AAcute]lculo *)
  insertion = {Particles};
  loopOrder = 0;
  topologyMode = "TreeFull";
  ppScheme = "ttbarLO";
  allowOneLoopPP = False;
  simplificationMode = "Light";
  simplificationTimeout = 30;
  m2Mode = "Estructural";
  m2DoColor = False;
  m2Timeout = 120;

  (* Simetr\[IAcute]as/filtros *)
  useCpRules = False;
  useRealRules = False;
  useLinearWCRules = False;
  useMasslessLeptonRules = False;
  useTopSectorRules = False;
  
  (* Estado visual *)
  progressValue = 0.;
  progressText = "Esperando c\[AAcute]lculo.";
  statusGlobal = Style["Interfaz reseteada. Se han limpiado diagramas, canales, amplitudes, WCs y selecci\[OAcute]n de part\[IAcute]culas.", Orange];

  ClearSystemCache[];
  forceFrontEndUpdateFA[];
  ,
  Method -> "Preemptive"
  ],
  
  Spacer[10],
  
  Button[
  "Exportar diagramas a PDF",
  Module[
  {
  path, nCanales, hadronicQ, labelTxt, tmpDir, tmpFiles,
  tmpPath, g, ok, goodFiles, imported, importedPages,
  outDir, outFile, baseName, dirName, safeLabel
  },
  
  If[diagsByChannel === {} || ! ListQ[diagsByChannel],
  statusGlobal = Style["No hay diagramas para exportar. Primero calcula los coeficientes de Wilson.", Red];
  Return[];
  ];
  
  hadronicQ = usesHadronInitialQ[catInList, pickIn, nIn];
  nCanales = Length[diagsByChannel];
  
  path = SystemDialogInput["FileSave", "diagramas.pdf"];
  If[path === $Canceled, Return[]];
  
  statusGlobal = Style[
  If[nCanales > 1,
  "Exportando diagramas canal por canal y componiendo el PDF final...",
  "Exportando diagramas a PDF..."
  ],
  Yellow
  ];
  forceFrontEndUpdateFA[];
  
  tmpDir = CreateDirectory[];
  tmpFiles = {};
  
  Do[
  labelTxt = If[ListQ[channelLabels] && Length[channelLabels] >= i,
  ToString[channelLabels[[i]]],
  "canal " <> ToString[i]
  ];
  
  safeLabel = StringReplace[labelTxt,
  {" " -> "_", "/" -> "-", "\\" -> "-", ":" -> "-", "*" -> "-", "?" -> "-", "\"" -> "", "<" -> "-", ">" -> "-", "|" -> "-"}
  ];
  
  tmpPath = FileNameJoin[{tmpDir, "canal_" <> ToString[i] <> "_" <> safeLabel <> ".pdf"}];
  
  statusGlobal = Style[
  "Pintando y exportando canal " <> ToString[i] <> "/" <> ToString[nCanales] <>
  If[labelTxt =!= "", ": " <> labelTxt, ""],
  Yellow
  ];
  forceFrontEndUpdateFA[];
  
  ok = Quiet @ Check[
  g = Paint[
  diagsByChannel[[i]],
  Numbering -> Simple,
  SheetHeader -> If[nCanales > 1, "Canal partonico: " <> labelTxt, None],
  ColumnsXRows -> {3, 2},
  ImageSize -> 800
  ];
  Export[tmpPath, g, "PDF"];
  FileExistsQ[tmpPath],
  False
  ];
  
  If[TrueQ[ok], AppendTo[tmpFiles, tmpPath]];
  ,
  {i, 1, nCanales}
  ];
  
  goodFiles = Select[tmpFiles, FileExistsQ];
  
  If[goodFiles === {},
  statusGlobal = Style["Error exportando los diagramas canal por canal.", Red];
  Return[];
  ];
  
  If[Length[goodFiles] == 1,
  Quiet @ Check[
  CopyFile[First[goodFiles], path, OverwriteTarget -> True],
  statusGlobal = Style["Error copiando el PDF exportado.", Red];
  Return[];
  ];
  
  statusGlobal = Style["PDF exportado: " <> ToString[path], Darker[Green]];
  Return[];
  ];
  
  statusGlobal = Style["Combinando canales en un unico PDF...", Yellow];
  forceFrontEndUpdateFA[];
  
  imported = Quiet @ Check[Import[#, "Images"], {}] & /@ goodFiles;
  importedPages = Select[Flatten[(If[ListQ[#], #, {#}] &) /@ imported, 1], ImageQ];
  
  If[importedPages =!= {},
  Quiet @ Check[
  Export[path, importedPages, "PDF"],
  statusGlobal = Style["Error combinando los canales en un unico PDF.", Red];
  Return[];
  ];
  
  statusGlobal = Style[
  "PDF exportado: " <> ToString[path] <>
  "\nSe han exportado " <> ToString[Length[goodFiles]] <> " canales partonicos.",
  Darker[Green]
  ];
  Return[];
  ];
  
  (* Fallback: si no se pueden recombinar las paginas, se dejan PDFs separados. *)
  dirName = DirectoryName[path];
  baseName = FileBaseName[path];
  outDir = FileNameJoin[{dirName, baseName <> "_canales"}];
  If[! DirectoryQ[outDir], CreateDirectory[outDir]];
  
  Do[
  outFile = FileNameJoin[{outDir, FileNameTake[goodFiles[[i]]]}];
  Quiet @ Check[CopyFile[goodFiles[[i]], outFile, OverwriteTarget -> True], Null];
  ,
  {i, 1, Length[goodFiles]}
  ];
  
  statusGlobal = Style[
  "No se pudo recombinar en un unico PDF, pero se han exportado los canales por separado en:\n" <> ToString[outDir],
  Orange
  ];
  ],
  Method -> "Queued"
  ]
  }
  ],
  
  Delimiter,
  
  Dynamic[
  Column[
  {
  statusGlobal,
  ProgressIndicator[Dynamic[progressValue], {0, 1}, ImageSize -> 800],
  Row[
  {
  Style["Progreso: ", Bold],
  Dynamic[progressText],
  Spacer[12],
  Dynamic[NumberForm[100 * safeProgressValueFA[progressValue], {4, 1}]],
  "%"
  }
  ]
  }
  ],
  TrackedSymbols :> {statusGlobal, progressValue, progressText, running, cancelRequested},
  SynchronousUpdating -> False
  ],
  
  Dynamic[
  If[wcResult =!= {},
  Column[
  {
  Style["WCs obtenidos:", 12, Bold],
  Pane[wcResult, {800, 200}, Scrollbars -> True]
  }
  ],
  Nothing
  ],
  TrackedSymbols :> {wcResult}
  ],
  
  Dynamic[
  If[channelLabels =!= {} && usesHadronInitialQ[catInList, pickIn, nIn],
  Column[
  {
  Style["Canales part\[OAcute]nicos evaluados:", 12, Bold],
  Pane[Column[channelLabels], {800, 120}, Scrollbars -> True]
  }
  ],
  Nothing
  ],
  TrackedSymbols :> {channelLabels, catInList, pickIn, nIn}
  ]
  },
  Spacings -> 1.2
  ]
  ];



(* ================================================================ *)
(* GUIAS FINALES DE LA INTERFAZ  *)
(* Estas definiciones sustituyen a las guias breves anteriores.  *)
(* ================================================================ *)

ClearAll[mostrarGuiaUso, mostrarGuiaModificaciones, mostrarGuiaErrores];

mostrarGuiaUso[] := CreateDialog[
  DynamicModule[{},
  Pane[
  Column[{
  Style["Guia de uso de la interfaz SMEFTsim/FeynArts/FeynCalc", 17, Bold],
  guiaNoteFA["Objetivo: generar diagramas, calcular amplitudes y extraer los coeficientes de Wilson que aparecen en M o en |M|^2, con control de sector activo, truncamiento lineal y reglas fisicas."],
  guiaOpenFA["1. Estructura de carpetas y modelos", {
  guiaBulletFA[Row[{"El archivo ", guiaCodeFA[".wl"], " debe estar en una carpeta de trabajo."}]],
  guiaBulletFA[Row[{"La carpeta local ", guiaCodeFA["Models"], " debe contener los ficheros ", guiaCodeFA[".mod"], ", ", guiaCodeFA[".gen"], " y preferentemente ", guiaCodeFA[".pars"], " del modelo."}]],
  guiaBulletFA[Row[{"Para ", guiaCodeFA["SMEFTsimtopU3l"], " se esperan ", guiaCodeFA["Models/SMEFTsimtopU3l.mod"], ", ", guiaCodeFA["Models/SMEFTsimtopU3l.gen"], " y ", guiaCodeFA["Models/SMEFTsimtopU3l.pars"], "."}]],
  guiaWarnFA["No pongas rutas completas en Model/GenericModel; escribe solo el nombre base del modelo."]
  }, True],
  guiaOpenFA["2. Seleccion de proceso", {
  guiaBulletFA[Row[{"Define el numero de entrantes/salientes y elige cada particula. Para el caso de referencia usa ", guiaCodeFA["e- e+ -> t tbar"], "."}]],
  guiaBulletFA[Row[{guiaCodeFA["InsertionLevel -> {Particles}"], " es el modo mas explicito para identificar WCs concretos."}]],
  guiaBulletFA[Row[{guiaCodeFA["InsertionLevel -> {Classes}"], " puede ser mas compacto, pero puede ocultar detalle de sabor/clase."}]],
  guiaBulletFA[Row[{guiaCodeFA["LoopOrder = 0"], " es el modo recomendado. Los loops simbolicos en SMEFTsim pueden crecer muy rapido."}]],
  guiaWarnFA["Para loops o procesos 2 -> 3 evita |M|^2 fisica completa salvo pruebas muy controladas."]
  }, False],
  guiaOpenFA["3. Colisiones hadronicas p p", {
  guiaBulletFA[Row[{"El boton ", guiaCodeFA["Usar colision p p"], " configura dos protones entrantes."}]],
  guiaBulletFA["La interfaz expande p p a canales partonicos y elimina duplicados bajo intercambio de protones."],
  guiaBulletFA[Row[{guiaCodeFA["ttbarLO"], ": recomendado para p p -> t tbar; incluye q qbar y g g principales."}]],
  guiaBulletFA[Row[{guiaCodeFA["4FS/5FS"], ": usa quarks ligeros; 5FS incluye b/bbar."}]],
  guiaBulletFA[Row[{guiaCodeFA["6FSFormal"], ": incluye t/tbar como partones formales; no es un PDF fisico estandar."}]],
  guiaWarnFA["Para p p y 2 -> 3 usa preferentemente |M|^2 Estructural, Lineal en WCs y Simplificacion Ninguna/Ligera."]
  }, False],
  guiaOpenFA["4. Reglas fisicas y sector activo", {
  guiaBulletFA[Row[{Style["CP conservante", Bold], ": anula coeficientes imaginarios y CP-odd identificados por la notacion del modelo."}]],
  guiaBulletFA[Row[{Style["WCs reales", Bold], ": anula las componentes independientes terminadas en ", guiaCodeFA["Im"], "."}]],
  guiaBulletFA[Row[{Style["Lineal en WCs", Bold], ": conserva la interferencia SM x dim-6, es decir, terminos de orden Lambda^-2; elimina/sortea los productos C_i C_j de orden Lambda^-4."}]],
  guiaBulletFA[Row[{Style["Leptones sin masa", Bold], ": aplica m_l -> 0 y Yukawas leptonicas -> 0. No elimina corrientes vectoriales leptonicas."}]],
  guiaBulletFA[Row[{Style["Top-sector", Bold], ": restringe los WCs activos antes del calculo al sector top. No es un filtrado posterior; equivale a usar un submodelo/restriction card."}]],
  guiaWarnFA["Para trabajar en el subespacio top, activa Top-sector antes del calculo. CP, WCs reales, Lineal y Leptones sin masa son hipotesis fisicas independientes."]
  }, True],
  guiaOpenFA["5. Modos de |M|^2", {
  guiaBulletFA[Row[{guiaCodeFA["Off"], ": no calcula |M|^2; busca WCs en la amplitud M procesada. Es rapido y da candidatos amplios."}]],
  guiaBulletFA[Row[{guiaCodeFA["Estructural"], ": construye M M* o la interferencia lineal directa si procede. No hace toda la suma fisica de espines/polarizaciones; puede ser sobre-inclusivo."}]],
  guiaBulletFA[Row[{guiaCodeFA["Fisico estricto"], ": intenta FermionSpinSum, sumas de polarizacion y simplificacion. Si no es concluyente, se marca como no concluyente; no debe confundirse con un resultado fisico cerrado."}]],
  guiaBulletFA[Row[{"El Status muestra el ", guiaCodeFA["Modo real de |M|^2 usado"], ". Si contiene ", guiaCodeFA["NoConclusivo"], " el resultado no es una |M|^2 fisica demostrada."}]],
  guiaWarnFA["La configuracion mas estable para top-sector e- e+ -> t tbar es |M|^2 Estructural + Lineal + Top-sector. Para una comprobacion fisica, prueba Fisico estricto con timeout alto."]
  }, True],
  guiaOpenFA["6. Resultado y variables utiles", {
  guiaBulletFA[Row[{guiaCodeFA["lastWCNames"], ": nombres de WCs obtenidos en la ultima ejecucion."}]],
  guiaBulletFA[Row[{guiaCodeFA["lastWCResult"], ": lista simbolica de WCs."}]],
  guiaBulletFA[Row[{guiaCodeFA["$lastM2ActualModesByChannel"], ": modo real usado por canal para |M|^2."}]],
  guiaBulletFA[Row[{guiaCodeFA["$lastM2PhysicalCandidates"], ": candidatos analizados en modo fisico."}]],
  guiaBulletFA[Row[{guiaCodeFA["$lastM2PhysicalUndecided"], ": WCs no concluyentes por timeout/simplificacion."}]],
  guiaBulletFA[Row[{guiaCodeFA["$lastM2PhysicalFailed"], ": WCs con fallo tecnico."}]],
  guiaBulletFA[Row[{"Puedes traducir los resultados con ", guiaCodeFA["translateWCListRenderedTable[lastWCNames, ...]"], " si has cargado el modulo de traducciones."}]]
  }, False],
  guiaOpenFA["7. Recomendaciones rapidas", {
  guiaBulletFA[Row[{"SMEFTsim completo: desactiva Top-sector; interpreta el resultado como dependencias del modelo completo, no como subconjunto top."}]],
  guiaBulletFA[Row[{"Procesos p p: usa esquema partonico minimo, Lineal en WCs y evita |M|^2 Fisico salvo canales partonicos pequenos."}]],
  guiaBulletFA[Row[{"Loops: empieza con |M|^2 Off o Estructural. La |M|^2 fisica con loops simbolicos suele no ser viable."}]]
  }, False],
  DefaultButton["Cerrar", DialogReturn[]]
  }, Spacings -> 0.85],
  {900, 650},
  Scrollbars -> True
  ]
  ],
  WindowTitle -> "Guia de uso"
];

mostrarGuiaModificaciones[] := CreateDialog[
  DynamicModule[{},
  Pane[
  Column[{
  Style["Guia de modificaciones y mantenimiento", 17, Bold],
  guiaNoteFA["Esta guia resume donde tocar el codigo sin romper la estructura de la interfaz. Haz siempre una copia antes de modificar."],
  guiaOpenFA["1. Cambiar o anadir modelos", {
  guiaBulletFA[Row[{"Copia ", guiaCodeFA[".mod/.gen/.pars"], " dentro de ", guiaCodeFA["Models"], "."}]],
  guiaBulletFA[Row[{"La validacion esta en ", guiaCodeFA["validarModelosFA"], " y la carpeta se obtiene con ", guiaCodeFA["faModelsDir[]"], "."}]],
  guiaBulletFA[Row[{"La carga de parametros y WCs esta en ", guiaCodeFA["cargarParametrosModeloFA[model_]"], "."}]],
  guiaWarnFA["Si otro modelo usa nombres de WCs distintos, revisa el filtrado de parametros SM y las reglas de CP/reales."]
  }, True],
  guiaOpenFA["2. Catalogo de particulas", {
  guiaBulletFA[Row[{"Las categorias y etiquetas se definen en las funciones de catalogo: ", guiaCodeFA["particleCatalogFA"], ", ", guiaCodeFA["particleLabelFA"], " y menus asociados."}]],
  guiaBulletFA["Para anadir una particula, anade el campo FeynArts correcto y una etiqueta clara."],
  guiaBulletFA["No pongas protones en salientes: la interfaz los trata solo como entrada hadronica expansible."]
  }, False],
  guiaOpenFA["3. Sector activo de WCs", {
  guiaBulletFA[Row[{"El criterio Top-sector esta en ", guiaCodeFA["topSectorWCQFA[wc_]"], "."}]],
  guiaBulletFA[Row[{"La lista activa la devuelve ", guiaCodeFA["activeWCsFA[]"], ". Todas las busquedas deben usar esta funcion, no WC2 directamente, salvo para poner a cero todo en el limite SM."}]],
  guiaBulletFA[Row[{"Los WCs inactivos se anulan con ", guiaCodeFA["inactiveTopSectorWCRulesFA[]"], " mediante ", guiaCodeFA["restrictToActiveWCSectorFA"], "."}]],
  guiaWarnFA["Cuidado con prefijos ambiguos: por ejemplo cHbq no debe capturar cHbox. Por eso se usa n === \"cHbq\"."]
  }, True],
  guiaOpenFA["4. Reglas fisicas", {
  guiaBulletFA[Row[{"Las reglas se crean en ", guiaCodeFA["buildSymmetryRules[]"], " y se aplican en ", guiaCodeFA["applySelectedSymmetries[expr, skipLinear]"], "."}]],
  guiaBulletFA[Row[{"CP: por defecto anula WCs independientes que terminan en ", guiaCodeFA["Im"], " o ", guiaCodeFA["til"], "."}]],
  guiaBulletFA[Row[{"WCs reales: anula los WCs independientes terminados en ", guiaCodeFA["Im"], ". Las conjugaciones de Re/Im se normalizan automaticamente."}]],
  guiaBulletFA[Row[{"Leptones sin masa: revisa ", guiaCodeFA["leptonMasslessRulesLocal"], " si otro modelo usa nombres distintos para masas, eigenvalores Yukawa o matrices Yukawa indexadas."}]],
  guiaBulletFA[Row[{"Linealidad: ", guiaCodeFA["keepLinearWCFA"], " cuenta el grado en WCs y conserva grado <= 1."}]],
  guiaWarnFA["No uses reglas globales con //., Times[a_,b_], etc. en expresiones grandes: pueden explotar combinatoriamente."]
  }, False],
  guiaOpenFA["5. |M|^2 y rendimiento", {
  guiaBulletFA[Row[{"El nucleo esta en ", guiaCodeFA["computeAmplitudeSquaredFA"], "."}]],
  guiaBulletFA[Row[{"Antes de cuadrar se llama a ", guiaCodeFA["preprocessAmplitudeForM2FA"], " para aplicar reglas baratas."}]],
  guiaBulletFA[Row[{"Si Lineal esta activo, se separan ", guiaCodeFA["M_SM"], " y ", guiaCodeFA["M_dim6"], " con ", guiaCodeFA["linearAmplitudePartFA"], " y se evita construir C_i C_j."}]],
  guiaBulletFA[Row[{"El modo fisico usa ", guiaCodeFA["reduceSpinColorChunkFA"], " y, si procede, bloques con ", guiaCodeFA["physicalM2ChunkedLinearFA"], "."}]],
  guiaWarnFA["No apliques FeynAmpDenominatorExplicit antes de FermionSpinSum en SMEFTsim grande; suele agrandar mucho la expresion."]
  }, True],
  guiaOpenFA["6. Loops y hadrones", {
  guiaBulletFA["La interfaz cambia de modo Fisico a Estructural en LoopOrder > 0 o p p expandido para evitar abortos."],
  guiaBulletFA["Para p p, reduce el numero de canales usando ttbarLO cuando sea posible."],
  guiaBulletFA["Para 2 -> 3, no uses FullSimplify; usa Ninguna o Ligera y timeout bajo."],
  guiaBulletFA["Para verificar un resultado fisico, prueba primero un canal partonico individual pequeno."]
  }, False],
  guiaOpenFA["7. Status y variables globales", {
  guiaBulletFA[Row[{"Los mensajes se escriben con ", guiaCodeFA["setStatus"], " y ", guiaCodeFA["setProgress"], "."}]],
  guiaBulletFA[Row[{"La salida final debe indicar siempre sector activo, modo real de |M|^2, canales fallidos y WCs no concluyentes."}]]
  }, False],
  DefaultButton["Cerrar", DialogReturn[]]
  }, Spacings -> 0.85],
  {900, 650},
  Scrollbars -> True
  ]
  ],
  WindowTitle -> "Guia de modificaciones"
];

mostrarGuiaErrores[] := CreateDialog[
  DynamicModule[{},
  Pane[
  Column[{
  Style["Guia de errores", 17, Bold],
  guiaOpenFA["1. No se encuentra Model/GenericModel", {
  guiaBulletFA["Comprueba que el .wl y la carpeta Models estan en la misma carpeta."],
  guiaBulletFA[Row[{"Ejecuta ", guiaCodeFA["faModelsDir[]"], " y ", guiaCodeFA["FileNames[\"*\", faModelsDir[]]"], " para ver que detecta Mathematica."}]],
  guiaBulletFA["No incluyas la extension .mod/.gen en los campos de la interfaz."]
  }, True],
  guiaOpenFA["2. No se detectan WCs en .pars", {
  guiaBulletFA[Row[{"La funcion afectada es ", guiaCodeFA["cargarParametrosModeloFA"], "."}]],
  guiaBulletFA["Comprueba que el .pars define M$ExtParams/M$IntParams y que los nombres no estan siendo filtrados como SM."],
  guiaBulletFA["Si usas otro modelo, revisa la lista de parametros SM excluidos."]
  }, False],
  guiaOpenFA["3. Sin diagramas", {
  guiaBulletFA["Puede deberse a proceso prohibido por el modelo, particulas mal elegidas, topologias demasiado restrictivas o nivel de insercion inadecuado."],
  guiaBulletFA["Prueba Arbol completo, LoopOrder=0 e InsertionLevel -> {Particles}."],
  guiaBulletFA["Si usas p p, revisa el esquema partonico y prueba primero un canal partonico concreto."]
  }, False],
  guiaOpenFA["4. Fallo en CreateFeynAmp o FCFAConvert", {
  guiaBulletFA["Reduce el proceso: 2 -> 2, LoopOrder=0, sin p p, simplificacion Ninguna."],
  guiaBulletFA["Si FCFAConvert falla, prueba con |M|^2 Off para separar el problema de conversion del problema de |M|^2."],
  guiaBulletFA["Evita opciones experimentales de FeynCalc si tu version no las soporta; la interfaz solo inserta opciones si existen."]
  }, False],
  guiaOpenFA["5. |M|^2 devuelve demasiados WCs", {
  guiaBulletFA["Comprueba el status: si el modo real es Estructural o FallbackStructural, el resultado puede ser sobre-inclusivo."],
  guiaBulletFA["Si comparas con un subespacio top, activa Top-sector antes del calculo."],
  guiaBulletFA["Si usas SMEFTsim completo, pueden aparecer operadores bosonicos/leptonicos que no pertenecen al subespacio top seleccionado."],
  guiaBulletFA["Si quieres una demostracion fisica completa, usa Fisico estricto y revisa que no aparezca NoConclusivo."]
  }, True],
  guiaOpenFA["6. |M|^2 fisica no concluyente o timeout", {
  guiaBulletFA["Esto no significa que los WCs sean cero; significa que FeynCalc no ha cerrado la reduccion en el tiempo dado."],
  guiaBulletFA["Baja a Estructural para obtener candidatos o aumenta Timeout |M|^2 para un canal pequeno."],
  guiaBulletFA[Row[{"Revisa ", guiaCodeFA["$lastM2PhysicalUndecided"], " y ", guiaCodeFA["$lastM2PhysicalFailed"], "."}]],
  guiaBulletFA["Evita Fisico en p p expandido, loops y 2 -> 3 grandes salvo pruebas controladas."]
  }, False],
  guiaOpenFA["7. La interfaz se queda congelada", {
  guiaBulletFA["Mathematica no refresca el FrontEnd durante llamadas largas del kernel. La barra se actualiza antes/despues de CreateFeynAmp, FCFAConvert y simplificaciones, no dentro."],
  guiaBulletFA["Usa Resetear/Limpiar todo para intentar cancelar. Si el kernel esta dentro de una llamada no interrumpible, puede tardar."],
  guiaBulletFA["Para recuperar memoria, reinicia el kernel si has lanzado varios calculos pesados."]
  }, False],
  guiaOpenFA["8. Checklist recomendado antes de dar por bueno un resultado", {
  guiaBulletFA["El Status debe indicar Calculo completado."],
  guiaBulletFA["Revisa Sector de WCs activo."],
  guiaBulletFA["Revisa Modo real de |M|^2 usado."],
  guiaBulletFA["Si hay WCs no concluyentes, no compares como resultado fisico cerrado."],
  guiaBulletFA["Traduce lastWCNames y comprueba operadores/notacion."]
  }, True],
  DefaultButton["Cerrar", DialogReturn[]]
  }, Spacings -> 0.85],
  {900, 650},
  Scrollbars -> True
  ]
  ],
  WindowTitle -> "Guia de errores"
];


WilsonUIManualCatalog[]
