(* ::Package:: *)
(* SMEFTsim_WC_traducciones_SMEFTNLO_BIBLIO_REVISADA.wl
   Tabla auxiliar de traduccion de coeficientes de Wilson entre:
   - SMEFTsim top / topU3l / general
   - SMEFT@NLO
   - dim6top

   Archivo ASCII para evitar problemas de codificacion en Windows.
   No modifica ninguna interfaz. Solo define datos y funciones auxiliares.
   Revision: tablas y exportaciones simplificadas; se ocultan Class, Translation type/Kind y Notes.
*)

ClearAll[
  $SMEFTsimWCTranslations,
  addTranslationRow,
  wcToString,
  wcTranslations,
  wcTranslationsDataset,
  translateFromSMEFTsim,
  translateWCList,
  translateToTarget,
  untranslatedWCs,
  wcCoverageReport,
  exportWCTranslationsCSV,
  exportWCTranslationsTeX,
  translateLastWCResult,
  allSMEFTsimKnownWCs,
  allTranslationTargets
];

$SMEFTsimWCTranslations = {};

addTranslationRow[model_, smeftsim_, target_, targetExpr_, class_, kind_, notes_, aliases_:{}] :=
  AppendTo[$SMEFTsimWCTranslations,
    <|
      "SMEFTsimModel" -> ToString[model],
      "SMEFTsim" -> ToString[smeftsim],
      "Target" -> ToString[target],
      "TargetExpression" -> ToString[targetExpr],
      "Class" -> ToString[class],
      "Kind" -> ToString[kind],
      "Notes" -> ToString[notes],
      "Aliases" -> (ToString /@ Flatten[{aliases}])
    |>
  ];

SetAttributes[wcToString, HoldFirst];
wcToString[x_String] := StringTrim[x];
wcToString[x_Symbol] := SymbolName[Unevaluated[x]];
wcToString[x_] := StringTrim[StringReplace[ToString[Unevaluated[x], InputForm], "\"" -> ""]];

(* ========================================================= *)
(* 1. SMEFTsim top/topU3l -> SMEFT@NLO                      *)
(* ========================================================= *)

(* One-to-one or sign-normalized entries from SMEFTsim guide tables. *)
addTranslationRow["top/topU3l", "cG", "SMEFT@NLO", "-gs*cG", "L1", "one-to-one-with-sign", "Overall sign/coupling convention from UFO comparison."];
addTranslationRow["top/topU3l", "cW", "SMEFT@NLO", "-cWWW", "L1", "one-to-one-with-sign", "Triple-W operator."];
addTranslationRow["top/topU3l", "cH", "SMEFT@NLO", "cp", "L2", "one-to-one", "Higgs potential operator."];
addTranslationRow["top/topU3l", "cHbox", "SMEFT@NLO", "cdp", "L3", "one-to-one", "Higgs derivative box operator."];
addTranslationRow["top/topU3l", "cHDD", "SMEFT@NLO", "cpDC", "L3", "one-to-one", "Higgs derivative operator."];
addTranslationRow["top/topU3l", "cHG", "SMEFT@NLO", "cpG", "L4", "one-to-one", "Higgs-gluon operator."];
addTranslationRow["top/topU3l", "cHW", "SMEFT@NLO", "cpW", "L4", "one-to-one", "Higgs-W operator."];
addTranslationRow["top/topU3l", "cHB", "SMEFT@NLO", "cpBB", "L4", "one-to-one", "Higgs-B operator."];
addTranslationRow["top/topU3l", "cHWB", "SMEFT@NLO", "cpWB", "L4", "one-to-one", "Higgs-WB operator."];
addTranslationRow["top/topU3l", "ctHRe", "SMEFT@NLO", "ctp", "L5", "one-to-one", "Top Yukawa.", {"cuHRe33"}];
addTranslationRow["top/topU3l", "ctGRe", "SMEFT@NLO", "-gs*ctG", "L6", "one-to-one-with-sign", "Top chromomagnetic dipole.", {"cuGRe33"}];
addTranslationRow["top/topU3l", "ctWRe", "SMEFT@NLO", "-ctW", "L6", "basis-sign", "SMEFT@NLO uses ctW. UFO comparison: -ctWRe -> ctW.", {"cuWRe33"}];
addTranslationRow["top/topU3l", "-ctWRe*cTheta + ctBRe*sTheta", "SMEFT@NLO", "ctZ", "L6", "basis-rotation", "Neutral top dipole in SMEFT@NLO."];
addTranslationRow["top/topU3l", "ctBRe", "SMEFT@NLO", "ctZ/sTheta - ctW/tTheta", "L6", "inverse-basis-rotation", "Inverse relation from SMEFT@NLO variables. Not an independent one-to-one mapping.", {"cuBRe33"}];

addTranslationRow["top/topU3l", "cHQ3", "SMEFT@NLO", "cpQ3", "L7", "basis-rotation", "Heavy LH doublet triplet.", {"cHq3Re33"}];
addTranslationRow["top/topU3l", "cHQ1 - cHQ3", "SMEFT@NLO", "cpQM", "L7", "basis-rotation", "Heavy LH doublet minus combination."];
addTranslationRow["top/topU3l", "cHQ1", "SMEFT@NLO", "cpQ3 + cpQM", "L7", "inverse-basis-rotation", "Since cpQ3=cHQ3 and cpQM=cHQ1-cHQ3.", {"cHq1Re33"}];
addTranslationRow["top/topU3l", "cHj3", "SMEFT@NLO", "cpq3i", "L7", "basis-rotation", "Light-quark LH doublet triplet current."];
addTranslationRow["top/topU3l", "cHj1 - cHj3", "SMEFT@NLO", "cpqMi", "L7", "basis-rotation", "Light-quark LH doublet minus combination."];
addTranslationRow["top/topU3l", "cHj1", "SMEFT@NLO", "cpq3i + cpqMi", "L7", "inverse-basis-rotation", "Since cpq3i=cHj3 and cpqMi=cHj1-cHj3."];
addTranslationRow["top/topU3l", "cHt", "SMEFT@NLO", "cpt", "L7", "one-to-one", "Right-handed top neutral current.", {"cHuRe33"}];
addTranslationRow["top/topU3l", "cHu", "SMEFT@NLO", "cpu", "L7", "one-to-one", "Light right-handed up neutral current.", {"cHuRe[rr]"}];
addTranslationRow["top/topU3l", "cHbq", "SMEFT@NLO", "cpd", "L7", "one-to-one", "Bottom/down right-handed neutral-current entry in SMEFT@NLO top mapping.", {"cHd", "cHdRe33"}];
addTranslationRow["top/topU3l", "cHe", "SMEFT@NLO", "cpe = cpmu = cpta", "L7", "topU3l-one-to-many", "topU3l has U(3) lepton symmetry for this coefficient."];
addTranslationRow["top", "cHe11", "SMEFT@NLO", "cpe", "L7", "one-to-one", "Electron right-handed current."];
addTranslationRow["top", "cHe22", "SMEFT@NLO", "cpmu", "L7", "one-to-one", "Muon right-handed current."];
addTranslationRow["top", "cHe33", "SMEFT@NLO", "cpta", "L7", "one-to-one", "Tau right-handed current."];
addTranslationRow["topU3l", "cHl1", "SMEFT@NLO", "cpl[p]", "L7", "one-to-many", "Lepton-flavor-universal topU3l coefficient maps to cpl[p]."];
addTranslationRow["topU3l", "cHl3", "SMEFT@NLO", "c3pl[p]", "L7", "one-to-many", "Lepton-flavor-universal topU3l coefficient maps to c3pl[p]."];
addTranslationRow["top", "cHl1[pp]", "SMEFT@NLO", "cpl[p]", "L7", "indexed", "Lepton flavor index p=1,2,3."];
addTranslationRow["top", "cHl3[pp]", "SMEFT@NLO", "c3pl[p]", "L7", "indexed", "Lepton flavor index p=1,2,3."];

(* Four-lepton / lepton-heavy entries *)
addTranslationRow["topU3l", "cll", "SMEFT@NLO", "cll[pppp] = cll[pprr]", "L8a", "topU3l-one-to-many", "topU3l lepton-universal four-lepton coefficient."];
addTranslationRow["topU3l", "cll1", "SMEFT@NLO", "cll[pppp] = cll[prrp]", "L8a", "topU3l-one-to-many", "Second four-lepton contraction."];
addTranslationRow["topU3l", "cte", "SMEFT@NLO", "cte[p]", "L8b", "one-to-many", "Lepton index p.", {"cte[pp]"}];
addTranslationRow["topU3l", "cQe", "SMEFT@NLO", "cQe[p]", "L8c", "one-to-many", "Lepton index p.", {"cQe[pp]"}];
addTranslationRow["topU3l", "ctl", "SMEFT@NLO", "ctl[p]", "L8c", "one-to-many", "Lepton index p.", {"ctl[pp]"}];
addTranslationRow["topU3l", "cQl3", "SMEFT@NLO", "cQl3[p]", "L8a", "basis-rotation", "topU3l leptonic triplet coefficient.", {"cQl3[pp]"}];
addTranslationRow["topU3l", "cQl1 - cQl3", "SMEFT@NLO", "cQlM[p]", "L8a", "basis-rotation", "topU3l leptonic minus combination."];
addTranslationRow["topU3l", "cQl1", "SMEFT@NLO", "cQl3[p] + cQlM[p]", "L8a", "inverse-basis-rotation", "Since cQlM[p]=cQl1-cQl3.", {"cQl1[pp]"}];

(* Four-quark one-to-one entries to SMEFT@NLO top names *)
addTranslationRow["top/topU3l", "cQj11", "SMEFT@NLO", "cQq11", "L8a", "one-to-one", "Heavy-light LH four-quark singlet/singlet."];
addTranslationRow["top/topU3l", "cQj18", "SMEFT@NLO", "cQq81", "L8a", "one-to-one", "Heavy-light LH four-quark singlet/octet."];
addTranslationRow["top/topU3l", "cQj31", "SMEFT@NLO", "cQq13", "L8a", "one-to-one", "Heavy-light LH four-quark triplet/singlet."];
addTranslationRow["top/topU3l", "cQj38", "SMEFT@NLO", "cQq83", "L8a", "one-to-one", "Heavy-light LH four-quark triplet/octet."];
addTranslationRow["top/topU3l", "cQQ1", "SMEFT@NLO", "1/2*cQQ1", "L8a", "normalization", "SMEFT@NLO table has factor 1/2."];
addTranslationRow["top/topU3l", "cQQ8", "SMEFT@NLO", "1/2*cQQ8", "L8a", "normalization", "SMEFT@NLO table has factor 1/2."];
addTranslationRow["top/topU3l", "ctu1", "SMEFT@NLO", "ctu1", "L8b", "one-to-one", "Right-handed top/up."];
addTranslationRow["top/topU3l", "ctu8", "SMEFT@NLO", "ctu8", "L8b", "one-to-one", "Right-handed top/up octet."];
addTranslationRow["top/topU3l", "ctd1", "SMEFT@NLO", "ctd1", "L8b", "one-to-one", "Right-handed top/down."];
addTranslationRow["top/topU3l", "ctd8", "SMEFT@NLO", "ctd8", "L8b", "one-to-one", "Right-handed top/down octet."];
addTranslationRow["top/topU3l", "ctt", "SMEFT@NLO", "ctt1", "L8b", "one-to-one", "Four-top operator."];
addTranslationRow["top/topU3l", "cQt1", "SMEFT@NLO", "cQt1", "L8c", "one-to-one", "Heavy-doublet/right-top."];
addTranslationRow["top/topU3l", "cQt8", "SMEFT@NLO", "cQt8", "L8c", "one-to-one", "Heavy-doublet/right-top octet."];
addTranslationRow["top/topU3l", "cQu1", "SMEFT@NLO", "cQu1", "L8c", "one-to-one", "Heavy-doublet/right-up."];
addTranslationRow["top/topU3l", "cQu8", "SMEFT@NLO", "cQu8", "L8c", "one-to-one", "Heavy-doublet/right-up octet."];
addTranslationRow["top/topU3l", "cQd1", "SMEFT@NLO", "cQd1", "L8c", "one-to-one", "Heavy-doublet/right-down."];
addTranslationRow["top/topU3l", "cQd8", "SMEFT@NLO", "cQd8", "L8c", "one-to-one", "Heavy-doublet/right-down octet."];
addTranslationRow["top/topU3l", "ctj1", "SMEFT@NLO", "ctq1", "L8c", "one-to-one", "Right-top/light-doublet."];
addTranslationRow["top/topU3l", "ctj8", "SMEFT@NLO", "ctq8", "L8c", "one-to-one", "Right-top/light-doublet octet."];

(* Scalar/tensor entries to SMEFT@NLO *)
addTranslationRow["top/topU3l", "cleQt1Re33", "SMEFT@NLO", "yl[3]*ctlS3", "L8d", "yukawa-normalized", "SMEFT@NLO includes an explicit tau Yukawa factor in topU3l table.", {"clequ1Re3333"}];
addTranslationRow["top/topU3l", "cleQt3Re33", "SMEFT@NLO", "yl[3]*ctlT3", "L8d", "yukawa-normalized", "SMEFT@NLO includes an explicit tau Yukawa factor in topU3l table.", {"clequ3Re3333"}];
addTranslationRow["top/topU3l", "clebQRe33", "SMEFT@NLO", "yl[3]*cblS3", "L8d", "yukawa-normalized", "SMEFT@NLO includes an explicit tau Yukawa factor in topU3l table.", {"cledqRe3333"}];

(* ========================================================= *)
(* 2. SMEFTsim top/topU3l -> dim6top                        *)
(* ========================================================= *)

addTranslationRow["top/topU3l", "ctHRe", "dim6top", "ctp", "L5", "one-to-one", "Top Yukawa.", {"cuHRe33"}];
addTranslationRow["top/topU3l", "ctHIm", "dim6top", "ctpI", "L5", "CPV-one-to-one", "Imaginary top Yukawa.", {"cuHIm33"}];
addTranslationRow["top/topU3l", "ctGRe", "dim6top", "-ctG", "L6", "one-to-one-with-sign", "Sign difference due to covariant derivative convention.", {"cuGRe33"}];
addTranslationRow["top/topU3l", "ctGIm", "dim6top", "-ctGI", "L6", "CPV-one-to-one-with-sign", "Sign difference due to covariant derivative convention.", {"cuGIm33"}];
addTranslationRow["top/topU3l", "ctWRe", "dim6top", "-ctW", "L6", "basis-sign", "dim6top table: -ctWRe -> ctW.", {"cuWRe33"}];
addTranslationRow["top/topU3l", "ctWIm", "dim6top", "-ctWI", "L6", "CPV-basis-sign", "dim6top table: -ctWIm -> ctWI.", {"cuWIm33"}];
addTranslationRow["top/topU3l", "-ctWRe*cTheta + ctBRe*sTheta", "dim6top", "ctZ", "L6", "basis-rotation", "Neutral top dipole in dim6top."];
addTranslationRow["top/topU3l", "-ctWIm*cTheta + ctBIm*sTheta", "dim6top", "ctZI", "L6", "CPV-basis-rotation", "CPV neutral top dipole in dim6top."];
addTranslationRow["top/topU3l", "ctBRe", "dim6top", "ctZ/sTheta - ctW/tTheta", "L6", "inverse-basis-rotation", "Inverse relation; ctBRe is not an independent dim6top coefficient.", {"cuBRe33"}];
addTranslationRow["top/topU3l", "ctBIm", "dim6top", "ctZI/sTheta - ctWI/tTheta", "L6", "inverse-CPV-basis-rotation", "Inverse relation; ctBIm is not an independent dim6top coefficient.", {"cuBIm33"}];
addTranslationRow["top/topU3l", "cHt", "dim6top", "cpt", "L7", "one-to-one", "Right-handed top neutral current.", {"cHuRe33"}];
addTranslationRow["top/topU3l", "cHbq", "dim6top", "cpb", "L7", "one-to-one", "Right-handed bottom neutral current.", {"cHdRe33"}];
addTranslationRow["top/topU3l", "cHQ3", "dim6top", "cpQ3", "L7", "basis-rotation", "Heavy LH doublet triplet.", {"cHq3Re33"}];
addTranslationRow["top/topU3l", "cHQ1 - cHQ3", "dim6top", "cpQM", "L7", "basis-rotation", "Heavy LH doublet minus combination."];
addTranslationRow["top/topU3l", "cHQ1", "dim6top", "cpQ3 + cpQM", "L7", "inverse-basis-rotation", "Since cpQ3=cHQ3 and cpQM=cHQ1-cHQ3.", {"cHq1Re33"}];
addTranslationRow["top/topU3l", "cHtbRe", "dim6top", "cptb", "L7", "one-to-one", "Right-handed Htb current.", {"cHudRe33"}];
addTranslationRow["top/topU3l", "cHtbIm", "dim6top", "cptbI", "L7", "CPV-one-to-one", "Imaginary Htb current.", {"cHudIm33"}];

(* cHj1/cHj3 are light-quark EW-current coefficients. dim6top focuses on top interactions; no direct standard dim6top top-sector parameter is listed for these. *)
addTranslationRow["top/topU3l", "cHj3", "dim6top", "No direct standard dim6top parameter", "L7", "not-mapped", "Light-quark EW-current coefficient; translate to SMEFT@NLO cpq3i if needed."];
addTranslationRow["top/topU3l", "cHj1", "dim6top", "No direct standard dim6top parameter", "L7", "not-mapped", "Light-quark EW-current coefficient; translate to SMEFT@NLO cpq3i+cpqMi if needed."];

(* dim6top two-lepton-two-heavy-quark rotations *)
addTranslationRow["topU3l", "cQl3", "dim6top", "cQl3[p]", "L8a", "basis-rotation", "Leptonic triplet coefficient; lepton index p in dim6top.", {"cQl3[pp]"}];
addTranslationRow["topU3l", "cQl1 - cQl3", "dim6top", "cQlM[p]", "L8a", "basis-rotation", "Leptonic minus combination."];
addTranslationRow["topU3l", "cQl1", "dim6top", "cQl3[p] + cQlM[p]", "L8a", "inverse-basis-rotation", "Since cQlM[p]=cQl1-cQl3.", {"cQl1[pp]"}];
addTranslationRow["topU3l", "cte", "dim6top", "cte[p]", "L8b", "one-to-many", "Lepton index p.", {"cte[pp]"}];
addTranslationRow["topU3l", "cQe", "dim6top", "cQe[p]", "L8c", "one-to-many", "Lepton index p.", {"cQe[pp]"}];
addTranslationRow["topU3l", "ctl", "dim6top", "ctl[p]", "L8c", "one-to-many", "Lepton index p.", {"ctl[pp]"}];
addTranslationRow["topU3l", "cbl", "dim6top", "cbl[p]", "L8c", "one-to-many", "Lepton index p.", {"cbl[pp]"}];
addTranslationRow["topU3l", "cbe", "dim6top", "cbe[p]", "L8b", "one-to-many", "Lepton index p.", {"cbe[pp]"}];

(* Four-quark one-to-one dim6top entries *)
addTranslationRow["top/topU3l", "ctu1", "dim6top", "ctu1", "L8b", "one-to-one", "Right-handed top/up."];
addTranslationRow["top/topU3l", "ctu8", "dim6top", "ctu8", "L8b", "one-to-one", "Right-handed top/up octet."];
addTranslationRow["top/topU3l", "ctd1", "dim6top", "ctd1", "L8b", "one-to-one", "Right-handed top/down."];
addTranslationRow["top/topU3l", "ctd8", "dim6top", "ctd8", "L8b", "one-to-one", "Right-handed top/down octet."];
addTranslationRow["top/topU3l", "ctb1", "dim6top", "ctb1", "L8b", "one-to-one", "Right-handed top/bottom."];
addTranslationRow["top/topU3l", "ctb8", "dim6top", "ctb8", "L8b", "one-to-one", "Right-handed top/bottom octet."];
addTranslationRow["top/topU3l", "ctj1", "dim6top", "ctq1", "L8c", "one-to-one", "Right-handed top/light doublet."];
addTranslationRow["top/topU3l", "ctj8", "dim6top", "ctq8", "L8c", "one-to-one", "Right-handed top/light doublet octet."];
addTranslationRow["top/topU3l", "cQu1", "dim6top", "cQu1", "L8c", "one-to-one", "Heavy doublet/right-up."];
addTranslationRow["top/topU3l", "cQu8", "dim6top", "cQu8", "L8c", "one-to-one", "Heavy doublet/right-up octet."];
addTranslationRow["top/topU3l", "cQd1", "dim6top", "cQd1", "L8c", "one-to-one", "Heavy doublet/right-down."];
addTranslationRow["top/topU3l", "cQd8", "dim6top", "cQd8", "L8c", "one-to-one", "Heavy doublet/right-down octet."];
addTranslationRow["top/topU3l", "cQb1", "dim6top", "cQb1", "L8c", "one-to-one", "Heavy doublet/right-bottom."];
addTranslationRow["top/topU3l", "cQb8", "dim6top", "cQb8", "L8c", "one-to-one", "Heavy doublet/right-bottom octet."];
addTranslationRow["top/topU3l", "cQt1", "dim6top", "cQt1", "L8c", "one-to-one", "Heavy doublet/right-top."];
addTranslationRow["top/topU3l", "cQt8", "dim6top", "cQt8", "L8c", "one-to-one", "Heavy doublet/right-top octet."];
addTranslationRow["top/topU3l", "ctt", "dim6top", "ctt1", "L8b", "one-to-one", "Four-top operator."];

(* Scalar/tensor dim6top entries *)
addTranslationRow["top/topU3l", "cleQt1Re33", "dim6top", "ctlS3", "L8d", "one-to-one", "Scalar semileptonic operator.", {"clequ1Re3333"}];
addTranslationRow["top/topU3l", "cleQt3Re33", "dim6top", "ctlT3", "L8d", "one-to-one", "Tensor semileptonic operator.", {"clequ3Re3333"}];
addTranslationRow["top/topU3l", "clebQRe33", "dim6top", "cblS3", "L8d", "one-to-one", "Scalar bottom semileptonic operator.", {"cledqRe3333"}];
addTranslationRow["top/topU3l", "cleQt1Im33", "dim6top", "ctlSI3", "L8d", "CPV-one-to-one", "Imaginary scalar semileptonic operator.", {"clequ1Im3333"}];
addTranslationRow["top/topU3l", "cleQt3Im33", "dim6top", "ctlTI3", "L8d", "CPV-one-to-one", "Imaginary tensor semileptonic operator.", {"clequ3Im3333"}];
addTranslationRow["top/topU3l", "clebQIm33", "dim6top", "cblSI3", "L8d", "CPV-one-to-one", "Imaginary scalar bottom semileptonic operator.", {"cledqIm3333"}];

(* Complicated Fierz/basis rotations from dim6top table 25. Kept as explicit formulas. *)
addTranslationRow["top", "1/3*cutbd1Re + 4/9*cutbd8Re*yu[r]*yd[s]", "dim6top", "cbtud1", "L8b", "basis-rotation", "Fierz/basis rotation involving cutbd1Re/cutbd8Re and Yukawas."];
addTranslationRow["top", "2*cutbd1Re - 1/3*cutbd8Re*yu[r]*yd[s]", "dim6top", "cbtud8", "L8b", "basis-rotation", "Fierz/basis rotation involving cutbd1Re/cutbd8Re and Yukawas."];
addTranslationRow["top", "-(2/3)*cjQtu1Re - (8/9)*cjQtu8Re*yu[r]", "dim6top", "ctQqu1", "L8c", "basis-rotation", "Fierz/basis rotation involving cjQtu coefficients."];
addTranslationRow["top", "-4*cjQtu1Re + (2/3)*cjQtu8Re*yu[r]", "dim6top", "ctQqu8", "L8c", "basis-rotation", "Fierz/basis rotation involving cjQtu coefficients."];
addTranslationRow["top", "-(2/3)*cjQbd1Re - (8/9)*cjQbd8Re*yd[r]", "dim6top", "cbQqd1", "L8c", "basis-rotation", "Fierz/basis rotation involving cjQbd coefficients."];

(* ========================================================= *)
(* 4. SMEFTsim general -> SMEFT@NLO / dim6top common entries *)
(* ========================================================= *)

addTranslationRow["general", "cuHRe33", "SMEFT@NLO", "ctp", "L5", "one-to-one", "General SMEFTsim top Yukawa coefficient."];
addTranslationRow["general", "cuGRe33", "SMEFT@NLO", "-gs*ctG", "L6", "one-to-one-with-sign", "General SMEFTsim top chromomagnetic coefficient."];
addTranslationRow["general", "cuWRe33", "SMEFT@NLO", "-ctW", "L6", "basis-sign", "General SMEFTsim top SU(2) dipole."];
addTranslationRow["general", "-cuWRe33*cTheta + cuBRe33*sTheta", "SMEFT@NLO", "ctZ", "L6", "basis-rotation", "General SMEFTsim neutral top dipole."];
addTranslationRow["general", "cHq3Re33", "SMEFT@NLO", "cpQ3", "L7", "basis-rotation", "General SMEFTsim heavy doublet triplet."];
addTranslationRow["general", "cHq1Re33 - cHq3Re33", "SMEFT@NLO", "cpQM", "L7", "basis-rotation", "General SMEFTsim heavy doublet minus combination."];
addTranslationRow["general", "cHq1Re33", "SMEFT@NLO", "cpQ3 + cpQM", "L7", "inverse-basis-rotation", "General SMEFTsim cHq1Re33."];
addTranslationRow["general", "cHq3Re[rr]", "SMEFT@NLO", "cpq3i", "L7", "basis-rotation-light", "General SMEFTsim light doublet triplet, r=1,2."];
addTranslationRow["general", "cHq1Re[rr] - cHq3Re[rr]", "SMEFT@NLO", "cpqMi", "L7", "basis-rotation-light", "General SMEFTsim light doublet minus combination, r=1,2."];

addTranslationRow["general", "cuHRe33", "dim6top", "ctp", "L5", "one-to-one", "General SMEFTsim top Yukawa coefficient."];
addTranslationRow["general", "cuHIm33", "dim6top", "ctpI", "L5", "CPV-one-to-one", "General SMEFTsim imaginary top Yukawa coefficient."];
addTranslationRow["general", "cuGRe33", "dim6top", "-ctG", "L6", "one-to-one-with-sign", "General SMEFTsim top chromomagnetic coefficient."];
addTranslationRow["general", "cuGIm33", "dim6top", "-ctGI", "L6", "CPV-one-to-one-with-sign", "General SMEFTsim imaginary top chromomagnetic coefficient."];
addTranslationRow["general", "cHq3Re33", "dim6top", "cpQ3", "L7", "basis-rotation", "General SMEFTsim heavy doublet triplet."];
addTranslationRow["general", "cHq1Re33 - cHq3Re33", "dim6top", "cpQM", "L7", "basis-rotation", "General SMEFTsim heavy doublet minus combination."];
addTranslationRow["general", "cHq1Re33", "dim6top", "cpQ3 + cpQM", "L7", "inverse-basis-rotation", "General SMEFTsim cHq1Re33."];

(* ========================================================= *)
(* 5. Helper functions                                        *)
(* ========================================================= *)

wcTranslations[] := $SMEFTsimWCTranslations;

wcTranslationsDataset[] := Dataset[$SMEFTsimWCTranslations][
  All,
  {"SMEFTsimModel", "SMEFTsim", "Target", "TargetExpression", "Aliases"}
];

allSMEFTsimKnownWCs[] := Sort @ DeleteDuplicates @ Flatten[
  Join[
    Lookup[$SMEFTsimWCTranslations, "SMEFTsim"],
    Lookup[$SMEFTsimWCTranslations, "Aliases"]
  ]
];

allTranslationTargets[] := Sort @ DeleteDuplicates @ Lookup[$SMEFTsimWCTranslations, "Target"];

translateFromSMEFTsim[wc_, target_:All] := Module[{q, rows},
  q = wcToString[wc];
  rows = Select[$SMEFTsimWCTranslations,
    (#["SMEFTsim"] === q || MemberQ[#["Aliases"], q]) &&
      (target === All || #["Target"] === ToString[target]) &
  ];
  If[rows === {},
    {<|
      "SMEFTsim" -> q,
      "Target" -> If[target === All, "All", ToString[target]],
      "Status" -> "Not found",
      "Hint" -> "Check spelling, model flavor assumption, or whether this WC is only present inside a basis rotation."
    |>} ,
    rows
  ]
];

translateWCList[wcs_List, target_:All] := Module[{names, rows},
  names = wcToString /@ wcs;
  rows = Flatten[translateFromSMEFTsim[#, target] & /@ names];
  Dataset[rows]
];

translateToTarget[target_] := Dataset @ Select[$SMEFTsimWCTranslations, #["Target"] === ToString[target] &];

untranslatedWCs[wcs_List, target_:All] := Module[{names, isMissing},
  names = DeleteDuplicates[wcToString /@ wcs];
  isMissing[q_] := Module[{rows},
    rows = translateFromSMEFTsim[q, target];
    Length[rows] == 1 && KeyExistsQ[First[rows], "Status"] && First[rows]["Status"] === "Not found"
  ];
  Select[names, isMissing]
];

wcCoverageReport[wcs_List, target_:All] := Module[{names, missing, translated},
  names = DeleteDuplicates[wcToString /@ wcs];
  missing = untranslatedWCs[names, target];
  translated = Complement[names, missing];
  <|
    "Target" -> If[target === All, "All", ToString[target]],
    "TotalInput" -> Length[names],
    "TranslatedCount" -> Length[translated],
    "MissingCount" -> Length[missing],
    "Translated" -> translated,
    "Missing" -> missing
  |>
];

exportWCTranslationsCSV[path_:Automatic] := Module[{out},
  out = If[path === Automatic,
    FileNameJoin[{NotebookDirectory[], "SMEFTsim_WC_traducciones_UFO_COMPLETO.csv"}],
    path
  ];
  Export[out,
    Normal[$SMEFTsimWCTranslations] /. a_Association :> KeyDrop[a, {"Aliases", "Class", "Kind", "Notes"}],
    "CSV"
  ];
  out
];

(* ========================================================= *)
(* 5.1 Export of translation tables to a standalone .tex     *)
(*     This helper does not require the Wilson-coefficient UI. *)
(* ========================================================= *)

ClearAll[
  exportWCTranslationsTeX,
  smeftsimTexDefaultDirectoryFA,
  smeftsimTexEscapeTextFA,
  smeftsimTexMathCellFA,
  smeftsimTexMathListCellFA,
  smeftsimTexTableRowFA,
  smeftsimTexColumnSpecFA,
  smeftsimTexLongTableFA,
  smeftsimTexInputNamesFA,
  smeftsimTexGoodRowsFA,
  smeftsimTexTargetCellFA,
  smeftsimTexTargetIsTextQFA,
  smeftsimTexSummaryRowsFA,
  smeftsimTexDetailedRowsFA,
  smeftsimTexDocumentFA
];

Options[exportWCTranslationsTeX] = {
  "Target" -> All,
  "TableType" -> "Summary",
  "Title" -> "SMEFTsim Wilson-coefficient translations",
  "IncludePreamble" -> True
};

exportWCTranslationsTeX::badtype =
  "Unknown TableType `1`. Use \"Summary\", \"Detailed\" or \"Both\".";

smeftsimTexDefaultDirectoryFA[] := Module[{nbDir, sourceDir},
  nbDir = Quiet @ Check[NotebookDirectory[], $Failed];
  sourceDir = Quiet @ Check[DirectoryName[$InputFileName], $Failed];

  Which[
    StringQ[nbDir] && DirectoryQ[nbDir], nbDir,
    StringQ[sourceDir] && sourceDir =!= "" && DirectoryQ[sourceDir], sourceDir,
    True, Directory[]
  ]
];

(* Escape ordinary text fields only. Mathematical entries are already LaTeX. *)
smeftsimTexEscapeTextFA[value_] := Module[{s},
  s = StringTrim @ StringReplace[ToString[value], WhitespaceCharacter .. -> " "];
  StringReplace[s, {
    "\\" -> "\\textbackslash{}",
    "&" -> "\\&",
    "%" -> "\\%",
    "$" -> "\\$",
    "#" -> "\\#",
    "_" -> "\\_",
    "{" -> "\\{",
    "}" -> "\\}",
    "~" -> "\\textasciitilde{}",
    "^" -> "\\textasciicircum{}"
  }]
];

smeftsimTexMathCellFA[value_] := Module[{s},
  s = StringTrim @ ToString[value];
  If[s === "" || s === "-" || s === "Not found",
    "--",
    "\\(\\displaystyle " <> s <> "\\)"
  ]
];

smeftsimTexMathListCellFA[values_List] := Module[{clean},
  clean = DeleteDuplicates @ Select[
    StringTrim @ ToString[#] & /@ values,
    # =!= "" && # =!= "-" && # =!= "Not found" &
  ];

  If[clean === {},
    "--",
    StringRiffle[smeftsimTexMathCellFA /@ clean, "\\par "]
  ]
];

smeftsimTexTableRowFA[cells_List] :=
  StringRiffle[cells, " & "] <> " \\\\";

smeftsimTexColumnSpecFA["Summary", nTargets_Integer?NonNegative] :=
  StringRiffle[
    Join[
      {"L{2.3cm}", "L{5.0cm}", "L{0.8cm}", "L{1.7cm}"},
      ConstantArray["L{3.4cm}", nTargets]
    ],
    " "
  ];

smeftsimTexColumnSpecFA["Detailed", _Integer?NonNegative] :=
  StringRiffle[
    {
      "L{2.4cm}", "L{5.2cm}", "L{2.2cm}", "L{6.5cm}"
    },
    " "
  ];

smeftsimTexLongTableFA[headers_List, rows_List, caption_, label_, columnSpec_] :=
 Module[{headerLine, nColumns, firstHead, laterHead, foot, endFoot},

  nColumns = Length[headers];
  headerLine = smeftsimTexTableRowFA[
    ("\\textbf{" <> smeftsimTexEscapeTextFA[#] <> "}" &) /@ headers
  ];

  firstHead = {
    "\\caption{" <> smeftsimTexEscapeTextFA[caption] <> "}\\label{" <> label <> "}\\\\",
    "\\toprule",
    headerLine,
    "\\midrule",
    "\\endfirsthead"
  };

  laterHead = {
    "\\multicolumn{" <> ToString[nColumns] <>
      "}{l}{\\small\\itshape Continuation of the previous page}\\\\",
    "\\toprule",
    headerLine,
    "\\midrule",
    "\\endhead"
  };

  foot = {
    "\\midrule",
    "\\multicolumn{" <> ToString[nColumns] <>
      "}{r}{\\small\\itshape Continued on the next page}\\\\",
    "\\endfoot"
  };

  endFoot = {
    "\\bottomrule",
    "\\endlastfoot"
  };

  StringRiffle[
    Join[
      {"\\begin{longtable}{" <> columnSpec <> "}"},
      firstHead,
      laterHead,
      foot,
      endFoot,
      rows,
      {"\\end{longtable}"}
    ],
    "\n"
  ]
];

smeftsimTexInputNamesFA[wcs_] := If[wcs === All,
  DeleteDuplicates @ Lookup[$SMEFTsimWCTranslations, "SMEFTsim", {}],
  DeleteDuplicates @ (wcToString /@ Flatten[{wcs}])
];

smeftsimTexGoodRowsFA[wc_, target_:All] := Select[
  If[target === All,
    translateFromSMEFTsim[wc],
    translateFromSMEFTsim[wc, target]
  ],
  ! KeyExistsQ[#, "Status"] &
];

smeftsimTexTargetIsTextQFA[row_Association] :=
  StringContainsQ[Lookup[row, "Kind", ""], "not-mapped", IgnoreCase -> True];

smeftsimTexTargetCellFA[wc_, target_String] := Module[{rows, latex, textValues},
  rows = smeftsimTexGoodRowsFA[wc, target];

  If[rows === {}, Return["--"]];

  If[AllTrue[rows, smeftsimTexTargetIsTextQFA],
    textValues = DeleteDuplicates @ Lookup[rows, "TargetExpression", "--"];
    Return[smeftsimTexEscapeTextFA @ StringRiffle[textValues, " | "]]
  ];

  latex = Lookup[addLatexColumnsFA /@ rows, "TargetLaTeX", ""];
  smeftsimTexMathListCellFA[latex]
];

smeftsimTexSummaryRowsFA[names_List, targets_List] := Table[
  Module[{goodRows, full, cells},

    goodRows = smeftsimTexGoodRowsFA[name];

    If[goodRows === {},
      cells = Join[
        {
          smeftsimTexMathCellFA[latexExprFA[name]],
          "--",
          "--",
          "--"
        },
        ConstantArray["--", Length[targets]]
      ],

      full = addFullInfoColumnsFA[First[goodRows]];

      cells = Join[
        {
          smeftsimTexMathCellFA[Lookup[full, "SMEFTsimLaTeX", latexExprFA[name]]],
          smeftsimTexMathCellFA[Lookup[full, "OperatorLaTeX", "-"]],
          smeftsimTexEscapeTextFA[Lookup[full, "OperatorDimension", "-"]],
          smeftsimTexMathCellFA[Lookup[full, "LambdaOrder", "-"]]
        },
        (smeftsimTexTargetCellFA[name, #] &) /@ targets
      ]
    ];

    smeftsimTexTableRowFA[cells]
  ],
  {name, names}
];

smeftsimTexDetailedRowsFA[names_List, target_:All] := Module[{rows},
  rows = Flatten[
    If[target === All,
      translateFromSMEFTsim /@ names,
      translateFromSMEFTsim[#, target] & /@ names
    ],
    1
  ];

  (
    Module[{row = #, full},
      smeftsimTexTableRowFA[
        If[KeyExistsQ[row, "Status"],
          {
            smeftsimTexMathCellFA[latexExprFA @ Lookup[row, "SMEFTsim", "-"]],
            "--",
            "--",
            smeftsimTexEscapeTextFA[Lookup[row, "Hint", "Not found in this translation file."]]
          },

          full = addFullInfoColumnsFA[row];
          {
            smeftsimTexMathCellFA[Lookup[full, "SMEFTsimLaTeX", "-"]],
            smeftsimTexMathCellFA[Lookup[full, "OperatorLaTeX", "-"]],
            smeftsimTexEscapeTextFA[Lookup[full, "Target", "-"]],
            If[
              smeftsimTexTargetIsTextQFA[row],
              smeftsimTexEscapeTextFA[Lookup[full, "TargetExpression", "-"]],
              smeftsimTexMathCellFA[Lookup[full, "TargetLaTeX", "-"]]
            ]
          }
        ]
      ]
    ] &
  ) /@ rows
];

smeftsimTexDocumentFA[tables_List, title_, includePreamble_] := Module[{body},
  body = StringRiffle[tables, "\n\n\\clearpage\n\n"];

  If[TrueQ[includePreamble],
    StringRiffle[
      {
        "\\documentclass[10pt,a4paper]{article}",
        "\\usepackage[T1]{fontenc}",
        "\\usepackage[utf8]{inputenc}",
        "\\usepackage[spanish]{babel}",
        "\\usepackage{amsmath,amssymb}",
        "\\usepackage{array}",
        "\\usepackage{booktabs}",
        "\\usepackage{longtable}",
        "\\usepackage{pdflscape}",
        "\\usepackage[margin=1.4cm]{geometry}",
        "\\newcolumntype{L}[1]{>{\\raggedright\\arraybackslash}p{#1}}",
        "\\setlength\\LTleft{0pt}",
        "\\setlength\\LTright{0pt}",
        "\\begin{document}",
        "\\begin{landscape}",
        "\\scriptsize",
        "\\setlength{\\tabcolsep}{3pt}",
        "\\renewcommand{\\arraystretch}{1.15}",
        "\\begin{center}\\textbf{" <> smeftsimTexEscapeTextFA[title] <> "}\\end{center}",
        body,
        "\\end{landscape}",
        "\\end{document}"
      },
      "\n"
    ],

    StringRiffle[
      {
        "% Required packages: amsmath, amssymb, array, booktabs, longtable, pdflscape.",
        "\\begin{landscape}",
        "\\scriptsize",
        "\\setlength{\\tabcolsep}{3pt}",
        "\\renewcommand{\\arraystretch}{1.15}",
        body,
        "\\end{landscape}"
      },
      "\n"
    ]
  ]
];

(* 
  Main usage:
    exportWCTranslationsTeX[{"ctWRe", "cHQ3"}, "tabla.tex"]
    exportWCTranslationsTeX[lastWCNames, "resultado.tex", "TableType" -> "Both"]
    exportWCTranslationsTeX[All, "traducciones_completas.tex", "TableType" -> "Detailed"]

  The function only needs the list of WC names. It does not require lastWCNames
  or any symbol defined by the user interface.
*)
exportWCTranslationsTeX[wcs_:All, path_:Automatic, OptionsPattern[]] := Module[
  {
    names, target, targets, tableType, title, includePreamble, tables = {},
    out, outDir, summaryHeaders, detailedHeaders, defaultName
  },

  target = OptionValue["Target"];
  tableType = ToLowerCase @ ToString @ OptionValue["TableType"];
  tableType = Replace[tableType, {
    "resumen" -> "summary",
    "detalle" -> "detailed",
    "detail" -> "detailed",
    "ambas" -> "both",
    "todas" -> "both",
    "all" -> "both"
  }];

  If[! MemberQ[{"summary", "detailed", "both"}, tableType],
    Message[exportWCTranslationsTeX::badtype, OptionValue["TableType"]];
    Return[$Failed]
  ];

  names = smeftsimTexInputNamesFA[wcs];
  target = If[target === All, All, ToString[target]];
  targets = If[target === All, allTranslationTargets[], {target}];
  title = OptionValue["Title"];
  includePreamble = TrueQ[OptionValue["IncludePreamble"]];

  If[MemberQ[{"summary", "both"}, tableType],
    summaryHeaders = Join[
      {
        "SMEFTsim",
        "Operator",
        "Dim.",
        "Lambda order"
      },
      targets
    ];

    AppendTo[
      tables,
      smeftsimTexLongTableFA[
        summaryHeaders,
        smeftsimTexSummaryRowsFA[names, targets],
        "Summary of Wilson-coefficient translations",
        "tab:smeftsim-wc-summary",
        smeftsimTexColumnSpecFA["Summary", Length[targets]]
      ]
    ]
  ];

  If[MemberQ[{"detailed", "both"}, tableType],
    detailedHeaders = {
      "SMEFTsim",
      "Operator",
      "Target",
      "Target expression"
    };

    AppendTo[
      tables,
      smeftsimTexLongTableFA[
        detailedHeaders,
        smeftsimTexDetailedRowsFA[names, target],
        "Detailed Wilson-coefficient translation table",
        "tab:smeftsim-wc-detailed",
        smeftsimTexColumnSpecFA["Detailed", Length[targets]]
      ]
    ]
  ];

  defaultName = "SMEFTsim_WC_traducciones_" <> tableType <> ".tex";
  out = If[path === Automatic,
    FileNameJoin[{smeftsimTexDefaultDirectoryFA[], defaultName}],
    ToString[path]
  ];

  If[ToLowerCase[FileExtension[out]] =!= "tex", out = out <> ".tex"];

  outDir = DirectoryName[out];
  If[
    StringQ[outDir] && outDir =!= "" && outDir =!= "." && ! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]
  ];

  Export[
    out,
    smeftsimTexDocumentFA[tables, title, includePreamble],
    "Text"
  ];

  out
];

ClearAll[wcTranslationLaTeXAuditFA];

(* Static presentation audit: all mathematical source/target expressions used
   by the translation rows should have an exact LaTeX entry.  "not-mapped"
   entries are intentionally text and are excluded from the mathematical list. *)
wcTranslationLaTeXAuditFA[] := Module[
  {rows, mapKeys, sourceExpressions, targetExpressions, textTargetRows,
   missingSource, missingTarget},

  rows = $SMEFTsimWCTranslations;
  mapKeys = Keys[$SMEFTsimLatexExactMap];
  sourceExpressions = DeleteDuplicates @ Lookup[rows, "SMEFTsim", {}];
  textTargetRows = Select[rows,
    StringContainsQ[Lookup[#, "Kind", ""], "not-mapped", IgnoreCase -> True] &
  ];
  targetExpressions = DeleteDuplicates @ Complement[
    Lookup[rows, "TargetExpression", {}],
    Lookup[textTargetRows, "TargetExpression", {}]
  ];

  missingSource = Complement[sourceExpressions, mapKeys];
  missingTarget = Complement[targetExpressions, mapKeys];

  <|
    "TranslationRows" -> Length[rows],
    "SourceExpressions" -> Length[sourceExpressions],
    "MathematicalTargetExpressions" -> Length[targetExpressions],
    "MissingSourceLaTeX" -> missingSource,
    "MissingTargetLaTeX" -> missingTarget,
    "PassedQ" -> (missingSource === {} && missingTarget === {})
  |>
];

translateLastWCResult[target_:All] := Which[
  ValueQ[lastWCNames], translateWCList[lastWCNames, target],
  ValueQ[lastWCResult], translateWCList[SymbolName /@ lastWCResult, target],
  True, Dataset[{<|"Status" -> "No lastWCNames or lastWCResult found. Export them from the interface first."|>}]
];

Print["SMEFTsim WC translation table loaded. Use wcTranslationsDataset[], translateFromSMEFTsim[\"ctWRe\"], translateWCList[lastWCNames], or wcCoverageReport[lastWCNames]."];

(* ========================================================= *)
(* 6. LaTeX display helpers                                  *)
(* ========================================================= *)

ClearAll[
  $SMEFTsimLatexExactMap,
  latexExactLookupFA,
  latexExprFA,
  addLatexColumnsFA,
  wcTranslationsDatasetLatex,
  translateFromSMEFTsimLatex,
  translateFromSMEFTsimLatexTable,
  translateWCListLatexTable,
  exportWCTranslationsLatexCSV
];

(* Exact translations for the most common SMEFTsim / UFO symbols.
   Everything is kept as strings so it is safe to load together with FeynArts/FeynCalc. *)
$SMEFTsimLatexExactMap = <|
  (* top Yukawa and dipoles *)
  "ctHRe" -> "C_{t\\varphi}",
  "ctHIm" -> "C_{t\\varphi}^{\\mathrm{Im}}",
  "ctGRe" -> "C_{tG}",
  "ctGIm" -> "C_{tG}^{\\mathrm{Im}}",
  "ctWRe" -> "C_{tW}",
  "ctWIm" -> "C_{tW}^{\\mathrm{Im}}",
  "ctBRe" -> "C_{tB}",
  "ctBIm" -> "C_{tB}^{\\mathrm{Im}}",
  "-ctWRe*cTheta + ctBRe*sTheta" -> "-c_\\theta C_{tW}+s_\\theta C_{tB}",
  "-ctWIm*cTheta + ctBIm*sTheta" -> "-c_\\theta C_{tW}^{\\mathrm{Im}}+s_\\theta C_{tB}^{\\mathrm{Im}}",

  (* Higgs-current operators, heavy and light quark doublets *)
  "cHt" -> "C_{\\varphi t}",
  "cHbq" -> "C_{\\varphi b}",
  "cHb" -> "C_{\\varphi b}",
  "cHQ1" -> "C_{\\varphi Q}^{(1)}",
  "cHQ3" -> "C_{\\varphi Q}^{(3)}",
  "cHQ1 - cHQ3" -> "C_{\\varphi Q}^{-}=C_{\\varphi Q}^{(1)}-C_{\\varphi Q}^{(3)}",
  "cHQ1 + cHQ3" -> "C_{\\varphi Q}^{+}=C_{\\varphi Q}^{(1)}+C_{\\varphi Q}^{(3)}",
  "cHj1" -> "C_{\\varphi q}^{(1)}",
  "cHj3" -> "C_{\\varphi q}^{(3)}",
  "cHj1 - cHj3" -> "C_{\\varphi q}^{-}=C_{\\varphi q}^{(1)}-C_{\\varphi q}^{(3)}",
  "cHtbRe" -> "C_{\\varphi tb}",
  "cHtbIm" -> "C_{\\varphi tb}^{\\mathrm{Im}}",

  (* Bosonic / EW input coefficients *)
  "cHbox" -> "C_{\\varphi\\Box}",
  "cHDD" -> "C_{\\varphi D}",
  "cHWB" -> "C_{\\varphi WB}",
  "cHG" -> "C_{\\varphi G}",
  "cHW" -> "C_{\\varphi W}",
  "cHB" -> "C_{\\varphi B}",

  (* two-quark two-lepton operators *)
  "cQe" -> "C_{Qe}",
  "cte" -> "C_{te}",
  "ctl" -> "C_{lt}",
  "cQl1" -> "C_{lQ}^{(1)}",
  "cQl3" -> "C_{lQ}^{(3)}",
  "cQl1 + cQl3" -> "C_{lQ}^{+}=C_{lQ}^{(1)}+C_{lQ}^{(3)}",
  "cQl1 - cQl3" -> "C_{lQ}^{-}=C_{lQ}^{(1)}-C_{lQ}^{(3)}",
  "cbl" -> "C_{lb}",
  "ceb" -> "C_{eb}",
  "cleQt" -> "C_{leQt}",
  "clebQ" -> "C_{lebQ}",
  "cledj" -> "C_{ledq}",
  "cleju" -> "C_{lequ}^{(1)}",

  (* SMEFT@NLO common parameters *)
  "ctp" -> "c_{t\\varphi}",
  "ctG" -> "c_{tG}",
  "ctW" -> "c_{tW}",
  "ctZ" -> "c_{tZ}",
  "cpQ3" -> "c_{\\varphi Q}^{3}",
  "cpQM" -> "c_{\\varphi Q}^{-}",
  "cpQ3 + cpQM" -> "c_{\\varphi Q}^{3}+c_{\\varphi Q}^{-}",
  "cpq3i" -> "c_{\\varphi q}^{3,i}",
  "cpqMi" -> "c_{\\varphi q}^{-,i}",
  "cpq3i + cpqMi" -> "c_{\\varphi q}^{3,i}+c_{\\varphi q}^{-,i}",
  "ctei" -> "c_{te}^{i}",
  "ctlSi" -> "c_{lt}^{S,i}",
  "ctlTi" -> "c_{lt}^{T,i}",

  (* dim6top common parameters *)
  "cpQ3" -> "c_{\\varphi Q}^{3}",
  "cpQM" -> "c_{\\varphi Q}^{-}",
  "ctpI" -> "c_{t\\varphi}^{\\mathrm{Im}}",
  "ctGI" -> "c_{tG}^{\\mathrm{Im}}"
|>;



(* --------------------------------------------------------- *)
(* Revision bibliografica SMEFTsim -> SMEFT@NLO              *)
(* Fuente: SMEFTsim 3.0 practical guide, App. E.2,           *)
(* Tables 28-31. Estas reglas corrigen exclusivamente la     *)
(* visualizacion LaTeX; no modifican la tabla algebraica.    *)
(* Cada barra de TeX se escribe como \\ en el codigo WL.       *)
(* --------------------------------------------------------- *)
$SMEFTsimLatexExactMap = Join[$SMEFTsimLatexExactMap, <|
  (* SMEFT@NLO: bosonic and Higgs-sector parameters, Tables 28 and 30 *)
  "-gs*cG" -> "-g_s\\,c_G",
  "-cWWW" -> "-c_{WWW}",
  "cp" -> "c_{\\varphi}",
  "cdp" -> "c_{\\varphi\\Box}",
  "cpDC" -> "c_{\\varphi D}",
  "cpG" -> "c_{\\varphi G}",
  "cpW" -> "c_{\\varphi W}",
  "cpBB" -> "c_{\\varphi B}",
  "cpWB" -> "c_{\\varphi WB}",

  (* SMEFT@NLO: top Yukawa and dipoles, Tables 28-31 *)
  "ctp" -> "c_{t\\varphi}",
  "-gs*ctG" -> "-g_s\\,c_{tG}",
  "-ctW" -> "-c_{tW}",
  "ctZ" -> "c_{tZ}",
  "ctZ/sTheta - ctW/tTheta" -> "\\frac{c_{tZ}}{s_\\theta}-\\frac{c_{tW}}{t_\\theta}",

  (* dim6top CP-odd dipoles and inverse neutral-dipole rotation *)
  "-ctG" -> "-c_{tG}",
  "-ctGI" -> "-c_{tG}^{\\mathrm{Im}}",
  "-ctWI" -> "-c_{tW}^{\\mathrm{Im}}",
  "ctZI" -> "c_{tZ}^{\\mathrm{Im}}",
  "ctZI/sTheta - ctWI/tTheta" -> "\\frac{c_{tZ}^{\\mathrm{Im}}}{s_\\theta}-\\frac{c_{tW}^{\\mathrm{Im}}}{t_\\theta}",

  (* SMEFT@NLO and dim6top heavy/light quark EW-current rotations, Table 29 *)
  "cpQ3" -> "c_{\\varphi Q}^{(3)}",
  "cpQM" -> "c_{\\varphi Q}^{-}",
  "cpQ3 + cpQM" -> "c_{\\varphi Q}^{(3)}+c_{\\varphi Q}^{-}",
  "cpq3i" -> "c_{\\varphi q}^{(3),i}",
  "cpqMi" -> "c_{\\varphi q}^{-,i}",
  "cpq3i + cpqMi" -> "c_{\\varphi q}^{(3),i}+c_{\\varphi q}^{-,i}",
  "cpt" -> "c_{\\varphi t}",
  "cpu" -> "c_{\\varphi u}",
  "cpd" -> "c_{\\varphi d}",
  "cpb" -> "c_{\\varphi b}",
  "cptb" -> "c_{\\varphi tb}",
  "cptbI" -> "c_{\\varphi tb}^{\\mathrm{Im}}",

  (* SMEFT@NLO: leptonic current parameters, Table 28 *)
  "cpe" -> "c_{\\varphi e}",
  "cpmu" -> "c_{\\varphi \\mu}",
  "cpta" -> "c_{\\varphi \\tau}",
  "cpe = cpmu = cpta" -> "c_{\\varphi e}=c_{\\varphi \\mu}=c_{\\varphi \\tau}",
  "cpl[p]" -> "c_{\\varphi l}^{(1),p}",
  "c3pl[p]" -> "c_{\\varphi l}^{(3),p}",

  (* Four-lepton and semileptonic topU3l entries, Tables 28-29 *)
  "cll[pppp] = cll[pprr]" -> "c_{ll}^{pppp}=c_{ll}^{pprr}",
  "cll[pppp] = cll[prrp]" -> "c_{ll}^{pppp}=c_{ll}^{prrp}",
  "cte[p]" -> "c_{te}^{p}",
  "cQe[p]" -> "c_{Qe}^{p}",
  "ctl[p]" -> "c_{lt}^{p}",
  "cbe[p]" -> "c_{be}^{p}",
  "cbl[p]" -> "c_{bl}^{p}",
  "cQl3[p]" -> "c_{lQ}^{(3),p}",
  "cQlM[p]" -> "c_{lQ}^{-,p}",
  "cQl3[p] + cQlM[p]" -> "c_{lQ}^{(3),p}+c_{lQ}^{-,p}",

  (* Scalar/tensor entries, including the explicit tau Yukawa in SMEFT@NLO *)
  "yl[3]*ctlS3" -> "y_l^{3}\\,c_{lt}^{S,3}",
  "yl[3]*ctlT3" -> "y_l^{3}\\,c_{lt}^{T,3}",
  "yl[3]*cblS3" -> "y_l^{3}\\,c_{bl}^{S,3}",
  "ctlS3" -> "c_{lt}^{S,3}",
  "ctlT3" -> "c_{lt}^{T,3}",
  "cblS3" -> "c_{bl}^{S,3}",
  "ctlSI3" -> "c_{lt}^{S,3,\\mathrm{Im}}",
  "ctlTI3" -> "c_{lt}^{T,3,\\mathrm{Im}}",
  "cblSI3" -> "c_{bl}^{S,3,\\mathrm{Im}}",

  (* Normalization factors and common four-quark names *)
  "1/2*cQQ1" -> "\\frac{1}{2}c_{QQ}^{1}",
  "1/2*cQQ8" -> "\\frac{1}{2}c_{QQ}^{8}",
  "cQQ1" -> "c_{QQ}^{1}",
  "cQQ8" -> "c_{QQ}^{8}",
  "cQq11" -> "c_{Qq}^{11}",
  "cQq81" -> "c_{Qq}^{81}",
  "cQq13" -> "c_{Qq}^{13}",
  "cQq83" -> "c_{Qq}^{83}",
  "ctu1" -> "c_{tu}^{1}",
  "ctu8" -> "c_{tu}^{8}",
  "ctd1" -> "c_{td}^{1}",
  "ctd8" -> "c_{td}^{8}",
  "ctt1" -> "c_{tt}^{1}",
  "cQt1" -> "c_{Qt}^{1}",
  "cQt8" -> "c_{Qt}^{8}",
  "cQu1" -> "c_{Qu}^{1}",
  "cQu8" -> "c_{Qu}^{8}",
  "cQd1" -> "c_{Qd}^{1}",
  "cQd8" -> "c_{Qd}^{8}",
  "ctq1" -> "c_{tq}^{1}",
  "ctq8" -> "c_{tq}^{8}",
  "ctb1" -> "c_{tb}^{1}",
  "ctb8" -> "c_{tb}^{8}",
  "cQb1" -> "c_{Qb}^{1}",
  "cQb8" -> "c_{Qb}^{8}",

  (* Names that occur in the Fierz/basis-rotation rows of dim6top. *)
  "cbtud1" -> "c_{btud}^{(1)}",
  "cbtud8" -> "c_{btud}^{(8)}",
  "ctQqu1" -> "c_{tQqu}^{(1)}",
  "ctQqu8" -> "c_{tQqu}^{(8)}",
  "cbQqd1" -> "c_{bQqd}^{(1)}",

  (* Canonical SMEFTsim-general source combinations. *)
  "cHq1Re33" -> "C_{\\varphi q}^{(1),33}",
  "cHq3Re33" -> "C_{\\varphi q}^{(3),33}",
  "cHq3Re[rr]" -> "C_{\\varphi q}^{(3),rr}",
  "cHq1Re33 - cHq3Re33" -> "C_{\\varphi q}^{(1),33}-C_{\\varphi q}^{(3),33}",
  "cHq1Re[rr] - cHq3Re[rr]" -> "C_{\\varphi q}^{(1),rr}-C_{\\varphi q}^{(3),rr}",
  "-cuWRe33*cTheta + cuBRe33*sTheta" -> "-c_{\\theta}C_{uW}^{33}+s_{\\theta}C_{uB}^{33}",
  "cuBRe33" -> "C_{uB}^{33}",

  (* Explicit Fierz combinations from the dim6top translation table. *)
  "1/3*cutbd1Re + 4/9*cutbd8Re*yu[r]*yd[s]" -> "\\frac{1}{3}C_{utbd}^{(1),\\mathrm{Re}}+\\frac{4}{9}C_{utbd}^{(8),\\mathrm{Re}}y_u^r y_d^s",
  "2*cutbd1Re - 1/3*cutbd8Re*yu[r]*yd[s]" -> "2C_{utbd}^{(1),\\mathrm{Re}}-\\frac{1}{3}C_{utbd}^{(8),\\mathrm{Re}}y_u^r y_d^s",
  "-(2/3)*cjQtu1Re - (8/9)*cjQtu8Re*yu[r]" -> "-\\frac{2}{3}C_{jQtu}^{(1),\\mathrm{Re}}-\\frac{8}{9}C_{jQtu}^{(8),\\mathrm{Re}}y_u^r",
  "-4*cjQtu1Re + (2/3)*cjQtu8Re*yu[r]" -> "-4C_{jQtu}^{(1),\\mathrm{Re}}+\\frac{2}{3}C_{jQtu}^{(8),\\mathrm{Re}}y_u^r",
  "-(2/3)*cjQbd1Re - (8/9)*cjQbd8Re*yd[r]" -> "-\\frac{2}{3}C_{jQbd}^{(1),\\mathrm{Re}}-\\frac{8}{9}C_{jQbd}^{(8),\\mathrm{Re}}y_d^r"
|>];

latexExactLookupFA[s_String] := Lookup[$SMEFTsimLatexExactMap, StringTrim[s], Missing["NotFound"]];

latexExprFA[expr_] := Module[{s, exact, tokens},
  s = StringTrim @ ToString[expr];
  exact = latexExactLookupFA[s];
  If[exact =!= Missing["NotFound"], Return[exact]];

  (* Lightweight fallback: keep algebra readable and replace known tokens when possible. *)
  tokens = Keys[$SMEFTsimLatexExactMap];
  tokens = Reverse @ SortBy[tokens, StringLength];

  s = StringReplace[s, {
      "*" -> " ",
      "cTheta" -> "c_\\theta",
      "sTheta" -> "s_\\theta",
      "gs" -> "g_s",
      "yu[r]" -> "y_u^r",
      "yd[s]" -> "y_d^s",
      "[rr]" -> "_{rr}",
      "[r]" -> "_r",
      "[s]" -> "_s"
    }];

  Fold[StringReplace[#1, #2 -> Lookup[$SMEFTsimLatexExactMap, #2]] &, s, tokens]
];

addLatexColumnsFA[row_Association] := Join[
  row,
  <|
    "SMEFTsimLaTeX" -> latexExprFA @ Lookup[row, "SMEFTsim", ""],
    "TargetLaTeX" -> latexExprFA @ Lookup[row, "TargetExpression", ""]
  |>
];

wcTranslationsDatasetLatex[] := Dataset[addLatexColumnsFA /@ $SMEFTsimWCTranslations][
  All,
  {"SMEFTsim", "SMEFTsimLaTeX", "Target", "TargetExpression", "TargetLaTeX"}
];

translateFromSMEFTsimLatex[wc_, target_:All] := Module[{rows},
  rows = If[target === All, translateFromSMEFTsim[wc], translateFromSMEFTsim[wc, target]];
  addLatexColumnsFA /@ rows
];

translateFromSMEFTsimLatexTable[wc_, target_:All] := Dataset[translateFromSMEFTsimLatex[wc, target]][
  All,
  {"SMEFTsim", "SMEFTsimLaTeX", "Target", "TargetExpression", "TargetLaTeX"}
];

translateWCListLatexTable[wcs_List, target_:All] := Module[{names, rows},
  names = wcToString /@ wcs;
  rows = Flatten[translateFromSMEFTsimLatex[#, target] & /@ names, 1];
  Dataset[rows][All, {"SMEFTsim", "SMEFTsimLaTeX", "Target", "TargetExpression", "TargetLaTeX"}]
];

exportWCTranslationsLatexCSV[path_:Automatic] := Module[{out, rows},
  out = If[path === Automatic,
    FileNameJoin[{NotebookDirectory[], "SMEFTsim_WC_traducciones_UFO_LATEX.csv"}],
    path
  ];
  rows = Normal[addLatexColumnsFA /@ $SMEFTsimWCTranslations] /. a_Association :> KeyDrop[a, "Aliases"];
  Export[out, rows, "CSV"];
  out
];

Print["LaTeX helpers loaded. Use translateFromSMEFTsimLatexTable[\"cHj1\"], translateWCListLatexTable[lastWCNames], or wcTranslationsDatasetLatex[]."];

(* ========================================================= *)
(* 5. Renderizado automatico de notacion LaTeX               *)
(*    - Si MaTeX esta instalado y funciona: usa MaTeX        *)
(*    - Si no: usa notacion nativa de Mathematica            *)
(* ========================================================= *)

ClearAll[
  smeftsimMaTeXAvailableQ,
  smeftsimResetMaTeXCheck,
  nativeLatexSymbolStringFA,
  nativeSuperscriptStringFA,
  nativeLatexReadBracedFA,
  nativeLatexFractionPartsFA,
  topLevelSplitFA,
  nativeLatexAtomFA,
  nativeLatexProductFA,
  nativeLatexSumFA,
  nativeLatexFormulaFA,
  renderWCNotationFA,
  renderTranslationTargetFA,
  translationRowsWithLatexFA,
  translationRowsRenderedFA,
  translateFromSMEFTsimRenderedTable,
  translateWCListRenderedTable,
  wcTranslationsRenderedTable,
  translateLastWCResultRendered
];

$SMEFTsimMaTeXChecked = False;
$SMEFTsimMaTeXAvailable = False;
$SMEFTsimMaTeXMagnification = 1.15;
$SMEFTsimRenderCache = <||>;

smeftsimResetMaTeXCheck[] := (
  $SMEFTsimMaTeXChecked = False;
  $SMEFTsimMaTeXAvailable = False;
  $SMEFTsimRenderCache = <||>;
);

smeftsimMaTeXAvailableQ[] := Module[{test},
  If[TrueQ[$SMEFTsimMaTeXChecked], Return[TrueQ[$SMEFTsimMaTeXAvailable]]];

  test = Quiet @ Check[
     Needs["MaTeX`"];
     MaTeX["x", Magnification -> 0.6],
     $Failed
  ];

  $SMEFTsimMaTeXAvailable = test =!= $Failed;
  $SMEFTsimMaTeXChecked = True;

  TrueQ[$SMEFTsimMaTeXAvailable]
];

nativeLatexSymbolStringFA[s_String] := StringReplace[StringTrim[s], {
   "\\varphi" -> "\[CurlyPhi]",
   "\\phi" -> "\[Phi]",
   "\\theta" -> "\[Theta]",
   "\\Theta" -> "\[CapitalTheta]",
   "\\Lambda" -> "\[CapitalLambda]",
   "\\mu" -> "\[Mu]",
   "\\tau" -> "\[Tau]",
   "\\epsilon" -> "\[Epsilon]",
   "\\sigma" -> "\[Sigma]",
   "\\gamma" -> "\[Gamma]",
   "\\nu" -> "\[Nu]",
   "\\rho" -> "\[Rho]",
   "\\Box" -> "\[Square]",
   "\\mathrm{Im}" -> "Im",
   "\\mathrm{Re}" -> "Re",
   "\\dagger" -> "\[Dagger]",
   "\\bar" -> "bar ",
   "\\tilde" -> "tilde ",
   "\\," -> " ",
   "\\!" -> "",
   "\\;" -> " ",
   "\\:" -> " ",
   "\\quad" -> " ",
   "\\displaystyle" -> "",
   "\\ " -> " ",
   "{" -> "",
   "}" -> ""
}];

nativeSuperscriptStringFA[s_String] := Module[{t},
  t = nativeLatexSymbolStringFA[s];
  t = StringReplace[t, {
     "(1)" -> "(1)",
     "(3)" -> "(3)",
     "\\mathrmIm" -> "Im"
  }];
  t
];

(* Read one balanced {...} group. It is deliberately small and self-contained,
   because the fallback renderer only needs the subset of TeX used here. *)
nativeLatexReadBracedFA[s_String, start_Integer] := Module[
  {chars = Characters[s], n = StringLength[s], i = start, depth = 0, out = "", ch},

  If[i > n || chars[[i]] =!= "{", Return[$Failed]];

  Do[
    ch = chars[[i]];
    Which[
      ch === "{",
        depth++;
        If[depth > 1, out = out <> ch],
      ch === "}",
        depth--;
        If[depth == 0, Return[<|"Content" -> out, "NextIndex" -> i + 1|>]];
        out = out <> ch,
      True,
        out = out <> ch
    ],
    {i, start, n}
  ];

  $Failed
];

(* Parse a leading \frac{numerator}{denominator}, preserving any suffix such as
   \frac{1}{2}c_{QQ}^{1}. *)
nativeLatexFractionPartsFA[s_String] := Module[{t, first, second, p},
  t = StringTrim[s];
  If[! StringStartsQ[t, "\\frac{"], Return[$Failed]];

  first = nativeLatexReadBracedFA[t, StringLength["\\frac"] + 1];
  If[first === $Failed, Return[$Failed]];

  p = first["NextIndex"];
  second = nativeLatexReadBracedFA[t, p];
  If[second === $Failed, Return[$Failed]];

  <|
    "Numerator" -> first["Content"],
    "Denominator" -> second["Content"],
    "Suffix" -> If[
      second["NextIndex"] > StringLength[t],
      "",
      StringTrim @ StringTake[t, {second["NextIndex"], -1}]
    ]
  |>
];

topLevelSplitFA[s_String, ops_List] := Module[
  {chars, depth = 0, current = "", out = {}, ch},

  chars = Characters[s];

  Do[
    ch = chars[[i]];
    Which[
      ch === "{",
        depth++;
        current = current <> ch,
      ch === "}",
        depth = Max[0, depth - 1];
        current = current <> ch,
      depth === 0 && MemberQ[ops, ch],
        If[StringTrim[current] =!= "", AppendTo[out, StringTrim[current]]];
        AppendTo[out, ch];
        current = "",
      True,
        current = current <> ch
    ],
    {i, Length[chars]}
  ];

  If[StringTrim[current] =!= "", AppendTo[out, StringTrim[current]]];
  out
];

nativeLatexAtomFA[s_String] := Module[
  {t, m, base, sub, sup, expr, frac, fracBox, suffix},

  t = StringTrim[s];
  If[t === "" || t === "-", Return["-"]];

  (* Fractions occur in the ctB/ctBI inverse rotations and in normalization
     factors. Handle them before token-level parsing. *)
  frac = nativeLatexFractionPartsFA[t];
  If[AssociationQ[frac],
    fracBox = FractionBox[
      ToBoxes[nativeLatexFormulaFA[frac["Numerator"]], StandardForm],
      ToBoxes[nativeLatexFormulaFA[frac["Denominator"]], StandardForm]
    ];
    suffix = frac["Suffix"];
    Return[
      If[suffix === "",
        fracBox,
        Row[{fracBox, " ", nativeLatexProductFA[suffix]}]
      ]
    ]
  ];

  (* A standalone Lambda power is used in the order column. *)
  m = StringCases[t,
    RegularExpression["^\\\\Lambda\\^\\{([^{}]+)\\}$"] :> {"$1"}
  ];
  If[m =!= {},
    Return[TraditionalForm[
      Superscript[Style[\[CapitalLambda], Italic], Style[nativeSuperscriptStringFA[First @ First[m]], Italic]]
    ]]
  ];

  (* Coefficients such as C_{\varphi q}^{(1)}, c_{tZ}, c_{\varphi q}^{3,i}. *)
  m = StringCases[t,
    RegularExpression["^([A-Za-z]+)_\\{([^{}]+)\\}(\\^\\{([^{}]+)\\})?$"] :>
      {"$1", "$2", "$4"}
  ];

  If[m =!= {},
    {base, sub, sup} = First[m];
    expr = Subscript[
      Style[base, Italic],
      Style[nativeLatexSymbolStringFA[sub], Italic]
    ];
    If[StringTrim[sup] =!= "",
      expr = Superscript[expr, Style[nativeSuperscriptStringFA[sup], Italic]]
    ];
    Return[TraditionalForm[expr]];
  ];

  (* Symbols with a simple subscript, e.g. c_W, s_W, g_s. *)
  m = StringCases[t,
    RegularExpression["^([A-Za-z]+)_([A-Za-z0-9]+)$"] :>
      {"$1", "$2"}
  ];

  If[m =!= {},
    {base, sub} = First[m];
    Return[TraditionalForm[Subscript[Style[base, Italic], Style[nativeLatexSymbolStringFA[sub], Italic]]]];
  ];

  (* Simple powers such as x^{Im}. *)
  m = StringCases[t,
    RegularExpression["^([A-Za-z0-9]+)\\^\\{([^{}]+)\\}$"] :>
      {"$1", "$2"}
  ];

  If[m =!= {},
    {base, sup} = First[m];
    Return[TraditionalForm[Superscript[Style[base, Italic], Style[nativeSuperscriptStringFA[sup], Italic]]]];
  ];

  Style[nativeLatexSymbolStringFA[t], Italic]
];

nativeLatexProductFA[s_String] := Module[{parts},
  parts = DeleteCases[StringSplit[StringTrim[s], Whitespace], ""];
  If[Length[parts] <= 1,
    nativeLatexAtomFA[StringTrim[s]],
    TraditionalForm[Row[Riffle[nativeLatexAtomFA /@ parts, " "]]]
  ]
];

nativeLatexSumFA[s_String] := Module[{parts},
  parts = topLevelSplitFA[StringTrim[s], {"+", "-"}];
  If[Length[parts] <= 1,
    nativeLatexProductFA[StringTrim[s]],
    TraditionalForm[
      Row[
        ((parts /. {
            "+" -> " + ",
            "-" -> " - "
          }) /. (x_String /; x =!= " + " && x =!= " - ") :> nativeLatexProductFA[x])
      ]
    ]
  ]
];

nativeLatexFormulaFA[s_String] := Module[{parts},
  parts = topLevelSplitFA[StringTrim[s], {"="}];
  If[Length[parts] <= 1,
    nativeLatexSumFA[StringTrim[s]],
    TraditionalForm[
      Row[
        ((parts /. "=" -> " = ") /. (x_String /; x =!= " = ") :> nativeLatexSumFA[x])
      ]
    ]
  ]
];

renderWCNotationFA[s_String, mode_:"Auto"] := Module[{latex, key, useMaTeX, rendered},
  latex = StringTrim[s];

  If[latex === "" || latex === "-", Return["-"]];

  key = {ToString[mode], latex};
  If[KeyExistsQ[$SMEFTsimRenderCache, key], Return[$SMEFTsimRenderCache[key]]];

  useMaTeX = Which[
    mode === "MaTeX", True,
    mode === "Native" || mode === "Mathematica", False,
    True, smeftsimMaTeXAvailableQ[]
  ];

  rendered = If[TrueQ[useMaTeX],
    Quiet @ Check[
      MaTeX[latex, Magnification -> $SMEFTsimMaTeXMagnification],
      nativeLatexFormulaFA[latex]
    ],
    nativeLatexFormulaFA[latex]
  ];

  $SMEFTsimRenderCache[key] = rendered;
  rendered
];

renderTranslationTargetFA[row_Association, mode_:"Auto"] := If[
  StringContainsQ[Lookup[row, "Kind", ""], "not-mapped", IgnoreCase -> True],
  Style[Lookup[row, "TargetExpression", "-"], Darker[Gray]],
  renderWCNotationFA[
    Lookup[row, "TargetLaTeX", Lookup[row, "TargetExpression", "-"]],
    mode
  ]
];

translationRowsWithLatexFA[rows_List] := addLatexColumnsFA /@ rows;

translationRowsRenderedFA[rows_List, mode_:"Auto"] := Module[{rowsLatex},
  rowsLatex = translationRowsWithLatexFA[rows];
  ({
      Lookup[#, "SMEFTsim", "-"],
      renderWCNotationFA[Lookup[#, "SMEFTsimLaTeX", Lookup[#, "SMEFTsim", "-"]], mode],
      Lookup[#, "Target", "-"],
      Lookup[#, "TargetExpression", "-"],
      renderTranslationTargetFA[#, mode]
    } & /@ rowsLatex)
];

translateFromSMEFTsimRenderedTable[wc_, target_:All, mode_:"Auto"] := Module[{rows, body},
  rows = If[target === All,
    translateFromSMEFTsim[wc],
    translateFromSMEFTsim[wc, target]
  ];

  If[rows === {},
    Return[
      Grid[
        {
          {Style["SMEFTsim", Bold], Style["Resultado", Bold]},
          {wcToString[wc], Style["Sin traduccion encontrada", Red]}
        },
        Frame -> All,
        Alignment -> Left,
        Spacings -> {1.2, 0.8}
      ]
    ]
  ];

  body = translationRowsRenderedFA[rows, mode];

  Grid[
    Prepend[
      body,
      Style[#, Bold] & /@ {
        "SMEFTsim",
        "SMEFTsim notacion",
        "Destino",
        "Expresion destino",
        "Destino notacion"
      }
    ],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.2, 0.8},
    ItemSize -> All
  ]
];

translateWCListRenderedTable[wcs_List, target_:All, mode_:"Auto"] := Module[{names, rows, body},
  names = wcToString /@ wcs;

  rows = Flatten[
    If[target === All,
      translateFromSMEFTsim /@ names,
      translateFromSMEFTsim[#, target] & /@ names
    ],
    1
  ];

  If[rows === {},
    Return[
      Grid[
        {
          {Style["Resultado", Bold]},
          {Style["Sin traducciones encontradas", Red]}
        },
        Frame -> All,
        Alignment -> Left
      ]
    ]
  ];

  body = translationRowsRenderedFA[rows, mode];

  Grid[
    Prepend[
      body,
      Style[#, Bold] & /@ {
        "SMEFTsim",
        "SMEFTsim notacion",
        "Destino",
        "Expresion destino",
        "Destino notacion"
      }
    ],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.2, 0.8},
    ItemSize -> All
  ]
];

wcTranslationsRenderedTable[target_:All, mode_:"Auto"] := Module[{rows},
  rows = If[target === All,
    $SMEFTsimWCTranslations,
    Select[$SMEFTsimWCTranslations, Lookup[#, "Target", ""] === ToString[target] &]
  ];
  translateWCListRenderedTable[DeleteDuplicates[Lookup[#, "SMEFTsim"] & /@ rows], target, mode]
];

translateLastWCResultRendered[target_:All, mode_:"Auto"] := Module[{names},
  names = Quiet @ Check[lastWCNames, {}];
  If[names === {} || ! ListQ[names],
    Return[
      Grid[
        {
          {Style["Resultado", Bold]},
          {Style["No existe lastWCNames o esta vacio. Ejecuta primero la interfaz y guarda lastWCNames.", Red]}
        },
        Frame -> All,
        Alignment -> Left
      ]
    ]
  ];
  translateWCListRenderedTable[names, target, mode]
];

Print[
  "Render automatico cargado. Usa translateFromSMEFTsimRenderedTable[\"cHj1\"], ",
  "translateWCListRenderedTable[lastWCNames] o translateLastWCResultRendered[]. ",
  "Modo Auto usa MaTeX si funciona; si no, usa notacion nativa de Mathematica."
];

(* ========================================================= *)
(* 7. Operator helpers                                       *)
(*    Se anaden columnas con el operador de Warsaw asociado al WC. *)
(*    asociado al WC. Si MaTeX funciona, se renderiza como   *)
(*    formula LaTeX real; si no, cae al modo nativo.         *)
(* ========================================================= *)

ClearAll[
  $SMEFTsimOperatorExactMap,
  operatorExactLookupFA,
  operatorInfoForRowFA,
  addOperatorColumnsFA,
  operatorTranslationsDataset,
  translateFromSMEFTsimOperatorTable,
  translateWCListOperatorTable
];

$SMEFTsimOperatorExactMap = <|

  (* Bosonic Warsaw operators *)
  "cG" -> <|"Operator" -> "QG", "OperatorLaTeX" -> "Q_G=f^{ABC}G^A_{\\mu\\nu}G^{B\\nu}_{\\rho}G^{C\\rho\\mu}"|>,
  "cW" -> <|"Operator" -> "QW", "OperatorLaTeX" -> "Q_W=\\epsilon^{IJK}W^I_{\\mu\\nu}W^{J\\nu}_{\\rho}W^{K\\rho\\mu}"|>,
  "cH" -> <|"Operator" -> "QH", "OperatorLaTeX" -> "Q_{\\varphi}=(\\varphi^\\dagger\\varphi)^3"|>,
  "cHbox" -> <|"Operator" -> "QHbox", "OperatorLaTeX" -> "Q_{\\varphi\\Box}=(\\varphi^\\dagger\\varphi)\\Box(\\varphi^\\dagger\\varphi)"|>,
  "cHDD" -> <|"Operator" -> "QHD", "OperatorLaTeX" -> "Q_{\\varphi D}=(\\varphi^\\dagger D_\\mu\\varphi)^*(\\varphi^\\dagger D^\\mu\\varphi)"|>,
  "cHG" -> <|"Operator" -> "QHG", "OperatorLaTeX" -> "Q_{\\varphi G}=(\\varphi^\\dagger\\varphi)G^A_{\\mu\\nu}G^{A\\mu\\nu}"|>,
  "cHW" -> <|"Operator" -> "QHW", "OperatorLaTeX" -> "Q_{\\varphi W}=(\\varphi^\\dagger\\varphi)W^I_{\\mu\\nu}W^{I\\mu\\nu}"|>,
  "cHB" -> <|"Operator" -> "QHB", "OperatorLaTeX" -> "Q_{\\varphi B}=(\\varphi^\\dagger\\varphi)B_{\\mu\\nu}B^{\\mu\\nu}"|>,
  "cHWB" -> <|"Operator" -> "QHWB", "OperatorLaTeX" -> "Q_{\\varphi WB}=(\\varphi^\\dagger\\tau^I\\varphi)W^I_{\\mu\\nu}B^{\\mu\\nu}"|>,

  (* Two-quark top/EW operators *)
  "ctHRe" -> <|"Operator" -> "Qtphi", "OperatorLaTeX" -> "Q_{t\\varphi}=(\\bar Q t)\\epsilon\\varphi^*(\\varphi^\\dagger\\varphi)"|>,
  "ctHIm" -> <|"Operator" -> "Qtphi", "OperatorLaTeX" -> "Q_{t\\varphi}=(\\bar Q t)\\epsilon\\varphi^*(\\varphi^\\dagger\\varphi)"|>,
  "ctGRe" -> <|"Operator" -> "QtG", "OperatorLaTeX" -> "Q_{tG}=(\\bar Q\\sigma^{\\mu\\nu}T^A t)\\epsilon\\varphi^*G^A_{\\mu\\nu}"|>,
  "ctGIm" -> <|"Operator" -> "QtG", "OperatorLaTeX" -> "Q_{tG}=(\\bar Q\\sigma^{\\mu\\nu}T^A t)\\epsilon\\varphi^*G^A_{\\mu\\nu}"|>,
  "ctWRe" -> <|"Operator" -> "QtW", "OperatorLaTeX" -> "Q_{tW}=(\\bar Q\\tau^I\\sigma^{\\mu\\nu}t)\\epsilon\\varphi^*W^I_{\\mu\\nu}"|>,
  "ctWIm" -> <|"Operator" -> "QtW", "OperatorLaTeX" -> "Q_{tW}=(\\bar Q\\tau^I\\sigma^{\\mu\\nu}t)\\epsilon\\varphi^*W^I_{\\mu\\nu}"|>,
  "ctBRe" -> <|"Operator" -> "QtB", "OperatorLaTeX" -> "Q_{tB}=(\\bar Q\\sigma^{\\mu\\nu}t)\\epsilon\\varphi^*B_{\\mu\\nu}"|>,
  "ctBIm" -> <|"Operator" -> "QtB", "OperatorLaTeX" -> "Q_{tB}=(\\bar Q\\sigma^{\\mu\\nu}t)\\epsilon\\varphi^*B_{\\mu\\nu}"|>,
  "-ctWRe*cTheta + ctBRe*sTheta" -> <|"Operator" -> "QtZ", "OperatorLaTeX" -> "Q_{tZ}=c_W Q_{tW}-s_W Q_{tB}"|>,
  "-ctWIm*cTheta + ctBIm*sTheta" -> <|"Operator" -> "QtZ", "OperatorLaTeX" -> "Q_{tZ}^{\\mathrm{Im}}=c_W Q_{tW}^{\\mathrm{Im}}-s_W Q_{tB}^{\\mathrm{Im}}"|>,

  "cHQ1" -> <|"Operator" -> "QphiQ1", "OperatorLaTeX" -> "Q_{\\varphi Q}^{(1)}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar Q\\gamma^\\mu Q)"|>,
  "cHQ3" -> <|"Operator" -> "QphiQ3", "OperatorLaTeX" -> "Q_{\\varphi Q}^{(3)}=(\\varphi^\\dagger i\\overleftrightarrow{D}^{I}_\\mu\\varphi)(\\bar Q\\tau^I\\gamma^\\mu Q)"|>,
  "cHQ1 - cHQ3" -> <|"Operator" -> "QphiQminus", "OperatorLaTeX" -> "Q_{\\varphi Q}^{-}=Q_{\\varphi Q}^{(1)}-Q_{\\varphi Q}^{(3)}"|>,
  "cHQ1 + cHQ3" -> <|"Operator" -> "QphiQplus", "OperatorLaTeX" -> "Q_{\\varphi Q}^{+}=Q_{\\varphi Q}^{(1)}+Q_{\\varphi Q}^{(3)}"|>,
  "cHj1" -> <|"Operator" -> "Qphiq1", "OperatorLaTeX" -> "Q_{\\varphi q}^{(1)}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar q\\gamma^\\mu q)"|>,
  "cHj3" -> <|"Operator" -> "Qphiq3", "OperatorLaTeX" -> "Q_{\\varphi q}^{(3)}=(\\varphi^\\dagger i\\overleftrightarrow{D}^{I}_\\mu\\varphi)(\\bar q\\tau^I\\gamma^\\mu q)"|>,
  "cHj1 - cHj3" -> <|"Operator" -> "Qphiqminus", "OperatorLaTeX" -> "Q_{\\varphi q}^{-}=Q_{\\varphi q}^{(1)}-Q_{\\varphi q}^{(3)}"|>,
  "cHt" -> <|"Operator" -> "Qphit", "OperatorLaTeX" -> "Q_{\\varphi t}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar t\\gamma^\\mu t)"|>,
  "cHu" -> <|"Operator" -> "Qphiu", "OperatorLaTeX" -> "Q_{\\varphi u}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar u\\gamma^\\mu u)"|>,
  "cHbq" -> <|"Operator" -> "Qphib", "OperatorLaTeX" -> "Q_{\\varphi b}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar b\\gamma^\\mu b)"|>,
  "cHb" -> <|"Operator" -> "Qphib", "OperatorLaTeX" -> "Q_{\\varphi b}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar b\\gamma^\\mu b)"|>,
  "cHtbRe" -> <|"Operator" -> "Qphitb", "OperatorLaTeX" -> "Q_{\\varphi tb}=i(\\tilde\\varphi^\\dagger D_\\mu\\varphi)(\\bar t\\gamma^\\mu b)"|>,
  "cHtbIm" -> <|"Operator" -> "Qphitb", "OperatorLaTeX" -> "Q_{\\varphi tb}=i(\\tilde\\varphi^\\dagger D_\\mu\\varphi)(\\bar t\\gamma^\\mu b)"|>,

  (* Lepton-current operators *)
  "cHl1" -> <|"Operator" -> "QphiL1", "OperatorLaTeX" -> "Q_{\\varphi l}^{(1)}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar l\\gamma^\\mu l)"|>,
  "cHl3" -> <|"Operator" -> "QphiL3", "OperatorLaTeX" -> "Q_{\\varphi l}^{(3)}=(\\varphi^\\dagger i\\overleftrightarrow{D}^{I}_\\mu\\varphi)(\\bar l\\tau^I\\gamma^\\mu l)"|>,
  "cHe" -> <|"Operator" -> "Qphie", "OperatorLaTeX" -> "Q_{\\varphi e}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar e\\gamma^\\mu e)"|>,
  "cHe11" -> <|"Operator" -> "Qphie", "OperatorLaTeX" -> "Q_{\\varphi e}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar e\\gamma^\\mu e)"|>,
  "cHe22" -> <|"Operator" -> "Qphie", "OperatorLaTeX" -> "Q_{\\varphi e}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar e\\gamma^\\mu e)"|>,
  "cHe33" -> <|"Operator" -> "Qphie", "OperatorLaTeX" -> "Q_{\\varphi e}=(\\varphi^\\dagger i\\overleftrightarrow{D}_\\mu\\varphi)(\\bar e\\gamma^\\mu e)"|>,

  (* Two-quark two-lepton operators *)
  "cQe" -> <|"Operator" -> "QeQ", "OperatorLaTeX" -> "Q_{eQ}=(\\bar Q\\gamma_\\mu Q)(\\bar e\\gamma^\\mu e)"|>,
  "cte" -> <|"Operator" -> "Qet", "OperatorLaTeX" -> "Q_{et}=(\\bar t\\gamma_\\mu t)(\\bar e\\gamma^\\mu e)"|>,
  "cbe" -> <|"Operator" -> "Qeb", "OperatorLaTeX" -> "Q_{eb}=(\\bar b\\gamma_\\mu b)(\\bar e\\gamma^\\mu e)"|>,
  "ctl" -> <|"Operator" -> "Qlt", "OperatorLaTeX" -> "Q_{lt}=(\\bar t\\gamma_\\mu t)(\\bar l\\gamma^\\mu l)"|>,
  "cbl" -> <|"Operator" -> "Qlb", "OperatorLaTeX" -> "Q_{lb}=(\\bar b\\gamma_\\mu b)(\\bar l\\gamma^\\mu l)"|>,
  "cQl1" -> <|"Operator" -> "QlQ1", "OperatorLaTeX" -> "Q_{lQ}^{(1)}=(\\bar Q\\gamma_\\mu Q)(\\bar l\\gamma^\\mu l)"|>,
  "cQl3" -> <|"Operator" -> "QlQ3", "OperatorLaTeX" -> "Q_{lQ}^{(3)}=(\\bar Q\\tau^I\\gamma_\\mu Q)(\\bar l\\tau^I\\gamma^\\mu l)"|>,
  "cQl1 + cQl3" -> <|"Operator" -> "QlQplus", "OperatorLaTeX" -> "Q_{lQ}^{+}=Q_{lQ}^{(1)}+Q_{lQ}^{(3)}"|>,
  "cQl1 - cQl3" -> <|"Operator" -> "QlQminus", "OperatorLaTeX" -> "Q_{lQ}^{-}=Q_{lQ}^{(1)}-Q_{lQ}^{(3)}"|>,

  (* Scalar/tensor semileptonic operators *)
  "cleQt1Re33" -> <|"Operator" -> "Qlequ1", "OperatorLaTeX" -> "Q_{lequ}^{(1)}=(\\bar l^j e)\\epsilon_{jk}(\\bar Q^k t)"|>,
  "cleQt3Re33" -> <|"Operator" -> "Qlequ3", "OperatorLaTeX" -> "Q_{lequ}^{(3)}=(\\bar l^j\\sigma_{\\mu\\nu}e)\\epsilon_{jk}(\\bar Q^k\\sigma^{\\mu\\nu}t)"|>,
  "clebQRe33" -> <|"Operator" -> "Qledq", "OperatorLaTeX" -> "Q_{ledq}=(\\bar l^j e)(\\bar b Q_j)"|>,

  (* Four-lepton operators *)
  "cll" -> <|"Operator" -> "Qll", "OperatorLaTeX" -> "Q_{ll}=(\\bar l\\gamma_\\mu l)(\\bar l\\gamma^\\mu l)"|>,
  "cll1" -> <|"Operator" -> "Qll-prrp", "OperatorLaTeX" -> "Q_{ll}^{\\prime}=(\\bar l_p\\gamma_\\mu l_r)(\\bar l_r\\gamma^\\mu l_p)"|>,

  (* Four-quark heavy-light and four-heavy operators *)
  "cQj11" -> <|"Operator" -> "QQq11", "OperatorLaTeX" -> "Q_{Qq}^{1,1}=(\\bar Q\\gamma_\\mu Q)(\\bar q\\gamma^\\mu q)"|>,
  "cQj18" -> <|"Operator" -> "QQq18", "OperatorLaTeX" -> "Q_{Qq}^{1,8}=(\\bar Q\\gamma_\\mu T^A Q)(\\bar q\\gamma^\\mu T^A q)"|>,
  "cQj31" -> <|"Operator" -> "QQq31", "OperatorLaTeX" -> "Q_{Qq}^{3,1}=(\\bar Q\\tau^I\\gamma_\\mu Q)(\\bar q\\tau^I\\gamma^\\mu q)"|>,
  "cQj38" -> <|"Operator" -> "QQq38", "OperatorLaTeX" -> "Q_{Qq}^{3,8}=(\\bar Q\\tau^I\\gamma_\\mu T^A Q)(\\bar q\\tau^I\\gamma^\\mu T^A q)"|>,
  "cQQ1" -> <|"Operator" -> "QQQ1", "OperatorLaTeX" -> "Q_{QQ}^{1}=(\\bar Q\\gamma_\\mu Q)(\\bar Q\\gamma^\\mu Q)"|>,
  "cQQ8" -> <|"Operator" -> "QQQ8", "OperatorLaTeX" -> "Q_{QQ}^{8}=(\\bar Q\\gamma_\\mu T^A Q)(\\bar Q\\gamma^\\mu T^A Q)"|>,

  "ctu1" -> <|"Operator" -> "Qtu1", "OperatorLaTeX" -> "Q_{tu}^{1}=(\\bar t\\gamma_\\mu t)(\\bar u\\gamma^\\mu u)"|>,
  "ctu8" -> <|"Operator" -> "Qtu8", "OperatorLaTeX" -> "Q_{tu}^{8}=(\\bar t\\gamma_\\mu T^A t)(\\bar u\\gamma^\\mu T^A u)"|>,
  "ctd1" -> <|"Operator" -> "Qtd1", "OperatorLaTeX" -> "Q_{td}^{1}=(\\bar t\\gamma_\\mu t)(\\bar d\\gamma^\\mu d)"|>,
  "ctd8" -> <|"Operator" -> "Qtd8", "OperatorLaTeX" -> "Q_{td}^{8}=(\\bar t\\gamma_\\mu T^A t)(\\bar d\\gamma^\\mu T^A d)"|>,
  "ctj1" -> <|"Operator" -> "Qtq1", "OperatorLaTeX" -> "Q_{tq}^{1}=(\\bar t\\gamma_\\mu t)(\\bar q\\gamma^\\mu q)"|>,
  "ctj8" -> <|"Operator" -> "Qtq8", "OperatorLaTeX" -> "Q_{tq}^{8}=(\\bar t\\gamma_\\mu T^A t)(\\bar q\\gamma^\\mu T^A q)"|>,
  "cQu1" -> <|"Operator" -> "QQu1", "OperatorLaTeX" -> "Q_{Qu}^{1}=(\\bar Q\\gamma_\\mu Q)(\\bar u\\gamma^\\mu u)"|>,
  "cQu8" -> <|"Operator" -> "QQu8", "OperatorLaTeX" -> "Q_{Qu}^{8}=(\\bar Q\\gamma_\\mu T^A Q)(\\bar u\\gamma^\\mu T^A u)"|>,
  "cQd1" -> <|"Operator" -> "QQd1", "OperatorLaTeX" -> "Q_{Qd}^{1}=(\\bar Q\\gamma_\\mu Q)(\\bar d\\gamma^\\mu d)"|>,
  "cQd8" -> <|"Operator" -> "QQd8", "OperatorLaTeX" -> "Q_{Qd}^{8}=(\\bar Q\\gamma_\\mu T^A Q)(\\bar d\\gamma^\\mu T^A d)"|>,
  "cQt1" -> <|"Operator" -> "QQt1", "OperatorLaTeX" -> "Q_{Qt}^{1}=(\\bar Q\\gamma_\\mu Q)(\\bar t\\gamma^\\mu t)"|>,
  "cQt8" -> <|"Operator" -> "QQt8", "OperatorLaTeX" -> "Q_{Qt}^{8}=(\\bar Q\\gamma_\\mu T^A Q)(\\bar t\\gamma^\\mu T^A t)"|>,
  "ctt" -> <|"Operator" -> "Qtt", "OperatorLaTeX" -> "Q_{tt}=(\\bar t\\gamma_\\mu t)(\\bar t\\gamma^\\mu t)"|>,
  "ctb1" -> <|"Operator" -> "Qtb1", "OperatorLaTeX" -> "Q_{tb}^{1}=(\\bar t\\gamma_\\mu t)(\\bar b\\gamma^\\mu b)"|>,
  "ctb8" -> <|"Operator" -> "Qtb8", "OperatorLaTeX" -> "Q_{tb}^{8}=(\\bar t\\gamma_\\mu T^A t)(\\bar b\\gamma^\\mu T^A b)"|>,
  "cQb1" -> <|"Operator" -> "QQb1", "OperatorLaTeX" -> "Q_{Qb}^{1}=(\\bar Q\\gamma_\\mu Q)(\\bar b\\gamma^\\mu b)"|>,
  "cQb8" -> <|"Operator" -> "QQb8", "OperatorLaTeX" -> "Q_{Qb}^{8}=(\\bar Q\\gamma_\\mu T^A Q)(\\bar b\\gamma^\\mu T^A b)"|>,
  "cbb" -> <|"Operator" -> "Qbb", "OperatorLaTeX" -> "Q_{bb}=(\\bar b\\gamma_\\mu b)(\\bar b\\gamma^\\mu b)"|>
|>;

operatorExactLookupFA[s_String] := Lookup[$SMEFTsimOperatorExactMap, StringTrim[s], Missing["NotFound"]];

operatorInfoForRowFA[row_Association] := Module[{keys, hits},
  keys = DeleteDuplicates @ Flatten @ {
      Lookup[row, "SMEFTsim", ""],
      Lookup[row, "TargetExpression", ""],
      Lookup[row, "Aliases", {}]
    };
  keys = DeleteCases[ToString /@ keys, ""];
  hits = Select[operatorExactLookupFA /@ keys, AssociationQ, 1];
  If[hits === {}, <|"Operator" -> "-", "OperatorLaTeX" -> "-"|>, First[hits]]
];

addOperatorColumnsFA[row_Association] := Module[{r, op},
  r = addLatexColumnsFA[row];
  op = operatorInfoForRowFA[row];
  Join[r, <|
      "Operator" -> Lookup[op, "Operator", "-"],
      "OperatorLaTeX" -> Lookup[op, "OperatorLaTeX", "-"]
    |>]
];

operatorTranslationsDataset[] := Dataset[addOperatorColumnsFA /@ $SMEFTsimWCTranslations][
  All,
  {"SMEFTsim", "SMEFTsimLaTeX", "Operator", "OperatorLaTeX", "Target", "TargetExpression", "TargetLaTeX"}
];

(* Override: from now on the standard LaTeX helpers also include operator columns. *)
translateFromSMEFTsimLatex[wc_, target_:All] := Module[{rows},
  rows = If[target === All, translateFromSMEFTsim[wc], translateFromSMEFTsim[wc, target]];
  addOperatorColumnsFA /@ rows
];

wcTranslationsDatasetLatex[] := operatorTranslationsDataset[];

translateFromSMEFTsimLatexTable[wc_, target_:All] := Dataset[translateFromSMEFTsimLatex[wc, target]][
  All,
  {"SMEFTsim", "SMEFTsimLaTeX", "Operator", "OperatorLaTeX", "Target", "TargetExpression", "TargetLaTeX"}
];

translateWCListLatexTable[wcs_List, target_:All] := Module[{names, rows},
  names = wcToString /@ wcs;
  rows = Flatten[translateFromSMEFTsimLatex[#, target] & /@ names, 1];
  Dataset[rows][All, {"SMEFTsim", "SMEFTsimLaTeX", "Operator", "OperatorLaTeX", "Target", "TargetExpression", "TargetLaTeX"}]
];

exportWCTranslationsLatexCSV[path_:Automatic] := Module[{out, rows},
  out = If[path === Automatic,
    FileNameJoin[{NotebookDirectory[], "SMEFTsim_WC_traducciones_UFO_OPERADORES.csv"}],
    path
  ];
  rows = Normal[addOperatorColumnsFA /@ $SMEFTsimWCTranslations] /. a_Association :> KeyDrop[a, {"Aliases", "Class", "Kind", "Notes"}];
  Export[out, rows, "CSV"];
  out
];

translationRowsWithLatexFA[rows_List] := addOperatorColumnsFA /@ rows;

translationRowsRenderedFA[rows_List, mode_:"Auto"] := Module[{rowsLatex},
  rowsLatex = translationRowsWithLatexFA[rows];
  ({
      Lookup[#, "SMEFTsim", "-"],
      renderWCNotationFA[Lookup[#, "SMEFTsimLaTeX", Lookup[#, "SMEFTsim", "-"]], mode],
      Lookup[#, "Operator", "-"],
      renderWCNotationFA[Lookup[#, "OperatorLaTeX", "-"], mode],
      Lookup[#, "Target", "-"],
      Lookup[#, "TargetExpression", "-"],
      renderTranslationTargetFA[#, mode]
    } & /@ rowsLatex)
];

translateFromSMEFTsimRenderedTable[wc_, target_:All, mode_:"Auto"] := Module[{rows, body},
  rows = If[target === All,
    translateFromSMEFTsim[wc],
    translateFromSMEFTsim[wc, target]
  ];

  If[rows === {},
    Return[
      Grid[
        {
          {Style["SMEFTsim", Bold], Style["Resultado", Bold]},
          {wcToString[wc], Style["Sin traduccion encontrada", Red]}
        },
        Frame -> All,
        Alignment -> Left,
        Spacings -> {1.2, 0.8}
      ]
    ]
  ];

  body = translationRowsRenderedFA[rows, mode];

  Grid[
    Prepend[
      body,
      Style[#, Bold] & /@ {
        "SMEFTsim",
        "SMEFTsim notacion",
        "Operador",
        "Operador notacion",
        "Destino",
        "Expresion destino",
        "Destino notacion"
      }
    ],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.2, 0.8},
    ItemSize -> All
  ]
];

translateWCListRenderedTable[wcs_List, target_:All, mode_:"Auto"] := Module[{names, rows, body},
  names = wcToString /@ wcs;

  rows = Flatten[
    If[target === All,
      translateFromSMEFTsim /@ names,
      translateFromSMEFTsim[#, target] & /@ names
    ],
    1
  ];

  If[rows === {},
    Return[
      Grid[
        {
          {Style["Resultado", Bold]},
          {Style["Sin traducciones encontradas", Red]}
        },
        Frame -> All,
        Alignment -> Left
      ]
    ]
  ];

  body = translationRowsRenderedFA[rows, mode];

  Grid[
    Prepend[
      body,
      Style[#, Bold] & /@ {
        "SMEFTsim",
        "SMEFTsim notacion",
        "Operador",
        "Operador notacion",
        "Destino",
        "Expresion destino",
        "Destino notacion"
      }
    ],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.2, 0.8},
    ItemSize -> All
  ]
];

translateFromSMEFTsimOperatorTable[wc_, target_:All, mode_:"Auto"] := translateFromSMEFTsimRenderedTable[wc, target, mode];
translateWCListOperatorTable[wcs_List, target_:All, mode_:"Auto"] := translateWCListRenderedTable[wcs, target, mode];

Print[
  "Operadores asociados cargados. Usa translateFromSMEFTsimRenderedTable[\"cHj1\"] ",
  "o translateWCListRenderedTable[lastWCNames]. Las columnas Operador y Operador notacion ya estan incluidas."
];

(* Aliases directos de nombres SMEFTsim general/indexados al mismo operador. *)
$SMEFTsimOperatorExactMap = Join[$SMEFTsimOperatorExactMap, <|
  "cuHRe33" -> Lookup[$SMEFTsimOperatorExactMap, "ctHRe"],
  "cuHIm33" -> Lookup[$SMEFTsimOperatorExactMap, "ctHIm"],
  "cuGRe33" -> Lookup[$SMEFTsimOperatorExactMap, "ctGRe"],
  "cuGIm33" -> Lookup[$SMEFTsimOperatorExactMap, "ctGIm"],
  "cuWRe33" -> Lookup[$SMEFTsimOperatorExactMap, "ctWRe"],
  "cHq1Re33" -> Lookup[$SMEFTsimOperatorExactMap, "cHQ1"],
  "cHq3Re33" -> Lookup[$SMEFTsimOperatorExactMap, "cHQ3"],
  "cHq3Re[rr]" -> Lookup[$SMEFTsimOperatorExactMap, "cHj3"],
  "cHl1[pp]" -> Lookup[$SMEFTsimOperatorExactMap, "cHl1"],
  "cHl3[pp]" -> Lookup[$SMEFTsimOperatorExactMap, "cHl3"],
  "cleQt1Im33" -> Lookup[$SMEFTsimOperatorExactMap, "cleQt1Re33"],
  "cleQt3Im33" -> Lookup[$SMEFTsimOperatorExactMap, "cleQt3Re33"],
  "clebQIm33" -> Lookup[$SMEFTsimOperatorExactMap, "clebQRe33"]
|>];

(* ========================================================= *)
(* 8. FIX: listas de WCs, orden de Lambda y bibliografia      *)
(* ========================================================= *)

ClearAll[
  normalizeWCListFA,
  lambdaInfoForRowFA,
  sourceInfoForRowFA,
  addFullInfoColumnsFA,
  translationRowsRenderedFullFA,
  renderTargetCellForWCFA,
  classKindNotesForWCFA,
  translateWCListDetailedTable,
  translateWCListSummaryTable,
  WCBibliografia,
  WCAyudaFunciones
];

normalizeWCListFA[wcs_List] := DeleteDuplicates[wcToString /@ Flatten[wcs]];
normalizeWCListFA[wc_] := {wcToString[wc]};

lambdaInfoForRowFA[row_Association] := Module[{status},
  status = Lookup[row, "Status", ""];
  If[status === "Not found",
    <|
      "OperatorDimension" -> "-",
      "LambdaOrder" -> "-",
      "ObservableOrder" -> "-"
    |>,
    <|
      "OperatorDimension" -> "6",
      "LambdaOrder" -> "\\Lambda^{-2}",
      "ObservableOrder" -> "lineal: \\Lambda^{-2}; cuadratico dim-6: \\Lambda^{-4} si se conserva C_i C_j"
    |>
  ]
];

sourceInfoForRowFA[row_Association] := Module[{target, kind},
  target = Lookup[row, "Target", ""];
  kind = Lookup[row, "Kind", ""];
  Which[
    Lookup[row, "Status", ""] === "Not found",
      <|"Reference" -> "-", "ReferenceKey" -> "-"|>,

    target === "SMEFT@NLO",
      <|"Reference" -> "SMEFTsim 3.0 practical guide, Appendix E.2, Tables 28-31", "ReferenceKey" -> "SMEFTsim3-AppE-SMEFTatNLO"|>,

    target === "dim6top",
      <|"Reference" -> "SMEFTsim 3.0 practical guide, Appendix E, Tables 24-25", "ReferenceKey" -> "SMEFTsim3-AppE-dim6top"|>,

    StringContainsQ[kind, "basis-rotation", IgnoreCase -> True],
      <|"Reference" -> "SMEFTsim 3.0 practical guide, Appendix E; basis-rotation table", "ReferenceKey" -> "SMEFTsim3-AppE-rotation"|>,

    True,
      <|"Reference" -> "SMEFTsim 3.0 practical guide and Warsaw-basis operator definitions", "ReferenceKey" -> "SMEFTsim3-Warsaw"|>
  ]
];

addFullInfoColumnsFA[row_Association] := Module[{r},
  r = addOperatorColumnsFA[row];
  Join[r, lambdaInfoForRowFA[row], sourceInfoForRowFA[row]]
];

translationRowsRenderedFullFA[rows_List, mode_:"Auto"] := Module[{rowsFull},
  rowsFull = addFullInfoColumnsFA /@ rows;
  ({
      Lookup[#, "SMEFTsim", "-"],
      renderWCNotationFA[Lookup[#, "SMEFTsimLaTeX", Lookup[#, "SMEFTsim", "-"]], mode],
      Lookup[#, "Operator", "-"],
      renderWCNotationFA[Lookup[#, "OperatorLaTeX", "-"], mode],
      Lookup[#, "OperatorDimension", "-"],
      renderWCNotationFA[Lookup[#, "LambdaOrder", "-"], mode],
      Lookup[#, "Target", "-"],
      Lookup[#, "TargetExpression", "-"],
      renderTranslationTargetFA[#, mode],
      Lookup[#, "Reference", "-"]
    } & /@ rowsFull)
];

translateWCListDetailedTable[wcs_List, target_:All, mode_:"Auto"] := Module[{names, rows, body},
  names = normalizeWCListFA[wcs];
  rows = Flatten[
    If[target === All,
      translateFromSMEFTsim /@ names,
      translateFromSMEFTsim[#, target] & /@ names
    ],
    1
  ];

  If[rows === {},
    Return[Grid[{{Style["Resultado", Bold]}, {Style["Sin traducciones encontradas", Red]}}, Frame -> All]]
  ];

  body = translationRowsRenderedFullFA[rows, mode];

  Grid[
    Prepend[
      body,
      Style[#, Bold] & /@ {
        "SMEFTsim",
        "SMEFTsim notacion",
        "Operador",
        "Operador notacion",
        "Dim.",
        "Orden Lambda",
        "Destino",
        "Expresion destino",
        "Destino notacion",
        "Referencia"
      }
    ],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.1, 0.7},
    ItemSize -> All
  ]
];

renderTargetCellForWCFA[wc_String, target_String, mode_:"Auto"] := Module[{rows, goodRows, rendered},
  rows = translateFromSMEFTsim[wc, target];
  goodRows = Select[rows, ! KeyExistsQ[#, "Status"] &];

  If[goodRows === {}, Return[Style["-", Gray]]];

  rendered = DeleteDuplicates[renderTranslationTargetFA[#, mode] & /@ (addLatexColumnsFA /@ goodRows)];
  If[Length[rendered] == 1, First[rendered], Column[rendered]]
];

(* Metadata is retained internally for lookup, filtering and bibliographic provenance; it is not shown in public tables or exports. *)
classKindNotesForWCFA[wc_String, target_:All] := Module[{rows, goodRows},
  rows = If[target === All,
    Flatten[translateFromSMEFTsim /@ {wc}, 1],
    translateFromSMEFTsim[wc, target]
  ];
  goodRows = Select[rows, ! KeyExistsQ[#, "Status"] &];
  If[goodRows === {},
    <|"Class" -> "-", "Kind" -> "-", "Reference" -> "-", "Notes" -> "-"|>,
    <|
      "Class" -> StringRiffle[DeleteDuplicates[Lookup[goodRows, "Class", "-"]], "; "],
      "Kind" -> StringRiffle[DeleteDuplicates[Lookup[goodRows, "Kind", "-"]], "; "],
      "Reference" -> StringRiffle[DeleteDuplicates[Lookup[sourceInfoForRowFA /@ goodRows, "Reference", "-"]], "; "],
      "Notes" -> StringRiffle[DeleteDuplicates[Lookup[goodRows, "Notes", "-"]], " | "]
    |>
  ]
];

translateWCListSummaryTable[wcs_List, target_:All, mode_:"Auto"] := Module[
  {names, targetList, rows, baseRows, base, full, aux, body, header},

  names = normalizeWCListFA[wcs];
  targetList = If[target === All,
    {"SMEFT@NLO", "dim6top"},
    {ToString[target]}
  ];

  body = Table[
    rows = Flatten[translateFromSMEFTsim /@ {name}, 1];
    baseRows = Select[rows, ! KeyExistsQ[#, "Status"] &];
    base = If[baseRows === {}, First[rows], First[baseRows]];
    full = addFullInfoColumnsFA[base];
    aux = classKindNotesForWCFA[name, target];

    Join[
      {
        name,
        renderWCNotationFA[Lookup[full, "SMEFTsimLaTeX", name], mode],
        Lookup[full, "Operator", "-"],
        renderWCNotationFA[Lookup[full, "OperatorLaTeX", "-"], mode],
        Lookup[full, "OperatorDimension", "-"],
        renderWCNotationFA[Lookup[full, "LambdaOrder", "-"], mode]
      },
      renderTargetCellForWCFA[name, #, mode] & /@ targetList,
      {
        Lookup[aux, "Reference", "-"]
      }
    ],
    {name, names}
  ];

  header = Join[
    {
      "SMEFTsim",
      "SMEFTsim notacion",
      "Operador",
      "Operador notacion",
      "Dim.",
      "Orden Lambda"
    },
    targetList,
    {
      "Referencia"
    }
  ];

  Grid[
    Prepend[body, Style[#, Bold] & /@ header],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.1, 0.7},
    ItemSize -> All
  ]
];

(* IMPORTANTE: si se llama a translateFromSMEFTsimRenderedTable con una lista,
   ya no se trata la lista completa como si fuera un unico WC. *)
translateFromSMEFTsimRenderedTable[wcs_List, target_:All, mode_:"Auto"] :=
  translateWCListSummaryTable[wcs, target, mode];

(* Tabla detallada: una fila por cada par WC-destino. *)
translateWCListRenderedTable[wcs_List, target_:All, mode_:"Auto"] :=
  translateWCListDetailedTable[wcs, target, mode];

translateWCListOperatorTable[wcs_List, target_:All, mode_:"Auto"] :=
  translateWCListDetailedTable[wcs, target, mode];

translateLastWCResultRendered[target_:All, mode_:"Auto"] := Module[{names},
  names = Quiet @ Check[lastWCNames, {}];
  If[names === {} || ! ListQ[names],
    Return[Grid[{{Style["Resultado", Bold]}, {Style["No existe lastWCNames o esta vacio. Ejecuta primero la interfaz y guarda lastWCNames.", Red]}}, Frame -> All, Alignment -> Left]]
  ];
  translateWCListSummaryTable[names, target, mode]
];

WCBibliografia[] := Dataset[{
  <|
    "Clave" -> "SMEFTsim3",
    "Referencia" -> "I. Brivio, SMEFTsim 3.0 - a practical guide, arXiv:2012.11343v3",
    "Uso" -> "Base Warsaw completa de operadores de dimension 6, clases L6^(n), convencion L6 = 1/Lambda^2 Sum C_i Q_i, parametros del codigo y traducciones entre UFOs; para SMEFT@NLO se usan especificamente las Tablas 28-31 del Apendice E.2.",
    "Partes usadas" -> "Sec. 1.1; App. D; App. E; Tables 24-31"
  |>,
  <|
    "Clave" -> "WarsawBasis",
    "Referencia" -> "Warsaw basis operator definitions as implemented in SMEFTsim and used by SMEFT@NLO/dim6top comparisons.",
    "Uso" -> "Definicion simbolica de operadores Q_i asociados a cada WC.",
    "Partes usadas" -> "SMEFTsim operator tables and Warsaw-basis definitions"
  |>
}];

WCAyudaFunciones[] := Dataset[{
  <|"Funcion" -> "translateFromSMEFTsimRenderedTable[wc]", "Uso" -> "Un WC individual: tabla renderizada con traducciones, operador, orden Lambda y referencias."|>,
  <|"Funcion" -> "translateFromSMEFTsimRenderedTable[wcList]", "Uso" -> "Lista de WCs: resumen, una fila por WC."|>,
  <|"Funcion" -> "translateWCListSummaryTable[wcList]", "Uso" -> "Resumen: una fila por WC y columnas SMEFT@NLO y dim6top."|>,
  <|"Funcion" -> "translateWCListRenderedTable[wcList]", "Uso" -> "Detalle: una fila por cada par WC-destino."|>,
  <|"Funcion" -> "translateWCListRenderedTable[wcList, \"SMEFT@NLO\"]", "Uso" -> "Detalle filtrado a un destino concreto."|>,
  <|"Funcion" -> "wcCoverageReport[wcList]", "Uso" -> "Informe de cobertura: traducidos y faltantes."|>,
  <|"Funcion" -> "untranslatedWCs[wcList]", "Uso" -> "Lista de WCs sin traduccion."|>,
  <|"Funcion" -> "exportWCTranslationsTeX[wcList, \"tabla.tex\"]", "Uso" -> "Exporta un documento .tex compilable con tabla resumen. Es independiente de la interfaz."|>,
  <|"Funcion" -> "exportWCTranslationsTeX[wcList, \"tabla.tex\", \"TableType\" -> \"Both\"]", "Uso" -> "Exporta las tablas resumen y detallada en el mismo .tex."|>,
  <|"Funcion" -> "WCBibliografia[]", "Uso" -> "Bibliografia usada para traducciones, operadores y ordenes de Lambda."|>
}];

Print[
  "Traducciones SMEFTsim-UFO cargadas. Usa translateWCListSummaryTable[wcList] para una fila por WC, ",
  "translateWCListRenderedTable[wcList] para detalle WC-destino, y WCBibliografia[]."
];
