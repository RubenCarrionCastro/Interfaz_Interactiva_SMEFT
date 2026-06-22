(* ::Package:: *)
(* SMEFTsim_WC_traducciones_SMEFTNLO_BIBLIO_REVISADA.wl
   Tabla auxiliar de traduccion de coeficientes de Wilson entre:
   - SMEFTsim top / topU3l / general
   - SMEFT@NLO
   - dim6top

   Archivo ASCII para evitar problemas de codificacion en Windows.
   No modifica ninguna interfaz. Solo define datos y funciones auxiliares.
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

wcTranslationsDataset[] := Dataset[$SMEFTsimWCTranslations];

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
    Normal[$SMEFTsimWCTranslations] /. a_Association :> KeyDrop[a, "Aliases"],
    "CSV"
  ];
  out
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
(* Tables 28-31. Estas reglas solo corrigen la visualizacion *)
(* LaTeX/MaTeX de nombres SMEFT@NLO; no cambian la tabla     *)
(* algebraica de traduccion.                                 *)
(* --------------------------------------------------------- *)
$SMEFTsimLatexExactMap = Join[$SMEFTsimLatexExactMap, <|
  (* SMEFT@NLO: bosonic and Higgs-sector parameters, Tables 28 and 30 *)
  "-gs*cG" -> "-g_s\,c_G",
  "-cWWW" -> "-c_{WWW}",
  "cp" -> "c_{\varphi}",
  "cdp" -> "c_{\varphi\Box}",
  "cpDC" -> "c_{\varphi D}",
  "cpG" -> "c_{\varphi G}",
  "cpW" -> "c_{\varphi W}",
  "cpBB" -> "c_{\varphi B}",
  "cpWB" -> "c_{\varphi WB}",

  (* SMEFT@NLO: top Yukawa and dipoles, Tables 28-31 *)
  "ctp" -> "c_{t\varphi}",
  "-gs*ctG" -> "-g_s\,c_{tG}",
  "-ctW" -> "-c_{tW}",
  "ctZ" -> "c_{tZ}",
  "ctZ/sTheta - ctW/tTheta" -> "\frac{c_{tZ}}{s_\theta}-\frac{c_{tW}}{t_\theta}",

  (* SMEFT@NLO: heavy/light quark EW-current rotations, Table 29 *)
  "cpQ3" -> "c_{\varphi Q}^{(3)}",
  "cpQM" -> "c_{\varphi Q}^{-}",
  "cpQ3 + cpQM" -> "c_{\varphi Q}^{(3)}+c_{\varphi Q}^{-}",
  "cpq3i" -> "c_{\varphi q}^{(3),i}",
  "cpqMi" -> "c_{\varphi q}^{-,i}",
  "cpq3i + cpqMi" -> "c_{\varphi q}^{(3),i}+c_{\varphi q}^{-,i}",
  "cpt" -> "c_{\varphi t}",
  "cpu" -> "c_{\varphi u}",
  "cpd" -> "c_{\varphi d}",

  (* SMEFT@NLO: leptonic current parameters, Table 28 *)
  "cpe" -> "c_{\varphi e}",
  "cpmu" -> "c_{\varphi \mu}",
  "cpta" -> "c_{\varphi \tau}",
  "cpe = cpmu = cpta" -> "c_{\varphi e}=c_{\varphi \mu}=c_{\varphi \tau}",
  "cpl[p]" -> "c_{\varphi l}^{(1),p}",
  "c3pl[p]" -> "c_{\varphi l}^{(3),p}",

  (* SMEFT@NLO: four-lepton and semileptonic topU3l entries, Tables 28-29 *)
  "cll[pppp] = cll[pprr]" -> "c_{ll}^{pppp}=c_{ll}^{pprr}",
  "cll[pppp] = cll[prrp]" -> "c_{ll}^{pppp}=c_{ll}^{prrp}",
  "cte[p]" -> "c_{te}^{p}",
  "cQe[p]" -> "c_{Qe}^{p}",
  "ctl[p]" -> "c_{lt}^{p}",
  "cQl3[p]" -> "c_{lQ}^{(3),p}",
  "cQlM[p]" -> "c_{lQ}^{-,p}",
  "cQl3[p] + cQlM[p]" -> "c_{lQ}^{(3),p}+c_{lQ}^{-,p}",

  (* SMEFT@NLO: scalar/tensor entries with explicit lepton Yukawa, Table 28 *)
  "yl[3]*ctlS3" -> "y_l^{3}\,c_{lt}^{S,3}",
  "yl[3]*ctlT3" -> "y_l^{3}\,c_{lt}^{T,3}",
  "yl[3]*cblS3" -> "y_l^{3}\,c_{bl}^{S,3}",

  (* SMEFT@NLO: normalization factors and common four-quark names, Table 28 *)
  "1/2*cQQ1" -> "\frac{1}{2}c_{QQ}^{1}",
  "1/2*cQQ8" -> "\frac{1}{2}c_{QQ}^{8}",
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
  "ctlS3" -> "c_{lt}^{S,3}",
  "ctlT3" -> "c_{lt}^{T,3}",
  "cblS3" -> "c_{bl}^{S,3}"
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
  {"SMEFTsim", "SMEFTsimLaTeX", "Target", "TargetExpression", "TargetLaTeX", "Class", "Kind", "Notes"}
];

translateFromSMEFTsimLatex[wc_, target_:All] := Module[{rows},
  rows = If[target === All, translateFromSMEFTsim[wc], translateFromSMEFTsim[wc, target]];
  addLatexColumnsFA /@ rows
];

translateFromSMEFTsimLatexTable[wc_, target_:All] := Dataset[translateFromSMEFTsimLatex[wc, target]][
  All,
  {"SMEFTsim", "SMEFTsimLaTeX", "Target", "TargetExpression", "TargetLaTeX", "Class", "Kind", "Notes"}
];

translateWCListLatexTable[wcs_List, target_:All] := Module[{names, rows},
  names = wcToString /@ wcs;
  rows = Flatten[translateFromSMEFTsimLatex[#, target] & /@ names, 1];
  Dataset[rows][All, {"SMEFTsim", "SMEFTsimLaTeX", "Target", "TargetExpression", "TargetLaTeX", "Class", "Kind", "Notes"}]
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
  topLevelSplitFA,
  nativeLatexAtomFA,
  nativeLatexProductFA,
  nativeLatexSumFA,
  nativeLatexFormulaFA,
  renderWCNotationFA,
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
   "\\Box" -> "\[Square]",
   "\\mathrm{Im}" -> "Im",
   "\\mathrm{Re}" -> "Re",
   "\\dagger" -> "\[Dagger]",
   "\\bar" -> "bar",
   "\\," -> "",
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
  {t, m, base, sub, sup, expr},

  t = StringTrim[s];
  If[t === "" || t === "-", Return["-"]];

  (* Coeficientes tipo C_{\varphi q}^{(1)}, c_{tZ}, c_{\varphi q}^{3,i}, etc. *)
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

  (* Simbolos con subindice simple tipo c_W, s_W, g_s. *)
  m = StringCases[t,
    RegularExpression["^([A-Za-z]+)_([A-Za-z0-9]+)$"] :>
      {"$1", "$2"}
  ];

  If[m =!= {},
    {base, sub} = First[m];
    Return[TraditionalForm[Subscript[Style[base, Italic], Style[nativeLatexSymbolStringFA[sub], Italic]]]];
  ];

  (* Superindice simple tipo x^{Im}. *)
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

translationRowsWithLatexFA[rows_List] := addLatexColumnsFA /@ rows;

translationRowsRenderedFA[rows_List, mode_:"Auto"] := Module[{rowsLatex},
  rowsLatex = translationRowsWithLatexFA[rows];
  ({
      Lookup[#, "SMEFTsim", "-"],
      renderWCNotationFA[Lookup[#, "SMEFTsimLaTeX", Lookup[#, "SMEFTsim", "-"]], mode],
      Lookup[#, "Target", "-"],
      Lookup[#, "TargetExpression", "-"],
      renderWCNotationFA[Lookup[#, "TargetLaTeX", Lookup[#, "TargetExpression", "-"]], mode],
      Lookup[#, "Class", "-"],
      Lookup[#, "Kind", "-"],
      Lookup[#, "Notes", "-"]
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
        "Destino notacion",
        "Clase",
        "Tipo",
        "Notas"
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
        "Destino notacion",
        "Clase",
        "Tipo",
        "Notas"
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
  {"SMEFTsim", "SMEFTsimLaTeX", "Operator", "OperatorLaTeX", "Target", "TargetExpression", "TargetLaTeX", "Class", "Kind", "Notes"}
];

(* Override: from now on the standard LaTeX helpers also include operator columns. *)
translateFromSMEFTsimLatex[wc_, target_:All] := Module[{rows},
  rows = If[target === All, translateFromSMEFTsim[wc], translateFromSMEFTsim[wc, target]];
  addOperatorColumnsFA /@ rows
];

wcTranslationsDatasetLatex[] := operatorTranslationsDataset[];

translateFromSMEFTsimLatexTable[wc_, target_:All] := Dataset[translateFromSMEFTsimLatex[wc, target]][
  All,
  {"SMEFTsim", "SMEFTsimLaTeX", "Operator", "OperatorLaTeX", "Target", "TargetExpression", "TargetLaTeX", "Class", "Kind", "Notes"}
];

translateWCListLatexTable[wcs_List, target_:All] := Module[{names, rows},
  names = wcToString /@ wcs;
  rows = Flatten[translateFromSMEFTsimLatex[#, target] & /@ names, 1];
  Dataset[rows][All, {"SMEFTsim", "SMEFTsimLaTeX", "Operator", "OperatorLaTeX", "Target", "TargetExpression", "TargetLaTeX", "Class", "Kind", "Notes"}]
];

exportWCTranslationsLatexCSV[path_:Automatic] := Module[{out, rows},
  out = If[path === Automatic,
    FileNameJoin[{NotebookDirectory[], "SMEFTsim_WC_traducciones_UFO_OPERADORES.csv"}],
    path
  ];
  rows = Normal[addOperatorColumnsFA /@ $SMEFTsimWCTranslations] /. a_Association :> KeyDrop[a, "Aliases"];
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
      renderWCNotationFA[Lookup[#, "TargetLaTeX", Lookup[#, "TargetExpression", "-"]], mode],
      Lookup[#, "Class", "-"],
      Lookup[#, "Kind", "-"],
      Lookup[#, "Notes", "-"]
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
        "Destino notacion",
        "Clase",
        "Tipo",
        "Notas"
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
        "Destino notacion",
        "Clase",
        "Tipo",
        "Notas"
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
      renderWCNotationFA[Lookup[#, "TargetLaTeX", Lookup[#, "TargetExpression", "-"]], mode],
      Lookup[#, "Class", "-"],
      Lookup[#, "Kind", "-"],
      Lookup[#, "Reference", "-"],
      Lookup[#, "Notes", "-"]
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
        "Clase",
        "Tipo",
        "Referencia",
        "Notas"
      }
    ],
    Frame -> All,
    Alignment -> Left,
    Spacings -> {1.1, 0.7},
    ItemSize -> All
  ]
];

renderTargetCellForWCFA[wc_String, target_String, mode_:"Auto"] := Module[{rows, goodRows, exprs, latexes},
  rows = translateFromSMEFTsim[wc, target];
  goodRows = Select[rows, ! KeyExistsQ[#, "Status"] &];

  If[goodRows === {}, Return[Style["-", Gray]]];

  exprs = DeleteDuplicates[Lookup[addLatexColumnsFA /@ goodRows, "TargetExpression", "-"]];
  latexes = DeleteDuplicates[Lookup[addLatexColumnsFA /@ goodRows, "TargetLaTeX", "-"]];

  If[Length[latexes] == 1,
    renderWCNotationFA[First[latexes], mode],
    Column[renderWCNotationFA[#, mode] & /@ latexes]
  ]
];

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
        Lookup[aux, "Class", "-"],
        Lookup[aux, "Kind", "-"],
        Lookup[aux, "Reference", "-"],
        Lookup[aux, "Notes", "-"]
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
      "Clase",
      "Tipo",
      "Referencia",
      "Notas"
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
  <|"Funcion" -> "WCBibliografia[]", "Uso" -> "Bibliografia usada para traducciones, operadores y ordenes de Lambda."|>
}];

Print[
  "Traducciones SMEFTsim-UFO cargadas. Usa translateWCListSummaryTable[wcList] para una fila por WC, ",
  "translateWCListRenderedTable[wcList] para detalle WC-destino, y WCBibliografia[]."
];
