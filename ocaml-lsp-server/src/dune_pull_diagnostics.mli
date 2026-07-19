open Import

val identifier : string

val document
  :  Diagnostic.t list
  -> DocumentDiagnosticParams.t
  -> DocumentDiagnosticReport.t
