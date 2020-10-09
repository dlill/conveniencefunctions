.COMPLIANCE_MODE <- TRUE
.PATH_SYSTEM_MONOLIX <- list(
    MLX2019R1 = '/opt/Lixoft/MonolixSuite2019R1'
)
.MODEL_MAX_DESIRED_STATENAME_LENGTH      <- 200
.MODEL_MAX_DESIRED_PARAMETERNAME_LENGTH  <- 200
.MODEL_MAX_DESIRED_VARIABLENAME_LENGTH   <- 200
.MODEL_MAX_DESIRED_REACTIONNAME_LENGTH   <- 200

.RESERVED_WORD_IQRMODELS                 <- c(
  "T","F","PK","G","H","gt","ge","lt","le","mod","and","or",
  "piecewise","interp0","interp1","interpcs", "default", "F1", "F2", "Tlag", "eps", "eta", "theta", "sigma",
  "a","b","b1","b2","b3","d","Intercept",
  "time", "y", "ydot", "RPAR", "IPAR"
)
