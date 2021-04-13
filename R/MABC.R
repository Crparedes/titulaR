#' Buyoyancy ...
#'
#' @export

MABC <- function(rho_obj = 0.998, rho_w = 8, rho_air = airDensity(20, 1013.25, 0)) {
  MABC <- (1 - rho_air/rho_w) / (1 - rho_air/rho_obj)
  return(MABC)
}

# MABC() # Agua MichalMariassy: 1.001025
# MABC(rho_obj = 7.133) # Zinc MichalMariassy: 1.000017
# MABC(rho_obj = 8.96) # Cobre SergioRezzonico: 0.9999856
# MABC(rho_obj = 4.53) # nitrato de plomo
