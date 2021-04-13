#' Air density in [g cm^-3]
#'
#' Calculates the density of the air in the laboratory according to two models that use
#' temperature, barometric pressure and relative humidity.
#'
#' @param Temp temperature
#'
#' @return air density value
#'
#' @examples
#' airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#' airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%'), opt = 'A') # [g/cm^3]
#'
#' @export

# https://www.nist.gov/system/files/documents/2019/05/13/sop-2-applying-air-buoyancy-20190506.pdf
airDensity <- function(Temp, p, h, units = c('deg.C', 'hPa', '%'), opt = 'B') { # [g/cm^3]
  if (!(opt %in% c('A', 'B'))) stop("Option parameter must be 'A' or 'B'. See details.")
  if (!(units[1] %in% c('deg.C', 'K'))) stop("Temperature units must be 'deg.C' or 'K'.")
  if (!(units[2] %in% c('Pa', 'hPa', 'kPa', 'mmHg'))) stop("Pressure units must be 'Pa', 'hPa', 'kPa' or 'mmHg'.")
  if (!(units[3] %in% c('%', 'ND'))) stop("Relative humidity must be '%' or 'Frac' (the latter for values between 0 and 1).")

  if (units[1] == 'deg.C') Temp <- Temp + 273.15

  if (opt == 'A') {
    if (units[2] == 'Pa') p  <- p * 0.007500615758456563
    if (units[2] == 'hPa') p  <- p * 100 * 0.007500615758456563
    if (units[2] == 'kPa') p  <- p * 1000 * 0.007500615758456563
    if (units[3] == 'Frac') h <- h*100
    if (h < 0 || h > 100) stop("Relative humidity must be between 0 and 1 (or 0%-100%).")
    e_s <- 1.3146e9 * exp(-5315.56/Temp)
    rho_air_exp <- expression(((0.46460 * (p - 0.0037960 * h * e_s))/Temp)*10^-3)
    rho_air <- eval(rho_air_exp)
  } else {
    if (units[2] == 'hPa') p  <- p * 100
    if (units[2] == 'kPa') p  <- p * 1000
    if (units[2] == 'mmHg') p  <- p * 133.322387415
    if (units[3] == '%') h <- h/100
    if (h < 0 || h > 1) stop("Relative humidity must be between 0 and 1 (or 0%-100%).")

    rho_air_exp <- expression(((p*M_a)/(Z*R*Temp)) * (1 - x_v*(1 - M_v/M_a)))
    x_v_exp <- expression(h * f * p_sv / p)
    Z_exp <- expression(1 - ((p/Temp)*(a0 + a1*t + a2*t^2 + (b0 + b1*t)*x_v + (c0 + c1*t)*x_v^2)) + ((p^2/Temp^2)*(d + e*x_v^2)))

    M_a <- 28.96546e-3 # [kg / mol] molar mass of the air within laboratory
    M_v <- 18.01528e-3 # [kg / mol] $/pm$ 0.00017e-3
    # p [Pa], ambient barometric pressure
    # Temp [K], ambient temperature
    R <- 8.314472 # [J / (mol K)]  $/pm$ 0.000015 universal gas constant
    # h [], relative humidity
    t <- Temp - 273.15 # [deg.C], ambient temperature
    f <- 1.00062 + 3.14e-8*p + 5.6e-7*t^2 # ??

    A <- 1.2378847e-5 # [K^-2]
    B <- -1.9121316e-2 # [K^-1]
    C <- 33.93711047 # []
    D <- -6.3431645e3 # [K]
    p_sv <- exp(A*T^2 + B*T + C + D/T) # [Pa]

    a0 <- 1.58123e-6 # [K Pa^-1]
    a1 <- -2.9331e-8 # [Pa^-1]
    a2 <- 1.1043e-10 # [K^-1 Pa^-1]
    b0 <- 5.707e-6 # [K Pa^-1]
    b1 <- -2.051e-8 # [Pa^-1]
    c0 <- 1.9898e-4 # [K Pa^-1]
    c1 <- -2.376e-6 # [Pa^-1]
    d <- 1.83e-11 # [K^2 Pa^-2]
    e <- -0.765e-8 # [K^2 Pa^-2]

    x_v <- eval(x_v_exp)
    Z <- eval(Z_exp)
    rho_air <- eval(rho_air_exp) * 10^-3
  }
  return(rho_air)
}

# Example 1 Appendix B
#airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#airDensity(Temp = 22.3, p = 748.1, h = 37, units = c('deg.C', 'mmHg', '%'), opt = 'A') # [g/cm^3]

# Example 3 Appendix B
#airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%')) # [g/cm^3]
#airDensity(Temp = 23.4, p = 612.3, h = 23, units = c('deg.C', 'mmHg', '%'), opt = 'A') # [g/cm^3]
