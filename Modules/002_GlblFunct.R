HorRatFun <- function (x) 2 ^(1 - 0.5 * log10(x))

is.null.empty <- function(x) {
  if (is.null(x)) {
    print(TRUE)
  } else {
    if (length(x) == 0 || any(is.na(unlist(x))) || any(unlist(x) == '')) {
      print(TRUE)
    } else {
      print(FALSE)
    }
  }
}
are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}

list.are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (are.null.empty(x[[i]])) {return(TRUE)}}
  return(FALSE)
}


niceRound <- function(x, n) {format(round(c(x, DummyNumber), digits = max(0, n)))[1]}

niceSeparator <- function(){
  return(tags$div('', tags$br(), tags$br(), 
                  tags$hr(style = paste0("border-top: 5px solid", BackHeaderButtons, ";"))))}

EnsureMinValue <- function(x, mini) {return(max(na.omit(c(x, min))))}
EnsureRangeValue <- function(x, mini = 3, maxi = 50) {
  if (is.na(x)) return(mini)
  if (x < mini) return(mini)
  if (x > maxi) return(maxi)
  return(x)
}


NoHayDatos <- function(n = NULL, AddInfo = NULL){
  if (is.null(n)) {
    return(h4(tags$hr(), tags$hr(), tags$b('No se encontraron datos en validaR.'), tags$br(), h5(MensEstadis), tags$hr(), tags$hr()))
  } else {
    return(h4(tags$hr(), tags$hr(), 
              tags$b('Esta herramienta requiere al menos', n, 'series de datos.'), tags$br(),
              ifelse(is.null(AddInfo), '', AddInfo), tags$br(),
              h5('Diríjase al submodulo ', tags$u(icon('clone'), tags$b('Inicio, ingreso de datos')), 'y complete las columnas que le faltan.'),
              tags$hr(), tags$hr()))
  }
}

is.null.empty <- function(x) {if (is.null(x)) {return(TRUE)} else {if (length(x) == 0 || x == '' || any(is.na(unlist(x)))) {return(TRUE)} else {return(FALSE)}}}
are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}

# Getting troubles using hot_to_r(), The following is a workaround
HOT2R <- function(x, IgnoreRowNames = FALSE) {
  if (is.null(x)) return(NA)
  rows <- length(x$data)
  cols <- length(x$data[[1]])
  extracted <- data.frame(matrix(nrow = rows, ncol = cols))
  for (i in 1:rows) {
    for (j in 1:cols) {
      extracted[i, j] <- ifelse(is.null(x$data[[i]][[j]]), NA, x$data[[i]][[j]])
    }
  }
  colnames(extracted) <- unlist(x$params$colHeaders)
  if (!IgnoreRowNames) rownames(extracted) <- unlist(x$params$rowHeaders)
  return(extracted)
}

# This is a uglier, more robust workaround... 
# There may be a trouble with the rhandsontable renderizer, Only 4 digital places are taken
HTMELIZAME.Esta.Mass <- function(mat, dVal, OrgUnits, UnitsTable) {
  htmlizado <- '<ol>'
  for (i in 1:nrow(mat)) {
    a1 <- format(round(c(mat[i, 1], DummyNumber), digits = abs(floor(log10(convertMassUnitsSI(dVal, from = OrgUnits, to = UnitsTable))))))[1]
    a2 <- format(round(c(mat[i, 2], DummyNumber), digits = abs(floor(log10(convertMassUnitsSI(dVal, from = OrgUnits, to = UnitsTable)) - 1))))[1]
    htmlizado <- c(htmlizado, '<li>', spcs(5), '(', a1, '&nbsp;&#177;&nbsp;', 
                   format(a2, scientific = FALSE), ') ', spcs(3), UnitsTable, '</li>')
  }
  htmlizado <- c(htmlizado, '</ol>')
  return(htmlizado)
}

HTMELIZAME.Esta.MABC <- function(mat) {
  htmlizado <- '<ol>'
  for (i in 1:nrow(mat)) {
    a1 <- format(round(c(mat[i, 1], DummyNumber), digits = 7))[1]
    a2 <- format(signif(c(mat[i, 2], DummyNumber), digits = 2))[1]
    htmlizado <- c(htmlizado, '<li>', spcs(5), '(', a1, '&nbsp;&#177;&nbsp;', 
                   format(a2, scientific = FALSE), ') ', '</li>')
  }
  htmlizado <- c(htmlizado, '</ol>')
  return(htmlizado)
}


niceSeparator <- function(){return(tags$hr(style = "border-top: 5px solid #2c3e50;"))}


EnsureMinValue <- function(x, min) {return(max(na.omit(c(x, min))))}

SummarizeRepInput <- function(x) {
  return(as.data.frame(t(apply(x, 1, function(xi) return(c(mean(xi), sd(xi)))))))
}

SummarizeEccenInput <- function(x) {
  return(c(mean(x[[1]]), max(abs(x[[1]][1] - x[[1]][2:5]))))
}



# Taken from https://www.rdocumentation.org/packages/berryFunctions/versions/1.21.14
is.error <- function (expr, tell = FALSE, force = FALSE) { 
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent = TRUE)
  iserror <- inherits(test, "try-error")
  if (tell) 
    if (iserror) 
      message("Note in is.error: ", test)
  if (force) 
    if (!iserror) 
      stop(expr_name, " is not returning an error.", call. = FALSE)
  iserror
}