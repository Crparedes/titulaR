choicesConcentrationUnits <- list(
  "Fracción másica [g/g]" = 1,#set_units(x = 0.01, g/g),
  "Fracción másica [%]" = 2,#set_units(x = 0.01, g/g),
               "[g/kg]" = 3,#set_units(x = 1, g/kg),
               "[mg/kg]" = 4,#set_units(x = 1, mg/kg),
               "[ug/kg]" = 5,#set_units(x = 1, ug/kg),
               "[ng/kg]" = 6,#set_units(x = 1, ng/kg),
               "Fracción volumétrica [mL/mL]" = 7,#set_units(x = 1, mL/mL),
               "fracción volumétrica [%]" = 8,#set_units(x = 0.01, mL/mL),
               "[g/mL]" = 9,#set_units(x = 1, g/mL),
               "[g/L]" = 10,#set_units(x = 1, g/L),
               "[mg/L]" = 11,#set_units(x = 1, mg/L),
               "[ug/L]" = 12,#set_units(x = 1, ug/L),
               "[ng/L]" = 13#set_units(x = 1, ng/L))),
)

choosenConcentrationUnits <- list(
  set_units(x = 1, g/g), 
  set_units(x = 0.01, g/g),
  set_units(x = 1, g/kg),
  set_units(x = 1, mg/kg),
  set_units(x = 1, ug/kg),
  set_units(x = 1, ng/kg),
  set_units(x = 1, g/g),
  set_units(x = 0.01, g/g),
  set_units(x = 1, g/g),
  set_units(x = 1, g/kg),
  set_units(x = 1, mg/kg),
  set_units(x = 1, ug/kg),
  set_units(x = 1, ng/kg)
  )

OrigChoosenConcentrationUnits <- list(
  set_units(x = 1, g/g), 
  set_units(x = 0.01, g/g),
  set_units(x = 1, g/kg),
  set_units(x = 1, mg/kg),
  set_units(x = 1, ug/kg),
  set_units(x = 1, ng/kg),
  set_units(x = 1, mL/mL),
  set_units(x = 0.01, mL/mL),
  set_units(x = 1, g/mL),
  set_units(x = 1, g/L),
  set_units(x = 1, mg/L),
  set_units(x = 1, ug/L),
  set_units(x = 1, ng/L)
)