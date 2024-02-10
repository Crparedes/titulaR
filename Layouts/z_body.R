# i18n <- Translator$new(translation_json_path = "translation.json")
# i18n$set_translation_language("es") # here you select the default translation to display

customBody <- dashboardBody(
  useShinyjs(), 
  withMathJax(),
  tags$head(tags$script(
  'var width = 1;
  $(document).on("shiny:connected", function(e) {
    width = window.innerWidth/2100;
    document.body.style.zoom = Math.sqrt(width);
  });')),
  extendShinyjs(
    text = "shinyjs.activateTab = function(name){
                setTimeout(function(){
                  $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').closest('li').addClass('active')
                  }, 100);
                  }",
    functions = c('activateTab')),
  extendShinyjs(
    text = "shinyjs.collapse = function(boxid) {
              $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
            }",

    # $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
    functions = c('collapse')),
  
  tags$script(HTML("$('body').addClass('fixed');")),
  
  
  chooseSliderSkin(skin = "Flat", color = BackHeaderButtons),
  
  tags$head(
    tags$style(HTML('
    .content-wrapper, .right-side, .main-footer {margin-left: 300px;}
    .wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}
                     .shiny-notification {position:fixed; top: calc(50% - 150px); left: calc(50% - 150px); 
                                         height: auto !important; opacity:0.98; margin-right:500px}
                     .btn-box-tool {color: #001848; font-size: 15px}')),
    tags$style(
      type = "text/css", 
      ".nav-tabs {
        background-color: #eeeeee !important;}"),
    
    tags$style(
      type = "text/css", 
      ".selectize-input, autonumeric-input {padding-left: 1px; border: none;}
       .selectize-control.single .selectize-input:not(.no-arrow):after {right: 2px;}
       .form-control {padding:2px !important;}
       .autonumeric-input {padding: 2px !important;}"),
    
    tags$style(
      type = "text/css", ".handsontable {overflow: hidden;}"),
    tags$style(
      type = "text/css", ".bg-light-blue, .label-primary, .modal-primary .modal-body {
      background-color: #3f4e4f!important;}"),
    tags$style(type = "text/css", "#inline label{ display: table-cell; text-align: right; vertical-align: middle; } 
               #inline .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineTOP label{ display: table-cell; text-align: right; vertical-align: top; } 
               #inlineTOP .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineTOPWide label{ display: table-cell; text-align: right; vertical-align: text-top; } 
               #inlineTOPWide .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineBOT label{ display: table-cell; text-align: right; vertical-align: bottom; } 
               #inlineBOT .form-group {display: table-row;}"),
    tags$style(HTML(".box.box-solid.box-success {border-top-color:#8daa8a}
                    .box.box-danger {border-top-color:#aa8a8d}")),
    tags$style(HTML('.checkbox-primary input[type="checkbox"]:checked+label::before, .checkbox-primary input[type="radio"]:checked+label::before {
                      background-color: #ffffff; border-color: #ffffff;}')),
    tags$style(HTML('.radio-primary input[type="radio"]:checked+label::after {
                      background-color: #3f4e4f; border-color: #3f4e4f;}')),
    tags$style(".checkbox-bs-mass input[type='checkbox']:checked + label::before,.checkbox-bs-mass input[type='radio']:checked + label::before {
    background-color: #f38f83;border-color: #f38f83;}
.checkbox-mass input[type='checkbox']:checked + label::before,.checkbox-mass input[type='radio']:checked + label::before {
    background-color: #f38f83;border-color: #f38f83;}"),
    tags$style(".checkbox-bs-primary input[type='checkbox']:checked + label::before,.checkbox-bs-primary input[type='radio']:checked + label::before {
    background-color: #dc91ba;border-color: #dc91ba;}
.checkbox-primary input[type='checkbox']:checked + label::before,.checkbox-primary input[type='radio']:checked + label::before {
    background-color: #dc91ba;border-color: #dc91ba;}")
  ), 
  customTheme,
  # div(id = 'inline', style = "float: right;", Nlns(2) ,
      # selectInput('selected_language', NonReqField(i18n$t("Lenguaje:")), choices = i18n$get_languages(), selected = i18n$get_key_translation())),
  tabItems(
    tabItem(tabName = "tabsInicio", inicioLy),
    tabItem(tabName = "tabsCertMass", BalanceCalibCertUI('Balanzas')),
    tabItem(tabName = "tabsCertMRCs", MaterialesRefereUI('MateRefe')),
    tabItem(tabName = "tabsSolution", PreparaDisolucioUI('Solution')),
    tabItem(tabName = "tabsMonoElem", TitularMonoelemtUI('MonoElem')),
    tabItem(tabName = "tabsEDTAsalt"),
    tabItem(tabName = "tabsSummResu", CombinaResultadosUI('Combina')),
    tabItem(tabName = "tabsGenerica")
  )
)

