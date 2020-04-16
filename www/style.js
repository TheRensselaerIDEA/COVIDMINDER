var sidebars = ["sidebar_us_mort", "sidebar_us_test", "sidebar_us_hosp", "sidebar_us_db", "sidebar_us_cardio", 
                "sidebar_ny_mort", "sidebar_ny_cases", "sidebar_ny_CoT", "sidebar_ny_det",
                "sidebar_ny_CoT_rates"];
var mainpanels = ["#mainpanel_us_mort", "#mainpanel_us_test", "#mainpanel_us_hosp","#mainpanel_us_db","#mainpanel_us_cardio", 
                  "#mainpanel_ny_mort", "#mainpanel_ny_cases", "#mainpanel_ny_CoT", "#mainpanel_ny_det","#mainpanel_ny_CoT_rates"];
function resize_plot() {
  for (let i=0; i<sidebars.length; i++) {
  var sidebar1 = document.getElementById(sidebars[i]);
  if(sidebar1) {
    var positionInfo = sidebar1.getBoundingClientRect();
    var height = positionInfo.height;
    $(mainpanels[i]).height(height);
  }
  }
}

//$("#mainPanel_us_mort").on("resize", resize_plot);
window.addEventListener("resize", resize_plot);
window.addEventListener("click", resize_plot);
resize_plot();
