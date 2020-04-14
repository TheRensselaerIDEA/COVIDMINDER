function resize_plot() {
  var sidebar1 = document.getElementById("tab-3554-1");
  var positionInfo = sidebar1.getBoundingClientRect();
  var height = positionInfo.height;
  var mainpanel1 = document.getElementById("mainPanel_us_mort").style.height = height;
  console.log(height);
}

window.addEventListener("onresize", resize_plot);
resize_plot();
