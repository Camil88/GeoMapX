$(document).ready(function(){

//document.getElementsByClassName('nav-item')[0].style.visibility='hidden';
//document.getElementsByClassName('navbar-nav').remove();
//document.getElementsByClassName('sidebar-toggle')[0].style.visibility='hidden';


//$('body').addClass("dark-mode");

//$('.nav-link[data-widget="pushmenu"], #datepicker_button').hide();

// INITIAL ACTIONS WHEN APP IS OPEN FOR THE FIRST TIME

$('.nav-link[data-widget="pushmenu"]').hide();
$('.nav-item').find('i.fa-angle-left').remove();

$('#body_ui-proxyMap-btnCoord2').addClass('makeBlue');
$('#body_ui-proxyMap-btnCoord3').addClass('makeBlue');

$('#datepicker_button').find('.fa').removeClass('fa-calendar');
$('#datepicker_button').find('.fa').addClass("fa-arrow-circle-right");



// te 2 ponizsze usuniete - to ustawialo ikonke zaznaczania w boxach transits i drivers.
//$('#body_ui-proxyMap-boxTable-btnDrawAreaTransits').prependTo('#shiny-tab-transitsOrders .card-tools');
//$('#body_ui-proxyMap-boxTable-btnDrawAreaDrivers').prependTo('#shiny-tab-drivers .card-tools');




// SIDEBAR ACTIONS

$('#header_ui-btnBurger').click(function(){
  icon = $(this).find(".fa");
  icon.toggleClass("fa-chevron-left");

  var displayReports = $('.nav-treeview:eq(0)').css('display');  
  var displayDatepicker = $('.nav-treeview:eq(1)').css('display');

  if (displayReports == 'block') {
    $('.nav-treeview:eq(0)').toggle();
    $('.has-treeview:eq(0)').toggleClass('menu-is-opening menu-open');
    
  } else if (displayDatepicker == 'block') {
    $('.nav-treeview:eq(1)').toggle();
    $('.has-treeview:eq(1)').toggleClass('menu-is-opening menu-open');    
  }

});


// toggle sidebar with report & calendar tabs
$('.fa-paper-plane').click(function(){ 
  $('body').toggleClass('sidebar-collapse');
  $('#header_ui-btnBurger .fa').toggleClass("fa-chevron-left");
  
  var displayReports = $('.nav-treeview:eq(1)').css('display');
  var collapseSidebar = $('body').hasClass('sidebar-collapse');

  if ((displayReports == 'none') && (collapseSidebar === false)) {
    $('.nav-treeview:eq(0)').hide();
    $('.has-treeview:eq(0)').removeClass('menu-is-opening menu-open'); 
    
  } else {
    $('.nav-treeview:eq(0)').show();
    $('.has-treeview:eq(0)').addClass('menu-is-opening menu-open');       
  }
  
});


$('.fa-calendar-alt').click(function(){ 
  $('body').toggleClass('sidebar-collapse');
  $('#header_ui-btnBurger .fa').toggleClass("fa-chevron-left");

  var displayDatepicker = $('.nav-treeview:eq(1)').css('display');
  var collapseSidebar = $('body').hasClass('sidebar-collapse');

  if ((displayDatepicker == 'none') && (collapseSidebar === false)) {
    $('.nav-treeview:eq(1)').hide();
    $('.has-treeview:eq(1)').removeClass('menu-is-opening menu-open'); 
    
  } else {
    $('.nav-treeview:eq(1)').show();
    $('.has-treeview:eq(1)').addClass('menu-is-opening menu-open');       
  }

});


$('#datepicker_button').click(function(){ 
  $('body').toggleClass('sidebar-collapse');
  $('.nav-treeview:eq(1)').toggle();
  $('.has-treeview:eq(1)').toggleClass('menu-is-opening menu-open');
  $('#header_ui-btnBurger .fa').toggleClass("fa-chevron-left");

});








// BOXTABLE ACTIONS
// te 2 ponizsze usuniete - to robilo akcje na ikonce zaznaczania w boxach transits i drivers.
/*
$('#body_ui-proxyMap-boxTable-btnDrawAreaTransits .fa-vector-square').click(function(){
  //document.querySelector('.leaflet-draw-draw-rectangle').click();
  $(this).toggleClass("fa-times");
  $('#body_ui-proxyMap-boxTable-btnDrawAreaDrivers .fa-vector-square').toggleClass("fa-times");
});

$('#body_ui-proxyMap-boxTable-btnDrawAreaDrivers .fa-vector-square').click(function(){
  //document.querySelector('.leaflet-draw-draw-rectangle').click();
  $(this).toggleClass("fa-times");
  $('#body_ui-proxyMap-boxTable-btnDrawAreaTransits .fa-vector-square').toggleClass("fa-times");
});
*/



// OPTIONS PANEL ACTIONS

// toggle search bar and search-cancel
$('#body_ui-proxyMap-optionsBtns-btnOpt1').click(function(){

  $("input.search-input").animate({width:'toggle'},650);

  var displayInput = $('.search-cancel').css('display');
  var textInput = $("input.search-input").val();
  
  if ((displayInput == 'block') && (textInput !== '')) {
    $('.search-cancel').fadeOut("fast");
    
  } else if ((displayInput == 'none') && (textInput === '')) {
    $('.search-cancel').fadeOut("fast");
  
  } else if ((displayInput == 'none') && (textInput !== '')) {
    $('.search-cancel').fadeIn(1500);
  }
});


// toggle map layers control
$('#body_ui-proxyMap-optionsBtns-btnOpt3').click(function(){
  $('.leaflet-control-layers-expanded').fadeToggle(400);
});


// toggle buttons color
$('#body_ui-proxyMap-optionsBtns-btnOpt1').click(function(){
 
  $(this).toggleClass("makeViolet");
  
  var btn3 = $('#body_ui-proxyMap-optionsBtns-btnOpt3');
  var btn4 = $('#body_ui-proxyMap-optionsBtns-btnOpt4');

  if (btn3.hasClass("makeViolet")) {
    btn3.toggleClass("makeViolet");
    $('.leaflet-control-layers-expanded').fadeToggle(400);
    
  } else if (btn4.hasClass("makeViolet")) {
    btn4.toggleClass("makeViolet");
    
  } else {
    return;
  }

}); 


$('#body_ui-proxyMap-optionsBtns-btnOpt3').click(function(){

  $(this).toggleClass("makeViolet");
  
  var btn1 = $('#body_ui-proxyMap-optionsBtns-btnOpt1');
  var btn4 = $('#body_ui-proxyMap-optionsBtns-btnOpt4');

  if (btn1.hasClass("makeViolet")) {
    btn1.toggleClass("makeViolet");
    $("input.search-input").animate({width:'toggle'},650);
    
  } else if (btn4.hasClass("makeViolet")) {
    btn4.toggleClass("makeViolet");
    
  } else {
    return;
  }

}); 


$('#body_ui-proxyMap-optionsBtns-btnOpt4').click(function(){
  
  $(this).toggleClass("makeViolet");
  
  var btn1 = $('#body_ui-proxyMap-optionsBtns-btnOpt1');
  var btn3 = $('#body_ui-proxyMap-optionsBtns-btnOpt3');
  
  if (btn1.hasClass("makeViolet")) {
    btn1.toggleClass("makeViolet");
    $("input.search-input").animate({width:'toggle'},650);
    
  } else if (btn3.hasClass("makeViolet")) {
    btn3.toggleClass("makeViolet");
    $('.leaflet-control-layers-expanded').fadeToggle(400);
    
  } else {
    return;
  }

}); 

$('#body_ui-proxyMap-optionsBtns-btnOpt3').click(function(){
  $(this).toggleClass("makeViolet");
});



// ANALYSIS PANEL ACTIONS

// toggle buttons color
$('#body_ui-proxyMap-analysis-btnAnl1').click(function(){
 
  $(this).toggleClass("makeBlue");
  
  var btn2 = $('#body_ui-proxyMap-analysis-btnAnl2');
  var btn3 = $('#body_ui-proxyMap-analysis-btnAnl3');

  if (btn2.hasClass("makeBlue")) {
    btn2.toggleClass("makeBlue");
    
  } else if (btn3.hasClass("makeBlue")) {
    btn3.toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 


$('#body_ui-proxyMap-analysis-btnAnl2').click(function(){

  $(this).toggleClass("makeBlue");
  
  var btn1 = $('#body_ui-proxyMap-analysis-btnAnl1');
  var btn3 = $('#body_ui-proxyMap-analysis-btnAnl3');

  if (btn1.hasClass("makeBlue")) {
    btn1.toggleClass("makeBlue");
    
  } else if (btn3.hasClass("makeBlue")) {
    btn3.toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 


$('#body_ui-proxyMap-analysis-btnAnl3').click(function(){
  
  $(this).toggleClass("makeBlue");
  
  var btn1 = $('#body_ui-proxyMap-analysis-btnAnl1');
  var btn2 = $('#body_ui-proxyMap-analysis-btnAnl2');
  
  if (btn1.hasClass("makeBlue")) {
    btn1.toggleClass("makeBlue");
    
  } else if (btn2.hasClass("makeBlue")) {
    btn2.toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 



// ANALYSIS PANEL ACTIONS

// toggle buttons color
$('#body_ui-proxyMap-btnCoord1').click(function(){
 
  $(this).toggleClass("makeBlue");
  
  var btn2 = $('#body_ui-proxyMap-btnCoord2');
  var btn3 = $('#body_ui-proxyMap-btnCoord3');

  if (btn2.hasClass("makeBlue") & btn3.hasClass("makeBlue")) {
    btn2.toggleClass("makeBlue");

  } else if (btn2.hasClass("makeBlue") & btn3.hasClass("makeBlue") == false) {
    btn2.toggleClass("makeBlue");
    btn3.toggleClass("makeBlue");
  
  } else if (btn3.hasClass("makeBlue") == false) {
    btn3.toggleClass("makeBlue");
    $(this).toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 



$('#body_ui-proxyMap-btnCoord2').click(function(){
 
  $(this).toggleClass("makeBlue");
  
  var btn1 = $('#body_ui-proxyMap-btnCoord1');
  var btn3 = $('#body_ui-proxyMap-btnCoord3');

  if (btn1.hasClass("makeBlue") & btn3.hasClass("makeBlue")) {
    btn1.toggleClass("makeBlue");

  } else if (btn1.hasClass("makeBlue") & btn3.hasClass("makeBlue") == false) {
    btn1.toggleClass("makeBlue");
    btn3.toggleClass("makeBlue");
  
  } else if (btn3.hasClass("makeBlue") == false) {
    btn3.toggleClass("makeBlue");
    $(this).toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 

/*

$('#body_ui-proxyMap-btnCoord2').click(function(){
 
  $(this).toggleClass("makeBlue");
  
  var btn1 = $('#body_ui-proxyMap-btnCoord1');
  var btn3 = $('#body_ui-proxyMap-btnCoord3');

  if (btn1.hasClass("makeBlue")) {
    btn1.toggleClass("makeBlue");
    
  } else if (btn3.hasClass("makeBlue") == false) {
    btn3.toggleClass("makeBlue");
    $(this).toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 
*/



$('#body_ui-proxyMap-btnCoord3').click(function(){
 
  $(this).toggleClass("makeBlue");
  
  var btn1 = $('#body_ui-proxyMap-btnCoord1');
  var btn2 = $('#body_ui-proxyMap-btnCoord2');

  if (btn1.hasClass("makeBlue") == false & btn2.hasClass("makeBlue") == false) {
    $(this).toggleClass("makeBlue");
    
  //} else if (btn3.hasClass("makeBlue") == false) {
  //  btn3.toggleClass("makeBlue");
   // $(this).toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 



// OTHERS



// options buttons color change when clicked 
/*
$('#body_ui-proxyMap-optionsBtns-btnOpt1, #body_ui-proxyMap-optionsBtns-btnOpt2, #body_ui-proxyMap-optionsBtns-btnOpt3, #body_ui-proxyMap-optionsBtns-btnOpt4').click(function(){
  $(this).toggleClass("makeViolet");
});
*/





// buttons triggering elements on Toolbar
/*
$('#btnOpt2').click(function(){
  document.querySelector('.leaflet-draw-draw-polygon').click();
});  */







/* jakby co to jest zrobione  z poziomu buttona btnAnl1
$('#body_ui-proxyMap-analysis-btnAnl1').click(function(){
  document.querySelector('.leaflet-draw-draw-rectangle').click();
});
*/











/* $('.fa-shipping-fast').click(); */

/*
$('.fa-shipping-fast').click(function(){ 
  $('#leftPanel').css("width", "410px");

});

$('.fa-user-friends').click(function(){ 
  $('#leftPanel').css("width", "440px");

}); 

*/






/*
$('#searchBox').keyup(function(){
  // Declare variables
  var input, filter, table, tr, td, i, txtValue;
  input = document.getElementById("searchBox_text");
  filter = input.value.toUpperCase();
  table = document.getElementById("body_ui-boxTableTransits-table");
  tr = table.getElementsByClassName("rt-tr");
  //td = table.getElementsByClassName("rt-td");
  
  // Loop through all table rows, and hide those who don't match the search query
  for (i = 0; i < tr.length; i++) {
    //td = tr[i].getElementsByClassName("rt-td")[i];
    td = tr[i].getElementsByClassName("rt-td");
    for(j = 0; j < td.length; j++) {
    if (td) {
      txtValue = td.textContent || td.innerText;
      if (txtValue.toUpperCase().indexOf(filter) > -1) {
        tr[j].style.display = "";
      } else {
        tr[j].style.display = "none";
      }
    }
  }
  }
});

console.log(tr[i].textContent || tr[i].innerText);

$('.rt-expandable:eq(3)').click();

*/

/*
$('.fa-chart-bar').click(function(){ 
$('#shiny-tab-dashboard').show();
});

*/


// add draw button and add draw action to it
// $('.card-header .btn-tool').before( "<button class='btn btnDrawArea'><i class='fa fa-border-style'></i></button>" );

//$('#body_ui-proxyMap-btnDrawArea2').prependTo('#shiny-tab-drivers .card-tools');




});

