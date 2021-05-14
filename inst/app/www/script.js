$(document).ready(function(){


// INITIAL ACTIONS WHEN THE APP IS OPEN

$('.nav-link[data-widget="pushmenu"]').hide();
$('.nav-item').find('i.fa-angle-left').remove();

$('#body_ui-proxyMap-btnCoord2').addClass('makeBlue');
$('#body_ui-proxyMap-btnCoord3').addClass('makeBlue');

$('#datepicker_button').find('.fa').removeClass('fa-calendar');
$('#datepicker_button').find('.fa').addClass("fa-arrow-circle-right");

$('#controlbar-toggle').hide();



// SIDEBAR ACTIONS

$('#header_ui-btnBurger').click(function(){
  icon = $(this).find(".fa");
  icon.toggleClass("fa-chevron-left");

  var displayDatepicker = $('.nav-treeview:eq(0)').css('display');

  if (displayDatepicker == 'block') {
    $('.nav-treeview:eq(0)').toggle();
    $('.has-treeview:eq(0)').toggleClass('menu-is-opening menu-open');
  }

});


$('.fa-calendar-alt').click(function(){ 
  $('body').toggleClass('sidebar-collapse');
  $('#header_ui-btnBurger .fa').toggleClass("fa-chevron-left");

  var displayDatepicker = $('.nav-treeview:eq(0)').css('display');
  var collapseSidebar = $('body').hasClass('sidebar-collapse');

  if ((displayDatepicker == 'none') && (collapseSidebar === false)) {
    $('.nav-treeview:eq(0)').hide();
    $('.has-treeview:eq(0)').removeClass('menu-is-opening menu-open'); 
    
  } else {
    $('.nav-treeview:eq(0)').show();
    $('.has-treeview:eq(0)').addClass('menu-is-opening menu-open');       
  }

});


$('#datepicker_button').click(function(){ 
  $('body').toggleClass('sidebar-collapse');
  $('.nav-treeview:eq(0)').toggle();
  $('.has-treeview:eq(0)').toggleClass('menu-is-opening menu-open');
  $('#header_ui-btnBurger .fa').toggleClass("fa-chevron-left");

});



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

  if (btn3.hasClass("makeViolet")) {
    btn3.toggleClass("makeViolet");
    $('.leaflet-control-layers-expanded').fadeToggle(400);
  
  } else {
    return;
  }

}); 


$('#body_ui-proxyMap-optionsBtns-btnOpt3').click(function(){

  $(this).toggleClass("makeViolet");
  
  var btn1 = $('#body_ui-proxyMap-optionsBtns-btnOpt1');

  if (btn1.hasClass("makeViolet")) {
    btn1.toggleClass("makeViolet");
    $("input.search-input").animate({width:'toggle'},650);
  
  } else {
    return;
  }

}); 


$('#sidebar_ui-btnAnalysis').click(function(){
  $(this).toggleClass("makeBlue");
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



$('#body_ui-proxyMap-btnCoord3').click(function(){
 
  $(this).toggleClass("makeBlue");
  
  var btn1 = $('#body_ui-proxyMap-btnCoord1');
  var btn2 = $('#body_ui-proxyMap-btnCoord2');

  if (btn1.hasClass("makeBlue") == false & btn2.hasClass("makeBlue") == false) {
    $(this).toggleClass("makeBlue");
    
  } else {
    return;
  }

}); 


});

