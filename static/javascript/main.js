
  var navbarHeight = $('#myNavbar').height(),
      scrollAnimation =800;

  $(document).ready(function() {
    clearNav = function(link){
      $('#myNavbar li').removeClass("active");
    }

    updateNav = function(link){
      clearNav();

      //Choose item
      if (!$('#myNavbar li').hasClass("active")){
        $('#myNavbar li a[href$="' + link+'"').parent().addClass("active");
      }
    }

    $(window).scroll(function() {

      var windowOffset = $(window).scrollTop()+navbarHeight;
      
      if (windowOffset > $("#References").offset().top) {
        updateNav('#References');
      } else if (windowOffset > $("#Methods").offset().top) {
        updateNav('#Methods');
      } else if (windowOffset > $("#Data").offset().top) {
        updateNav('#Data');
      }else {
        clearNav();
      }


    });
      
  });


// Smooth scrolling
$(function() {
  $('a[href*=#]:not([href=#])').click(function() {
    if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'') && location.hostname == this.hostname) {
      var target = $(this.hash);
      target = target.length ? target : $('[name=' + this.hash.slice(1) +']');
      if (target.length) {
        clearNav();
        $('html,body').animate({
          scrollTop: (target.offset().top /*- navbarHeight*/)
        }, scrollAnimation);
        return false;
      }
    }
  });
});

$(function() {
var opts = {
  lines: 13, // The number of lines to draw
  length: 7, // The length of each line
  width: 4, // The line thickness
  radius: 10, // The radius of the inner circle
  corners: 1, // Corner roundness (0..1)
  rotate: 0, // The rotation offset
  color: '#000', // #rgb or #rrggbb
  speed: 1, // Rounds per second
  trail: 60, // Afterglow percentage
  shadow: false, // Whether to render a shadow
  hwaccel: false, // Whether to use hardware acceleration
  className: 'spinner', // The CSS class to assign to the spinner
  zIndex: 2e9, // The z-index (defaults to 2000000000)
  top: 'auto', // Top position relative to parent in px
  left: '50%' // Left position relative to parent in px
};
var target = document.getElementById('spinnerContainer');
var spinner = new Spinner(opts).spin(target);
});
