
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

