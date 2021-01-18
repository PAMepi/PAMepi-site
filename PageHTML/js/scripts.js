function scroll_to(clicked_link, nav_height) {
  var element_class = clicked_link.attr('href').replace('#', '.');
  var scroll_to = 0;
  if (element_class != '.top-content') {
    element_class += '-container';
    scroll_to = $(element_class).offset().top - nav_height;
  }
  if ($(window).scrollTop() != scroll_to) {
    $('html, body').stop().animate({
      scrollTop: scroll_to
    }, 1000);
  }
}


jQuery(document).ready(function() {

  /*
      Navigation
  */
  $('a.scroll-link').on('click', function(e) {
    e.preventDefault();
    scroll_to($(this), $('nav').outerHeight());
  });

  /*
      Background
  */
  $('.section-4-container').backstretch("assets/img/backgrounds/bg.jpg");

  /*
	    Wow
	*/
  new WOW().init();

  /*
      Carousel
  */
  $('#carousel-example').on('slide.bs.carousel', function(e) {

    /*
        CC 2.0 License Iatek LLC 2018
        Attribution required
    */
    var $e = $(e.relatedTarget);
    var idx = $e.index();
    var itemsPerSlide = 3;
    var totalItems = $('.carousel-item').length;

    if (idx >= totalItems - (itemsPerSlide - 1)) {
      var it = itemsPerSlide - (totalItems - idx);
      for (var i = 0; i < it; i++) {
        // append slides to end
        if (e.direction == "left") {
          $('.carousel-item').eq(i).appendTo('.carousel-inner');
        } else {
          $('.carousel-item').eq(0).appendTo('.carousel-inner');
        }
      }
    }
  });

});

$(function() {
  $('#carousel-example').carousel({
    interval: 10000,
    pause: "hover"
  });

  $('input').focus(function() {
    $("#carousel-example").carousel('pause');
  }).blur(function() {
    $("#carousel-example").carousel('cycle');
  });
});


// Set all carousel items to the same height
function normalizeSlideHeights() {
    $('.carousel').each(function(){
      var items = $('.header', this);
      // reset the height
      items.css('min-height', 0);
      // set the height
      var maxHeight = Math.max.apply(null,
          items.map(function(){
              return $(this).outerHeight()}).get() );
      items.css('min-height', maxHeight + 'px');
		

			var items = $('.body-news', this);
			// reset the height
			items.css('min-height', 0);
			// set the height
			var maxHeight = Math.max.apply(null,
					items.map(function(){
							return $(this).outerHeight()}).get() );
			items.css('min-height', maxHeight + 'px');

    })
}

$(window).on(
    'load resize orientationchange',
    normalizeSlideHeights);
