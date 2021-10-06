function scroll_to(clicked_link, nav_height) {
  var element_class = clicked_link.attr("href").replace("#", ".");
  var scroll_to = 0;
  if (element_class != ".top-content") {
    element_class += "-container";
    scroll_to = $(element_class).offset().top - nav_height;
  }
  if ($(window).scrollTop() != scroll_to) {
    $("html, body").stop().animate(
      {
        scrollTop: scroll_to,
      },
      1000
    );
  }
}

jQuery(document).ready(function () {
  /*
        Navigation
    */
  $("a.scroll-link").on("click", function (e) {
    e.preventDefault();
    scroll_to($(this), $("nav").outerHeight());
  });

  /*
        Carousel
    */
  $("#carousel-example").on("slide.bs.carousel", function (e) {
    /*
            CC 2.0 License Iatek LLC 2018
            Attribution required
        */
    var $e = $(e.relatedTarget);
    var idx = $e.index();
    var itemsPerSlide = 3;
    var totalItems = $(".carousel-item").length;

    if (idx >= totalItems - (itemsPerSlide - 1)) {
      var it = itemsPerSlide - (totalItems - idx);
      for (var i = 0; i < it; i++) {
        // append slides to end
        if (e.direction == "left") {
          $(".carousel-item").eq(i).appendTo(".carousel-inner");
        } else {
          $(".carousel-item").eq(0).appendTo(".carousel-inner");
        }
      }
    }
  });
});

$(function () {
  $("#carousel-example").carousel({
    interval: 10000,
    pause: "hover",
  });

  $("input")
    .focus(function () {
      $("#carousel-example").carousel("pause");
    })
    .blur(function () {
      $("#carousel-example").carousel("cycle");
    });
});

// Set all carousel items to the same height
function normalizeSlideHeights() {
  $(".carousel").each(function () {
    var items = $(".box-header", this);
    // reset the height
    //items.css('min-height', 0);
    // set the height
    var maxHeight = Math.max.apply(
      null,
      items
        .map(function () {
          return $(this).outerHeight();
        })
        .get()
    );
    items.css("min-height", maxHeight + "px");

    var items = $(".body-news", this);
    // reset the height
    items.css("min-height", 0);
    // set the height
    var maxHeight = Math.max.apply(
      null,
      items
        .map(function () {
          return $(this).outerHeight();
        })
        .get()
    );
    items.css("min-height", maxHeight + "px");
  });
}

$(window).on("load resize orientationchange", normalizeSlideHeights);

// change size of navbar when scroll down the page
window.onscroll = function () {
  if (document.body.scrollTop > 50 || document.documentElement.scrollTop > 50) {
    document.getElementById("navbar").style.padding = "0";
    logos = document.getElementsByClassName("navbar-brand");
    for (logo of logos) {
      logo.style.fontSize = "1rem";
    }
    document.getElementsByClassName("navbar-brand")[1].style.fontSize = "1rem";
    document.getElementById("img-logo").style.width = "30px";
    document.getElementById("img-logo").style.height = "30px";
  } else {
    logos = document.getElementsByClassName("navbar-brand");
    for (logo of logos) {
      logo.style.fontSize = "1.2rem";
    }
    document.getElementById("navbar").style.padding = "0.8rem 0";

    document.getElementById("img-logo").style.width = "40px";
    document.getElementById("img-logo").style.height = "40px";
  }
};

// change position of the footer to be always on the bottom
$(document).ready(function () {
  setInterval(function () {
    var docHeight = $(window).height();
    var footerHeight = $("#footer").height();
    var footerTop = $("#footer").position().top + footerHeight;
    var marginTop = docHeight - footerTop + 10;

    if (footerTop < docHeight) $("#footer").css("margin-top", marginTop + "px");
    // padding of 30 on footer
    else $("#footer").css("margin-top", "0px");
    // console.log("docheight: " + docHeight + "\n" + "footerheight: " + footerHeight + "\n" + "footertop: " + footerTop + "\n" + "new docheight: " + $(window).height() + "\n" + "margintop: " + marginTop);
  }, 250);
});

// keep dropdown open
$(document).ready(function () {
  $(".dropdown-menu a.dropdown-toggle").on("click", function (e) {
    if (!$(this).next().hasClass("show")) {
      $(this)
        .parents(".dropdown-menu")
        .first()
        .find(".show")
        .removeClass("show");
    }
    var $subMenu = $(this).next(".dropdown-menu");
    $subMenu.toggleClass("show");

    $(this)
      .parents("li.nav-item.dropdown.show")
      .on("hidden.bs.dropdown", function (e) {
        $(".dropdown-submenu .show").removeClass("show");
      });

    return false;
  });
});

function hasTouch() {
  return (
    "ontouchstart" in document.documentElement ||
    navigator.maxTouchPoints > 0 ||
    navigator.msMaxTouchPoints > 0
  );
}

// remove all the :hover on touchscreens devices
if (hasTouch()) {
  try {
    // prevent exception on browsers not supporting DOM styleSheets properly
    for (var si in document.styleSheets) {
      var styleSheet = document.styleSheets[si];
      if (!styleSheet.rules) continue;

      for (var ri = styleSheet.rules.length - 1; ri >= 0; ri--) {
        if (!styleSheet.rules[ri].selectorText) continue;

        if (styleSheet.rules[ri].selectorText.match(":hover")) {
          styleSheet.deleteRule(ri);
        }
      }
    }
  } catch (ex) {}
}
