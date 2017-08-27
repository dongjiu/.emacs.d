var keys = {
    left: 37,
    up: 38,
    right: 39,
    down: 40,
};

var presentation = {
    registerEvents: function () {
        var i, self = this;
        $('body').keyup(function (event) {
            if (event.which == keys.down) {
                self.nextSlide();
            } else if (event.which == keys.up) {
                self.prevSlide();
            }
        });

        $('body').on('click', '.go-home', function() {
            presentation.showSlide(0);
        });

        $('body').on('click', '.slides-overview', function() {
            presentation.showOverview();
        });
    },

    showSlide: function (index) {
        var i, $slides = $('body > .slide');
        for (i = 0; i < $slides.length; i++) {
            if (i == index)
                $($slides[i]).show();
            else
                $($slides[i]).hide();
        }
    },

    changeSlide: function (oldSlide, newSlide) {
        $(oldSlide).fadeOut(100, function () {
            $(newSlide).fadeIn();
        });
    },

    nextSlide: function () {
        var i, $slides = $('body > .slide'), $s;
        for (i = 0; i < $slides.length - 1; i++) {
            $s = $($slides[i]);
            if ($s.is(':visible')) {
                this.changeSlide($s, $slides[i+1]);
                return;
            }
        }
    },

    prevSlide: function () {
        var i, $slides = $('body > .slide'), $s;
        for (i = 1; i < $slides.length; i++) {
            $s = $($slides[i]);
            if ($s.is(':visible')) {
                this.changeSlide($s, $slides[i-1]);
                return;
            }
        }
    },

    addHeaders: function () {
        var $header = $('#templates > #header-template');
        $('.slide').each(function () {
            var title = $(this).data('title');
            var $h = $header.clone();
            $h.find('.title').text(title);
            $h.children().prependTo(this);
        });
    },

    addNavs: function () {
        var $nav = $('#templates > #nav-template');
        $('.slide').each(function () {
            if ($(this).data('with-nav')) {
                $nav.clone().children().appendTo(this);
            }
        });
    },

    showOverview: function () {
        $('.slide').hide();
        $('#overview').html('')
            .append($('.slide').clone())
            .show();
        $('#overview .slide *').attr('disabled', true);
        $('#overview .slide').show();
        $('#overview .slide').animate({'zoom': 0.5}, 0);

        var presentation = this;
        $('#overview .slide').each(function (i) {
            $(this).click(function () {
                $('#overview').hide();
                presentation.showSlide(i);
            });
        });
    },
};

$(document).ready(function () {
    presentation.showSlide(0);
    presentation.registerEvents();
    presentation.addHeaders();
    presentation.addNavs();
});
