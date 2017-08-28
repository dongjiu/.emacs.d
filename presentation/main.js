var keys = {
    left: 37,
    up: 38,
    right: 39,
    down: 40,
    t: 84,
};

var presentation = {
    registerEvents: function () {
        var i, self = this;
        $('body').keyup(function (event) {
            if (event.which == keys.left) {
                self.prevSlide();
            } else if (event.which == keys.right) {
                self.nextSlide();
            } else if (event.which == keys.up) {
                self.prevView();
            } else if (event.which == keys.down) {
                self.nextView();
            } else if (event.which == keys.t && event.ctrlKey && event.altKey) {
                self.showOverview();
            }
        });

        $('body').on('click', '.go-home', function() {
            self.showSlide(0);
        });

        $('body').on('click', '.slides-overview', function() {
            self.showOverview();
        });

        $('body').on('next-view', '.slide:visible', function () {
            var $elems = $(this).find('.to-show:not(.showed)');
            if ($elems.length > 0) {
                $elems.first().addClass('showed');
            } else {
                self.nextSlide();
            }
        });

        $('body').on('prev-view', '.slide:visible', function () {
            var $elems = $(this).find('.to-show.showed');
            if ($elems.length > 0) {
                $elems.last().removeClass('showed');
            } else {
                self.prevSlide();
            }
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

    prevView: function () {
        $('.slide:visible').trigger('prev-view');
    },

    nextView: function () {
        $('.slide:visible').trigger('next-view');
    },

    addHeaders: function () {
        var $header = $('#templates > #header-template');
        $('.slide').each(function () {
            var title = $(this).data('title');
            if (title) {
                var $h = $header.clone();
                $h.find('.title').text(title);
                $h.children().prependTo(this);
            }
        });
    },

    addNavs: function () {
        var $nav = $('#templates > #nav-template');
        $('.slide').each(function () {
            if ($(this).data('with-nav')) {
                $nav.clone().children().appendTo($('body'));
            }
        });
    },

    showOverview: function () {
        $('.slide').hide();
        $('#overview').html('')
            .append($('.slide').clone())
            .show();
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
