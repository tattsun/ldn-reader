var isMobile = function() {
    var ua = navigator.userAgent;
    if(ua.indexOf('iPhone') > 0 || ua.indexOf('iPod') > 0 || ua.indexOf('Android') > 0
       && ua.indexOf('Mobile') > 0) {
        return true;
    } else if (ua.indexOf('iPad') > 0 || ua.indexOf('Android') > 0) {
        return true;
    }
    return false;
};

$('ul.slimmenu').slimmenu({
    resizeWidth: '800',
    collapserTitle: 'Livedoor News Reader',
    animSpeed: 'medium',
    easingEffect: null,
    indentChildren: false,
    childenIndenter: '&nbsp;'
});

(function() {
    if (isMobile() && $('#toMobile').length > 0) {
        $('#toMobile').html("<a href='/m/top'>モバイルサイトへ</a>");
    }
})();
