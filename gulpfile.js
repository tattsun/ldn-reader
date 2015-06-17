var gulp = require('gulp');
var minifyCss = require('gulp-minify-css');
var uglify = require('gulp-uglify');

gulp.task('minify-css', function() {
    return gulp.src('asset/css/*.css')
        .pipe(minifyCss({compatibility: 'ie8'}))
        .pipe(gulp.dest('dist'));
});

gulp.task('minify-css-vendor', function() {
    return gulp.src('asset/vendor/*.css')
        .pipe(minifyCss({compatibility: 'ie8'}))
        .pipe(gulp.dest('dist'));
});

gulp.task('minify-js-vendor', function() {
    return gulp.src('asset/vendor/*.js')
        .pipe(uglify())
        .pipe(gulp.dest('dist'));
});
