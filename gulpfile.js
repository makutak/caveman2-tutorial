var gulp = require("gulp");

// sass compiler
var sass = require("gulp-sass");

// add vender prifix
var autoprefixer = require("gulp-autoprefixer");

// css minify
var cleanCSS = require('gulp-clean-css');

gulp.task("custom", function() {
    gulp.src("assets/custom.scss")
    .pipe(sass({
      includePaths: ["bower_components/bootstrap-sass/assets/stylesheets"]
    }).on('error', sass.logError))
    .pipe(autoprefixer())
    .pipe(cleanCSS())
    .pipe(gulp.dest("./static/css/"));
});
