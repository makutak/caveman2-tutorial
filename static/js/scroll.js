$('.users').infiniteScroll({
  // options
  path: '.page-link',
  append: '.users li',
  history: false,
  debug: true
});

$('#user-microposts').infiniteScroll({
  // options
  path: '.page-link',
  append: '#user-microposts li',
  history: false,
  debug: true
});

$('#feed').infiniteScroll({
  // options
  path: '.page-link',
  append: '#feed li',
  history: false,
  debug: true
});
