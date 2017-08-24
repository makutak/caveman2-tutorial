$('.users').infiniteScroll({
  // options
  path: 'users?page={{#}}',
  append: '.users li',
  history: true,
  debug: true
});
