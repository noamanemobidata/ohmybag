function update(e){
     
  var rankList = document.getElementById('rank-list-rank_list_1');
  
  var rect = rankList.getBoundingClientRect();
  
  var x = (e.clientX || e.touches[0].clientX) - rect.left
  var y = (e.clientY || e.touches[0].clientY ) -rect.top

  document.documentElement.style.setProperty('--cursorX', x + 'px')
  document.documentElement.style.setProperty('--cursorY', y + 'px')
}

Shiny.addCustomMessageHandler('initializeUpdateFunction', function(message) {
  document.addEventListener('mousemove', update)
  document.addEventListener('touchmove', update)
});