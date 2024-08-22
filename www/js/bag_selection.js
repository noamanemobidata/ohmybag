function selectBagSize(element) {
    const bags = document.querySelectorAll('.bag-size');
    bags.forEach(bag => {
        bag.classList.remove('selected');
    });
    element.classList.add('selected');
    
    // Mettre à jour la valeur de l'input caché
    var selectedSize = element.getAttribute('data-size');
    Shiny.setInputValue('bag_size', selectedSize, {priority: 'event'});
}