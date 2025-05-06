// Define SVO values for each item at each position
const svoValues = {
  1: {
    self: [85, 85, 85, 85, 85, 85, 85, 85, 85],
    other: [85, 76, 68, 59, 50, 41, 33, 24, 15]
  },
  2: {
    self: [85, 87, 89, 91, 93, 94, 96, 98, 100],
    other: [15, 19, 24, 28, 33, 37, 41, 46, 50]
  },
  3: {
    self: [50, 54, 59, 63, 68, 72, 76, 81, 85],
    other: [100, 98, 96, 94, 93, 91, 89, 87, 85]
  },
  4: {
    self: [50, 54, 59, 63, 68, 72, 76, 81, 85],
    other: [100, 89, 79, 68, 58, 47, 36, 26, 15]
  },
  5: {
    self: [100, 94, 88, 81, 75, 69, 63, 56, 50],
    other: [50, 56, 63, 69, 75, 81, 88, 94, 100]
  },
  6: {
    self: [100, 98, 96, 94, 93, 91, 89, 87, 85],
    other: [50, 54, 59, 63, 68, 72, 76, 81, 85]
  }
};

// Function to update the displayed values and hidden inputs
function updateSVOValues(item, value) {
  const index = value - 1; // Convert to 0-based index
  const selfValue = svoValues[item].self[index];
  const otherValue = svoValues[item].other[index];
  
  // Update the values display
  const valuesContainer = document.getElementById('svo_values_' + item);
  valuesContainer.innerHTML = '';
  
  // Create 9 value columns with clear visuals for the current selection
  for (let i = 0; i < 9; i++) {
    const valueDiv = document.createElement('div');
    valueDiv.className = 'svo-value';
    
    // Add self value
    const selfDiv = document.createElement('div');
    selfDiv.className = 'svo-value-self';
    selfDiv.textContent = svoValues[item].self[i];
    
    // Add divider
    const divider = document.createElement('div');
    divider.className = 'svo-divider';
    
    // Add other value
    const otherDiv = document.createElement('div');
    otherDiv.className = 'svo-value-other';
    otherDiv.textContent = svoValues[item].other[i];
    
    // Highlight the selected position
    if (i === index) {
      selfDiv.style.fontWeight = 'bold';
      selfDiv.style.color = '#007bff';
      otherDiv.style.fontWeight = 'bold';
      otherDiv.style.color = '#007bff';
      divider.style.backgroundColor = '#007bff';
      
      // Add a highlighted connector line between the values
      divider.style.width = '2px';
    }
    
    valueDiv.appendChild(selfDiv);
    valueDiv.appendChild(divider);
    valueDiv.appendChild(otherDiv);
    
    valuesContainer.appendChild(valueDiv);
  }
  
  // Update hidden inputs with the selected values
  document.getElementById('svo' + item + '_self_value').value = selfValue;
  document.getElementById('svo' + item + '_other_value').value = otherValue;
}

// Initialize all sliders
$(document).ready(function() {
  for (let i = 1; i <= 6; i++) {
    // Initialize with default value
    updateSVOValues(i, 5);
    
    // Add change listener
    $('#svo' + i).on('change', function(e) {
      updateSVOValues(i, parseInt(e.target.value));
    });
  }
});
