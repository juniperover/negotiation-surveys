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
  
  // Update hidden inputs with the selected values
  document.getElementById('svo' + item + '_self_value').value = selfValue;
  document.getElementById('svo' + item + '_other_value').value = otherValue;
  
  // Clear previous values display
  const valuesContainer = document.getElementById('svo_values_' + item);
  valuesContainer.innerHTML = '';
  
  // Create table without labels - just the values and ticks
  const table = document.createElement('table');
  table.style.width = '100%';
  table.style.borderCollapse = 'collapse';
  
  // Create top row for self values
  const topRow = document.createElement('tr');
  
  // Add cells for each value
  for (let i = 0; i < 9; i++) {
    const cell = document.createElement('td');
    cell.style.textAlign = 'center';
    cell.style.padding = '5px';
    cell.textContent = svoValues[item].self[i];
    
    // Highlight the selected value
    if (i === index) {
      cell.style.fontWeight = 'bold';
      cell.style.color = '#007bff';
    }
    
    topRow.appendChild(cell);
  }
  
  // Create middle row for tick marks
  const middleRow = document.createElement('tr');
  middleRow.style.height = '20px';
  
  // Add tick mark cells
  for (let i = 0; i < 9; i++) {
    const cell = document.createElement('td');
    cell.style.textAlign = 'center';
    cell.style.position = 'relative';
    
    // Create the tick mark
    const tick = document.createElement('div');
    tick.style.width = '1px';
    tick.style.height = '20px';
    tick.style.backgroundColor = i === index ? '#007bff' : '#999';
    tick.style.margin = '0 auto';
    
    cell.appendChild(tick);
    middleRow.appendChild(cell);
  }
  
  // Create bottom row for other values
  const bottomRow = document.createElement('tr');
  
  // Add cells for each value
  for (let i = 0; i < 9; i++) {
    const cell = document.createElement('td');
    cell.style.textAlign = 'center';
    cell.style.padding = '5px';
    cell.textContent = svoValues[item].other[i];
    
    // Highlight the selected value
    if (i === index) {
      cell.style.fontWeight = 'bold';
      cell.style.color = '#007bff';
    }
    
    bottomRow.appendChild(cell);
  }
  
  // Add all rows to the table
  table.appendChild(topRow);
  table.appendChild(middleRow);
  table.appendChild(bottomRow);
  
  // Add the table to the values container
  valuesContainer.appendChild(table);
}

// Initialize all sliders when the document is ready
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