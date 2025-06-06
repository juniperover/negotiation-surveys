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
  
  // Create container div with proper alignment
  const container = document.createElement('div');
  container.style.width = '100%';
  container.style.position = 'relative';
  
  // Create table for self values (You Receive)
  const selfTable = document.createElement('table');
  selfTable.style.width = '100%';
  selfTable.style.borderCollapse = 'collapse';
  selfTable.style.marginBottom = '5px';
  
  const selfRow = document.createElement('tr');
  
  // Add cells for self values with proper width distribution
  for (let i = 0; i < 9; i++) {
    const cell = document.createElement('td');
    cell.style.textAlign = 'center';
    cell.style.padding = '3px 2px';
    cell.style.width = '11.11%'; // Equal distribution for 9 columns
    cell.style.fontSize = '14px';
    cell.textContent = svoValues[item].self[i];
    
    // Highlight the selected value
    if (i === index) {
      cell.style.fontWeight = 'bold';
      cell.style.color = '#007bff';
      cell.style.backgroundColor = '#e3f2fd';
      cell.style.borderRadius = '3px';
    }
    
    selfRow.appendChild(cell);
  }
  
  selfTable.appendChild(selfRow);
  
  // Create table for other values (Other Receives)
  const otherTable = document.createElement('table');
  otherTable.style.width = '100%';
  otherTable.style.borderCollapse = 'collapse';
  otherTable.style.marginTop = '5px';
  
  const otherRow = document.createElement('tr');
  
  // Add cells for other values with proper width distribution
  for (let i = 0; i < 9; i++) {
    const cell = document.createElement('td');
    cell.style.textAlign = 'center';
    cell.style.padding = '3px 2px';
    cell.style.width = '11.11%'; // Equal distribution for 9 columns
    cell.style.fontSize = '14px';
    cell.textContent = svoValues[item].other[i];
    
    // Highlight the selected value
    if (i === index) {
      cell.style.fontWeight = 'bold';
      cell.style.color = '#007bff';
      cell.style.backgroundColor = '#e3f2fd';
      cell.style.borderRadius = '3px';
    }
    
    otherRow.appendChild(cell);
  }
  
  otherTable.appendChild(otherRow);
  
  // Add tables to container
  container.appendChild(selfTable);
  container.appendChild(otherTable);
  
  // Add the container to the values container
  valuesContainer.appendChild(container);
}

// Function to align slider with value tables
function alignSlider(item) {
  const slider = document.getElementById('svo' + item);
  if (slider) {
    // Set slider to align with the 9-position grid
    slider.style.width = '100%';
    slider.style.margin = '10px 0';
    
    // Ensure the slider handle aligns with the value positions
    const sliderContainer = slider.parentElement;
    if (sliderContainer) {
      sliderContainer.style.padding = '0';
      sliderContainer.style.margin = '0';
    }
  }
}

// Initialize all sliders when the document is ready
$(document).ready(function() {
  for (let i = 1; i <= 6; i++) {
    // Initialize with default value
    updateSVOValues(i, 5);
    
    // Align slider with value tables
    alignSlider(i);
    
    // Add change listener
    $('#svo' + i).on('change', function(e) {
      updateSVOValues(i, parseInt(e.target.value));
    });
    
    // Also listen for input events for real-time updates
    $('#svo' + i).on('input', function(e) {
      updateSVOValues(i, parseInt(e.target.value));
    });
  }
});

// Additional CSS to ensure proper alignment
$(document).ready(function() {
  // Add custom CSS for slider alignment
  const style = document.createElement('style');
  style.textContent = `
    .svo-slider-container .form-group {
      margin-bottom: 0 !important;
    }
    
    .svo-slider-container .irs {
      margin: 0 !important;
    }
    
    .svo-slider-container .irs-line {
      margin: 0 auto !important;
    }
    
    .svo-values table {
      table-layout: fixed !important;
    }
    
    .svo-slider-container {
      padding: 0 !important;
    }
  `;
  document.head.appendChild(style);
});