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
  
  // Create table for alignment
  const table = document.createElement('table');
  table.style.width = '100%';
  table.style.borderCollapse = 'collapse';
  
  // Create top row for "You Receive" values
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
  
  // Create bottom row for "Other Receives" values
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