function notSelectedValue(fieldId, selectedFields) {
    var field = document.getElementById(fieldId);
    var selectedIndex = field.selectedIndex;
    console.log("selectedIndex: " + selectedIndex + " for fieldId: " + fieldId);
    if (selectedIndex !== -1) {
      selectedFields.push(fieldId);
    }
  }

  export { notSelectedValue };
