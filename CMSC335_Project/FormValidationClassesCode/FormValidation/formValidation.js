"use strict";

/* IMPORTANT: Setting the function to call when submit is selected */
window.onsubmit = validateForm;

/* This function must return true or false 
   If true the data will be sent to the server. 
   If false the data will not be sent to the server */

function validateForm() {
    /* Retrieving the values */
    const name = document.getElementById("name").value;
    const account = document.getElementById("account").value;
    const payment = document.getElementById("payment").value;

    /* Validating numeric values */
    let invalidMessages = "";
    if (String(parseInt(account)) !== account) { /* Would it detect NaN as invalid account? */
        invalidMessages += "Invalid account number provided.\n";
    }
    const paymentNumeric = Number(payment);
    if (Number.isNaN(paymentNumeric) || paymentNumeric <= 0) {
        invalidMessages += "Invalid payment amount provided.";
    }

    if (invalidMessages !== "") { /* Can be written as if (invalidMessages) */
        alert(invalidMessages);
        return false;
    } else {
        let valuesProvided = "Do you want to submit the following payment?\n";
        valuesProvided += "Name: " + name + "\n";
        valuesProvided += "Account: " + account + "\n";
        valuesProvided += "Payment: " + payment + "\n";
        
        /* We could have written confirm(valuesProvide) */
        return window.confirm(valuesProvided);
    }
}
