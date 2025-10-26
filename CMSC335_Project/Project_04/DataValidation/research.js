"use strict";

window.onsubmit = validateForm;

function validateForm(){
    const conditionSelected = document.querySelector("#highBloodPressure").checked || document.querySelector("#diabetes").checked || document.querySelector("#glaucoma").checked || document.querySelector("#asthma").checked || document.querySelector("#none").checked; 
    const periodSelected = document.querySelector("#never").checked || document.querySelector("#lessThanOne").checked || document.querySelector("#oneToTwo").checked || document.querySelector("#moreThanTwo").checked;
    const validPhone1 = document.querySelector("#PBlank1").value.length == 3 && String(parseInt(document.querySelector("#PBlank1").value)) == document.querySelector("#PBlank1").value;
    const validPhone2 = document.querySelector("#PBlank2").value.length == 3 && String(parseInt(document.querySelector("#PBlank2").value)) == document.querySelector("#PBlank2").value;
    const validPhone3 = document.querySelector("#PBlank3").value.length == 4 && String(parseInt(document.querySelector("#PBlank3").value)) == document.querySelector("#PBlank3").value;
    const validPhone = validPhone1 && validPhone2 && validPhone3;
    const validStudyID1 = verifyStudyID(document.querySelector("#StudyId1").value, "A");
    const validStudyID2 = verifyStudyID(document.querySelector("#StudyId2").value, "B");
    const validStudyID = validStudyID1 && validStudyID2;
    const validCondition = (document.querySelector("#highBloodPressure").checked || document.querySelector("#diabetes").checked || document.querySelector("#glaucoma").checked || document.querySelector("#asthma").checked) != (document.querySelector("#none").checked); 
    let errMsg = "";
    if(!validPhone)
        errMsg += "Invalid phone number\n";
    if(!conditionSelected)
        errMsg += "No conditions selected\n";
    else if(!validCondition)
        errMsg += "Invalid conditions selection\n";
    if(!periodSelected)
        errMsg += "No time period selected\n";
    if(!validStudyID)
        errMsg += "Invalid study id\n";

    if(errMsg != ""){
        alert(errMsg);
        return false;
    }
    return window.confirm("Do you want to submit the form data? ");
}

function verifyStudyID(target, firstLetter){
    if(target.length != 4){
        return false;
    }

    const letter = target.slice(0,1);
    const rest = target.slice(1);
    return (letter == firstLetter && String(parseInt(rest)) == rest);
}