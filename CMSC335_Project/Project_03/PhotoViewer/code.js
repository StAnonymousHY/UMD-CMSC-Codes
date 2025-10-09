"use strict";

let curr;
let loadClicked = false;
let photoList;
let slideShowController;
const interval = 1000;

document.querySelector("#loadPhotos").onclick = loadPhotos;
document.querySelector("#loadJSON").onclick = loadJSON;
document.querySelector("#previousPhoto").onclick = previousPhoto;
document.querySelector("#nextPhoto").onclick = nextPhoto;
document.querySelector("#firstPhoto").onclick = firstPhoto;
document.querySelector("#lastPhoto").onclick = lastPhoto;
document.querySelector("#slideShow").onclick = slideShow;
document.querySelector("#randomSlideShow").onclick = randomSlideShow;
document.querySelector("#stopSlideShow").onclick = stopSlideShow;

function loadPhotos() {
    document.querySelector("#DisplayMessage").textContent = "Photo Viewer System";
    const folder = document.querySelector("#PhotosFolder").value;
    const name = document.querySelector("#CommonName").value;
    let start = document.querySelector("#StartPhotoNumber").value;
    let end = document.querySelector("#EndPhotoNumber").value;

    if(start > end){
        document.querySelector("#DisplayMessage").textContent = "Error: Invalid Range";
        return;
    }

    loadClicked = true;
    curr = 0;

    photoList = [];
    for(let i = start ; i <= end ; i++){
        photoList.push(folder+name+i+".jpg")
    }

    document.querySelector("#Image").src = photoList[curr];
    document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
}

async function loadJSON(){
    document.querySelector("#DisplayMessage").textContent = "Photo Viewer System";
    const URL = document.querySelector("#URL").value;
    loadClicked = true;
    curr = 0;

    const response = await fetch(URL);
    const json = await response.json();

    photoList = [];
    for(let i = 0 ; i < json.images.length ; i++){
        photoList.push(json.images[i].imageURL);
    }

    document.querySelector("#Image").src = photoList[curr];
    document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
}

function previousPhoto(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    curr -= 1;
    if(curr < 0){
        curr = photoList.length - 1;
    }

    document.querySelector("#Image").src = photoList[curr];
    document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
}

function nextPhoto(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    curr += 1;
    if(curr >= photoList.length){
        curr = 0;
    }

    document.querySelector("#Image").src = photoList[curr];
    document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
}

function firstPhoto(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    curr = 0;

    document.querySelector("#Image").src = photoList[curr];
    document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
}

function lastPhoto(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    curr = photoList.length - 1;

    document.querySelector("#Image").src = photoList[curr];
    document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
}

function slideShow(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    clearInterval(slideShowController);
    /* LAMBDA */
    slideShowController = setInterval(() => nextPhoto(), interval);
}

function randomSlideShow(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    clearInterval(slideShowController);
    /* LAMBDA */
    slideShowController = setInterval(() => {
        curr = Math.floor(Math.random() * photoList.length); 
        document.querySelector("#Image").src = photoList[curr];
        document.querySelector("#PhotoBeingDisplayed").value = photoList[curr];
    }, interval);
}

function stopSlideShow(){
    if(!loadClicked){
        document.querySelector("#DisplayMessage").textContent = "Error: you must load data first";
        return;
    }

    clearInterval(slideShowController);
}