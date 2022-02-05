function hidefunction(){
    var x = document.getElementById("spinDiv");
    if (x.style.display=="none") {
        x.style.display="block";
    }
    else {
        x.style.display ="none";
    }
}

document.getElementById("spinDiv").onclick = hidefunction();
