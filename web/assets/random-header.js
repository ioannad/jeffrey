var i = 0;
var images = new Array();
var captio = new Array();

images[0] = "/examples/preview/preview-000.png";
captio[0] = "Consequences (descendants) of HR.132";

images[1] = "/examples/preview/preview-001.png";
captio[1] = "Alephs and their properties";

images[2] = "/examples/preview/preview-002.png";
captio[2] = "The full diagram (number labels)";

images[3] = "/examples/preview/preview-003.png";
captio[3] = "HR. 0 - 10";

images[4] = "/examples/preview/preview-004.png";
captio[4] = "HR. 0 - 22";

images[5] = "/examples/preview/preview-005.png";
captio[5] = "Old collection of choice principles";

images[6] = "/examples/preview/preview-000.png";
captio[6] = "10 pseudo-random HR. forms";

function nextImage()
{
    var ran = Math.floor((Math.random()*10) % images.length);
    document.slider.src = images[ran];
}

window.onload=nextImage;
