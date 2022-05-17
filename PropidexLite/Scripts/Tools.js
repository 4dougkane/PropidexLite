var reds = new Array();
var nred = 0;
var tooltip = null;
var tiprect = null;
var tiptext = null;
var txt1 = null;
var txt2 = null;
var txt3 = null;
var txt4 = null;

var scaleFactor = 1;
var vx1 = 0;
var vy1 = 0;
var vx2 = 960;
var vy2 = 760;
var wx1 = 0;
var wy1 = 0;
var wx2 = 960;
var wy2 = 760;
var zoomLevel = 1;
var cx1 = 480;
var cy1 = 380;
var panRates = 100;
var downX = 0;
var downY = 0;
var moveX = 0;
var moveY = 0;
var mouseMove = false;
document.documentElement.addEventListener('keydown', processKeyPress, true);
document.documentElement.addEventListener('keypress', processKeyPress, true);

function handleMouseDown(evt) {
    var theSvgElement = document.getElementById("root");
    var viewBox = theSvgElement.getAttribute('viewBox'); // Grab the object representing the SVG element's viewBox attribute.
    var viewBoxValues = viewBox.split(' '); 			// Create an array and insert each individual view box attribute value (assume they're seperated by a single whitespace character).

    viewBoxValues[0] = parseFloat(viewBoxValues[0]); 	// Convert string "numeric" values to actual numeric values.
    viewBoxValues[1] = parseFloat(viewBoxValues[1]);
    moveX = 0;
    moveY = 0;
    downX = evt.clientX;
    downY = evt.clientY;
    mouseMove = true;
    handleMouseMove(evt);
}

function handleMouseMove(evt) {
    mouseMove = true;
    moveX = evt.clientX;
    moveY = evt.clientY;
}

function handleMouseUp(evt) {
    var theSvgElement = document.getElementById("root");
    var viewBox = theSvgElement.getAttribute('viewBox'); // Grab the object representing the SVG element's viewBox attribute.
    var viewBoxValues = viewBox.split(' '); 			// Create an array and insert each individual view box attribute value (assume they're seperated by a single whitespace character).
    viewBoxValues[0] = parseFloat(viewBoxValues[0]); 	// Convert string "numeric" values to actual numeric values.
    viewBoxValues[1] = parseFloat(viewBoxValues[1]);
    if (mouseMove == true) {
        if (downX != 0 && moveX !=0) {
            var diffX = downX - moveX;
            viewBoxValues[0] += diffX;
        }
        if (downY != 0 && moveY !=0) {
            var diffY = downY - moveY;
            viewBoxValues[1] += diffY;
        }
        theSvgElement.setAttribute('viewBox', viewBoxValues.join(' '));
        downX = 0;
        downY = 0;
        moveX = 0;
        moveY = 0;
        mouseMove = false;
    }
   }

function processKeyPress(evt) {
    var theSvgElement = document.getElementById("root");
    var viewBox = theSvgElement.getAttribute('viewBox'); // Grab the object representing the SVG element's viewBox attribute.
    var viewBoxValues = viewBox.split(' '); 			// Create an array and insert each individual view box attribute value (assume they're seperated by a single whitespace character).

    viewBoxValues[0] = parseFloat(viewBoxValues[0]); 	// Convert string "numeric" values to actual numeric values.
    viewBoxValues[1] = parseFloat(viewBoxValues[1]);

    switch (evt.keyCode) {
        case 37:
            viewBoxValues[0] += panRates; // Increase the x-coordinate value of the viewBox attribute to pan right.
            break;
        case 39:
            viewBoxValues[0] -= panRates; // Decrease the x-coordinate value of the viewBox attribute to pan left.
            break;
        case 38:
            viewBoxValues[1] += panRates; // Increase the y-coordinate value of the viewBox attribute to pan down.
            break;
        case 40:
            viewBoxValues[1] -= panRates; // Decrease the y-coordinate value of the viewBox attribute to pan up.      
            break;
    } // switch

    theSvgElement.setAttribute('viewBox', viewBoxValues.join(' ')); // Convert the viewBoxValues array into a string with a white space character between the given values.
}

function showTooltip(evt) {
    if (!tooltip) { var tooltip = document.getElementById('tooltip'); }
    if (tooltip !=null) {
        if (tooltip.getAttribute('visibility') == 'hidden') {
            tooltip.setAttribute('visibility', 'show');
            document.getElementById('txt1').style.display = 'none';
            document.getElementById('txt2').style.display = 'none';
            document.getElementById('txt3').style.display = 'none';
            document.getElementById('txt4').style.display = 'none';
            var x = evt.offsetX;
            var y = evt.offsetY;
           // alert('x=' + x + '  y=' + y);
            var nx = (x / scaleFactor) *10;
            var ny = (y / scaleFactor) *10;
            var target = evt.target;
            var Label1 = '';
            var Label2 = '';
            var Label3 = '';
            var Label4 = '';
            var dy = 0;
            if (target.getAttribute('desc') != null) {
                var labels = target.getAttribute('desc').split('^');
                if (labels != null) {
                    if (labels[0] != null) { Label1 = labels[0]; }
                    if (labels[1] != null) { Label2 = labels[1]; }
                    if (labels[2] != null) { Label3 = labels[2]; }
                    if (labels[3] != null) { Label4 = labels[3]; }
                }
                if (!txt1)
                    var txt1 = document.getElementById('txt1');
                if (Label1 != null && Label1.replace(/\s+/g, '').length > 0) {
                    txt1.setAttribute('x', nx);
                    txt1.setAttribute('y', ny);
                    dy += 1.2;
                    txt1.setAttribute('dy', dy +'em');
                    var child1 = txt1.firstChild;
                    child1.data = Label1;
                    txt1.style.display = '';

                }
                if (!txt2)
                    var txt2 = document.getElementById('txt2');
                if (Label2 != null && Label2.replace(/\s+/g, '').length > 0) {
                    
                    txt2.setAttribute('x', nx);
                    txt2.setAttribute('y', ny);
                    dy += 1.2;
                    txt2.setAttribute('dy', dy + 'em');
                    var child2 = txt2.firstChild;
                    child2.data = Label2;
                    txt2.style.display = '';
                }

                if (!txt3)
                    var txt3 = document.getElementById('txt3');
                if (Label3 != null && Label3.replace(/\s+/g, '').length > 0) {
                   
                    txt3.setAttribute('x', nx);
                    txt3.setAttribute('y', ny);
                    dy += 1.2;
                    txt3.setAttribute('dy', dy + 'em');
                    var child3 = txt3.firstChild;
                    child3.data = Label3;
                    txt3.style.display = '';
                }
                if (!txt4)
                    var txt4 = document.getElementById('txt4');
                if (Label4 != null && Label4.replace(/\s+/g, '').length > 0) {
                   
                    txt4.setAttribute('x', nx);
                    txt4.setAttribute('y', ny);
                    dy += 1.2;
                    txt4.setAttribute('dy', dy + 'em');
                    var child4 = txt4.firstChild;
                    child4.data = Label4;
                    txt4.style.display = '';
                }
                if (!tiptext)
                    var tiptext = document.getElementById('tooltext');
                tiptext.setAttribute('x', nx);
                tiptext.setAttribute('y', ny);
                tiptext.setAttribute('font-size', 110 / scaleFactor);

                var contour = tiptext.getBBox();

                if (!tiprect)
                    var tiprect = document.getElementById('toolrect');
                tiprect.setAttribute('x', nx);
                tiprect.setAttribute('y', ny);
                tiprect.setAttribute('width', contour.width + 100 / scaleFactor);
                tiprect.setAttribute('height', contour.height + 100 / scaleFactor);
        }
    }
    return true;
}
}

function hideTooltip(evt) {
    if (!tooltip)
        var tooltip = document.getElementById('tooltip');
    tooltip.setAttribute('visibility', 'hidden');
    document.getElementById('txt1').style.display = 'none';
    document.getElementById('txt2').style.display = 'none';
    document.getElementById('txt3').style.display = 'none';
    document.getElementById('txt4').style.display = 'none';
    return true;
}

function zoomout(evt) {
    zoomLevel = zoomLevel - 1;

    wx2 = vx2 - 96 * zoomLevel;
    wy2 = vy2 - 76 * zoomLevel;
    scaleFactor = vx2 / wx2;

    wx1 = cx1 - (wx2 / 2);
    wy1 = cy1 - (wy2 / 2);
    var view = wx1 * 10 + " " + wy1 * 10 + " " + wx2 * 10 + " " + wy2 * 10;
    document.getElementById("root").setAttribute("viewBox", view);
    return true;
}

function zoomin(evt) {
    zoomLevel = zoomLevel + 1;
    wx2 = vx2 - (96 * zoomLevel);
    wy2 = vy2 - (76 * zoomLevel);
    if (wy2 <= 0) {
        zoomLevel = zoomLevel - 1;
        return true;
    }
    scaleFactor = vx2 / wx2;

    wx1 = cx1 - (wx2 / 2);
    wy1 = cy1 - (wy2 / 2);

    var view = wx1 * 10 + " " + wy1 * 10 + ' ' + wx2 * 10 + ' ' + wy2 * 10;
    //alert(document.getElementById("root").getAttribute("viewBox"));
    document.getElementById("root").setAttribute("viewBox", view);
    return true;
}



