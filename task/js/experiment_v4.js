// ------------ version number -------------
// 1 = local pilot
// 2.0 = turk pilot (first 20 participants)
// 2.1 = pilot with asynchronous code fixed
// 2.2 = first turk batch ()
// 3.0 = pilot of experiment two (three colours)
// 4.0 = pilot of experiment three (semantic tags)
version = 4.0;		

// ------------ structural constants -------------

nCircles = 4; 			// n bandits
maxTrials = 30; 		// n trials per block
nBlocks = 3; 			// number of blocks
nInstruct = 3; 			// number of instructions divs
nQuestions = 4; 		// number of instruction check questions
nDemographic = 4;		// number of demographic questions
walkSigma = 8; 			// SD of gaussian random walk for bandits
payoffSigma = 4; 		// SD of payoffs around bandit mean
changeWindowStart = 9; 	// first trial at which a visual change can occur in each block 
changeWindowEnd = 24;	// final trial at which a visual change can occur in each block
						// 		i.e. changes can occur from trial changeWindowStart to trial changeWindowEnd. 
						// 		(this replaces this changePadding variable in version 2.1 and before.


// ------------ window events -------------

window.onbeforeunload = onBeforeUnloadHandler;


// ------------ display constants -------------

radius = 45; 				// radius of option circles
circleOffset = 140; 		// offset of circles from centre of html canvas
lineWidth = 4; 				// line width for circle outline
outlineColour = "#000000";	// standard outline of options
selectionColour = "#00BFFF";// outline of selected option
fillColour = "#FFFFFF"		// set to white for this experiment
tagTexts = ["    ???    ", "     good      ", "     bad     "] // these are the three possible tag texts

// ------------ timing constants -------------

selectionDisplayInterval = 1000; 	// time (in milliseconds) that a selected option is outlined before feedback
feedbackDisplayInterval = 1000; 	// time (in milliseconds) that feedback is on screen
blockMinimumInterval = 2000; 		// time (in milliseconds) that block feedback will be shown for


// ------------ display preferences -------------

canvas = document.getElementById("canvasHandle");	// id of html canvas
context = canvas.getContext("2d"); 
context.font = "40px Helvetica"; 					// font for html canvas
context.textAlign = "center"; 						// text alignment in html canvas
centreX = canvas.width / 2; 						// horizontal centre of canvas
centreY = canvas.height / 2; 						// vertical centre of canvas
var whichSelected = "none"; 						// variable to be updated with selected option on each trial
var whichFilled = "none"; 							// variable to be updated with filled option


// ------------ initialise containers -------------

var payoffs; 		// matrix containing actual payouts; size is nOptions x nBlocks x nTrials 
var changeNumber; 	// at which trial number is an option filled?
var whichToFill; 	// container determining which option is filled at change time
var data = {}; 		// data container
data.walks = [];	// container for walk mean matrix;
data.payoffs = [];	// container for payoff matrix
var trialData = {};	// container for trialData
var trialTic; 		// trialTimer
var trialToc;		// another trial timer


// ------------ initialise counters -------------

var blockNo = 1;			// which block is it?
var trialNo = 1;			// which trial is it?
var instruct = 1; 			// which instruction is the participant up to?
var checkCount = 0; 		// how many times did the participant fail the instruction check?
var blockWinnings = 0; 		// how much has the participant won in this block?
var maxBlockWinnings = 0; 	// what is the maximum the participant could have won in this block?
var nCompleted = 0;			// number of demographic questions completed


// ------------ timing stuff -------------

data.startTime = Date.now() 
data.stopTime = 0


// ------------ other stuff -------------

data.instructionFails = 0 ;													// how many times did they fail the check?
data.instructionTime = []; 													// track how long they looked at the instructions
data.instructionCheckScore = [];											// how many questions did they get right each time?
data.unloadAttempts = 0; 													// idle curiosity: does anyone really try to leave?
data.ID = Math.floor( Math.random()*69999999 )+10000000 					// generate completion code
idString = data.ID.toString();
document.getElementById("turkcode").innerText =  idString.substring(0,4); 	// write the turk code to the document right at the beginning
data.version = version;


// ------------ functions: flow control -------------

// landing page button click
function splashButtonClick() {
	setDisplay('splash','none') 
	setDisplay('demographics','')
	setHeader('Demographics') 
}

// function to record demographics
function demographicsButtonClick() {
	
	recordDemographics() // writes demographic details to the data.demographics structure
	demoSuccess = validateDemographicData() // validates demographic data
	
	if (demoSuccess){
		setDisplay('demographics','none') 
		setHeader('A Simple Choice Game')
		setDisplay('instruction','')
		setFocus('instructionButton')
		tic = Date.now() // start timer [see ***]
	} else{
		clearCheckRadio()
		setDisplay('demographics','none') 
		setHeader('Please Try Again!')		
		setDisplay( 'demographicFail', '')
		setFocus('demographicFailButton')
	}		
} 


// function to return to demographic questions
function demoFailButtonClick(){
	setDisplay('demographicFail','none')
	setDisplay('demographics','')
	setHeader('Demographics') 	
}

// function to advance instructions
function instructionButtonClick() {

	if( instruct==nInstruct-1) { // change the text when we first reach last instruction
		document.getElementById("instructionButton").value = "Check your knowledge!"
	}

	if( instruct >= nInstruct ) { // if all instructions are revealed, move along
		setDisplay('instruction','none') 
 		setDisplay('check','') 
 		setHeader('Check Your Knowledge!')
 		toc = Date.now() // stop timer [see ***]
 		scrollTo('top')
//		setDisplay('start','');
		
	} else { // reveal next instruction if needed
		setDisplay('instruction'+instruct,'')
		setBoxBorder( instruct-1, '2px dotted grey' )	
		scrollTo('instruct'+instruct)
		}
	instruct++
}

// function to check instructions
function checkButtonClick() {
	setDisplay( 'check', 'none')
	
	var success = storeInstructionData()
	if( success ) {
		setHeader('Correct!')
		setDisplay( 'checkSuccess', '')
		setFocus('checkSuccessButton')
	} else {
		clearCheckRadio()
		setHeader('Please Try Again!')
		setDisplay( 'checkFail', '')
		setFocus('checkFailButton')
	}
	
}

// function for experiment start
function startButtonClick(){
	setDisplay( 'checkSuccess', 'none')
	setDisplay("start","none")
	writeData(data); // participant, demographic and instruction check data currently written by a separate function to that which writes trial data.
	document.getElementById("header").style.textAlign = "center";
	data.walks = assignWalks();
	payoffs = assignPayoffs();
	data.payoffs = payoffs;
	changeNumber = Math.floor(Math.random() * ((changeWindowEnd + 1) - changeWindowStart) + changeWindowStart);
	tagText = tagTexts[Math.floor(Math.random() * tagTexts.length)];


	trialHandler();
}

// function after instruction check fail
function checkFailButtonClick() {
	setDisplay( 'checkFail', 'none')
	checkCount++
	setBoxBorder( nInstruct-1, '2px dotted grey' )
	setDisplay( 'instruction', '' )
	setHeader('A Simple Choice Game')
	setFocus('instructionButton')
	scrollTo('top')
	tic = Date.now() // start timer [see ***]
}

// ------------ functions: in-trial dynamics -------------

// function to determine how to proceed in this trial
function trialHandler(){
	clearCanvas();
	if (trialNo <= maxTrials){
		startTrial();
	} else{
		showBlockFeedback();
	}	
}

// function to start a new trial
function startTrial(){
	var headerText = "Game " + blockNo + " of " + nBlocks + ", Choice " + trialNo + " of " + maxTrials;
	setHeader(headerText);
	drawArray();
	trialTic = Date.now();
	listenTrialResponse();
}

// wait for one of the buttons to be pressed
function listenTrialResponse(){
	$(document).keydown(function(evt){
	if (evt.which == 37 | evt.which == 38 | evt.which == 39 | evt.which == 40){ // these are the four arrow keys
		trialToc = Date.now();
		showSelection(evt);
	}});
}

// prepare the visual array for the next trial
function prepareNextTrial(){

	whichSelected = "none";
	clearCanvas();
	drawArray();
	trialNo = trialNo + 1;
	trialHandler();
}

// ------------ functions: option display -------------

// draw the option array
function drawArray(){


var topStroke = outlineColour, bottomStroke = outlineColour, leftStroke = outlineColour, rightStroke = outlineColour;
var topFill = "#FFFFFF", bottomFill = "#FFFFFF", leftFill = "#FFFFFF", rightFill = "#FFFFFF";


switch(whichSelected) {
	case "top":
		topStroke = selectionColour;
		break
	case "bottom":
		bottomStroke = selectionColour;
		break
	case "left":
		leftStroke = selectionColour;
		break
	case "right":
		rightStroke = selectionColour;
		break
	default:
		break
}

switch(whichFilled) {
	case "top":
		topFill = fillColour;
		indicatorTextX = centreX - (1.5 * circleOffset)
		indicatorTextY = centreY - circleOffset
		arrowStartX = centreX - circleOffset
		arrowStartY = centreY - circleOffset
		arrowEndX = centreX - (1.6 * radius)
		arrowEndY = centreY - circleOffset
		break
	case "bottom":
		bottomFill = fillColour;
		indicatorTextX = centreX - (1.5 * circleOffset)
		indicatorTextY = centreY + circleOffset
		arrowStartX = centreX - circleOffset
		arrowStartY = centreY + circleOffset
		arrowEndX = centreX - (1.6 * radius)
		arrowEndY = centreY + circleOffset
		break
	case "left":
		leftFill = fillColour;
		indicatorTextX = centreX - (2.5 * circleOffset)
		indicatorTextY = centreY + circleOffset
 		indicatorTextY = centreY
		arrowStartX = centreX - (2 * circleOffset)
		arrowStartY = centreY
		arrowEndX = centreX - circleOffset - (1.6 * radius)
		arrowEndY = centreY
		break
	case "right":
		rightFill = fillColour;
		indicatorTextX = centreX + (2.5 * circleOffset)
		indicatorTextY = centreY
		arrowStartX = centreX + (2 * circleOffset)
		arrowStartY = centreY
		arrowEndX = centreX + circleOffset + (1.5 * radius)
		arrowEndY = centreY
		break
	default:
		break
}

canvas.style.background = "white"

if (whichFilled != "none"){
 	context.fillStyle = selectionColour;
	context.fillText(tagText, indicatorTextX, indicatorTextY)	
	drawArrow(arrowStartX, arrowStartY, arrowEndX, arrowEndY)	
}

// draw top circle
context.fillStyle = topFill;
context.beginPath();
context.arc(centreX, centreY - circleOffset, radius, 0, 2 * Math.PI, false)
context.fill();
context.lineWidth = lineWidth;
context.strokeStyle = topStroke;
context.closePath();
context.stroke();

// draw bottom circle
context.fillStyle = bottomFill;
context.beginPath();
context.arc(centreX, centreY + circleOffset, radius, 0, 2 * Math.PI, false)
context.fill();
context.lineWidth = lineWidth;
context.strokeStyle = bottomStroke;
context.closePath();
context.stroke();

// draw left circle
context.fillStyle = leftFill;
context.beginPath();
context.arc(centreX - circleOffset, centreY, radius, 0, 2 * Math.PI, false)
context.fill();
context.lineWidth = lineWidth;
context.strokeStyle = leftStroke;
context.closePath();
context.stroke();

// draw right circle
context.fillStyle = rightFill;
context.beginPath();
context.arc(centreX + circleOffset, centreY, radius, 0, 2 * Math.PI, false)
context.fill();
context.lineWidth = lineWidth;
context.strokeStyle = rightStroke;
context.closePath();
context.stroke();

}

// highlight the selected option (but don't show points yet)
function showSelection(evt){
$(document).off("keydown");
switch (evt.which){
	case 37:
		whichSelected = "left";
		break
	case 38:
		whichSelected = "top";	
		break
	case 39:
		whichSelected = "right";			
		break
	case 40:
		whichSelected = "bottom";
		break
	default:
		whichSelected = "none";
		break		
}
// data.choice[trialNo] = whichSelected;
evt.preventDefault();
clearCanvas();
drawArray();
setTimeout(displayFeedback,selectionDisplayInterval)
}

// clear the html canvas
function clearCanvas(){
	context.clearRect (0, 0, canvas.width, canvas.height);
}

// display trial feedback
function displayFeedback(){	
	var pointsWon, hDist;
	clearCanvas();
	drawArray();
	context.fillStyle = selectionColour;
	context.textBaseline = "middle";
	pointsWon = getWinnings();
	maxPossible = getMaxWinnings();
	blockWinnings = blockWinnings + pointsWon;
	maxBlockWinnings = maxBlockWinnings + maxPossible; //keep track of maximum possible winnings in block
	context.fillText(pointsWon, centreX, centreY)
	writeTrialData(); //moved to fix problems with asynchronous code execution in version 2.1
	if ((trialNo + 1) == changeNumber){assignChange(); whichFilled = whichToFill;};  // fill the changed option at the appropriate time
	drawArray();
	setTimeout(prepareNextTrial,feedbackDisplayInterval)
	//writeTrialData();
}

// display block-final feedback
function showBlockFeedback(){
	var text1, text2, hDist1, hDist2;
	headerText = "Feedback: Game " + blockNo + " of " + nBlocks;
	setHeader(headerText);
	clearCanvas();
	context.fillStyle = "#000000"
	context.textBaseline = "middle"
	oldFont = context.font;
	context.font = "20px Helvetica";
	text1 = "You won " + blockWinnings + " points in this block.";
	text2 = "That's equal to " + Math.round(((blockWinnings / maxBlockWinnings) * 100)) + " percent of the best possible score."
	text3 = "Press any button to continue.";
	context.fillText(text1, centreX, canvas.height/8)
	context.fillText(text2, centreX, 40+ canvas.height/8)
	context.fillText(text3, centreX, 80+ canvas.height/8)
	context.font = oldFont;
	trialNo = 1;
	whichFilled = "none";
	changeNumber = Math.floor(Math.random() * ((changeWindowEnd + 1) - changeWindowStart) + changeWindowStart); 
	blockWinnings = 0;
	maxBlockWinnings = 0;
	setTimeout(function(){
		if (blockNo < nBlocks) {
		blockNo = blockNo + 1;			
		$(document).keydown(function(evt){trialHandler()})
	} else {
		$(document).keydown(function(evt){clearCanvas(); wrapUp();})
	}	
	},blockMinimumInterval)
	// old version below (pre-2.1):
	/* 	if (blockNo < nBlocks) {
		trialNo = 1;
		whichFilled = "none";
		blockNo = blockNo + 1;
		changeNumber = Math.floor(Math.random() * ( (maxTrials - 4) - 5 ) + 5); 
		blockWinnings = 0;
		maxBlockWinnings = 0;
		$(document).keydown(function(evt){trialHandler()})
	} else {
		$(document).keydown(function(evt){clearCanvas(); wrapUp();})
	}	 */	
}

function wrapUp() {	
	setHeader( "Finished!") 
	setDisplay( "wrapup", "")
	window.onbeforeunload = function(){}
}

// ------------ functions: generate and retrieve payouts -------------

// assign the mean of the payoffs for each option on each trial
function assignWalks(){
	
	walkMu = createArray(nCircles, nBlocks, maxTrials)
	
	// horribly inelegant, but works:
	for (var iOption = 0; iOption < walkMu.length; iOption++){
		optionWalk = walkMu[iOption];
		for (var iBlock = 0; iBlock < optionWalk.length; iBlock++){
			blockWalk = optionWalk[iBlock];
		for (var iTrial = 0; iTrial < blockWalk.length; iTrial++){
			if (iTrial == 0){
				blockWalk[iTrial] = Math.random() * 100;			
			} else{
				blockWalk[iTrial] = Infinity;
				while ((blockWalk[iTrial] > 100 || blockWalk[iTrial] < 0)){
					blockWalk[iTrial] = blockWalk[iTrial-1] + rnorm(0,walkSigma);
			}
			}
			optionWalk[iBlock] = blockWalk;
		}
		walkMu[iOption] = optionWalk;
		}
	}
	return walkMu;
}

// assign actual payoffs for each of the options on every trial
function assignPayoffs(){
	var optionPayoff, blockPayoff
	payoffs = createArray(nCircles,nBlocks,maxTrials);
	
	// horribly inelegant, but works:
	for (var iOption = 0; iOption < payoffs.length; iOption++){
		optionPayoff = payoffs[iOption];
		for (var iBlock = 0; iBlock < optionPayoff.length; iBlock++){
			blockPayoff = optionPayoff[iBlock];
		for (var iTrial = 0; iTrial < blockPayoff.length; iTrial++){
			blockPayoff[iTrial] = Infinity;
			while ((blockPayoff[iTrial] > 100 || blockPayoff[iTrial] < 0)){
				blockPayoff[iTrial] = Math.round(rnorm(data.walks[iOption][iBlock][iTrial],payoffSigma));
			}
		}
			optionPayoff[iBlock] = blockPayoff;
		}
		payoffs[iOption] = optionPayoff;
		}
	return payoffs
}

// determine the trial number at which an option will become filled
function assignChange(){
	var whichChange; 
	whichToFill = whichSelected//initialise as same as previous choice to ensure while loop runs
	while (whichToFill == whichSelected){
		whichChange = Math.floor(Math.random() * 3);
		switch(whichChange){
			case 0:
				whichToFill = "top";
				break
			case 1:
				whichToFill = "right";
				break
			case 2:
				whichToFill = "bottom"
				break
			case 3:
				whichToFill = "left";
				break
			default:
				break
		}
	}
}

// retrieve the amount won on a given trial
function getWinnings(){
	switch(whichSelected) {
		case "top":
			circleNo = 0;
			break
		case "bottom":
			circleNo= 2;
			break
		case "left":
			circleNo = 3;
			break
		case "right":
			circleNo = 1;
			break
		default:
			break
	}
	pointsWon = payoffs[circleNo][blockNo-1][trialNo-1];
	return pointsWon;
}

// find the value of the best option on a given trial
function getMaxWinnings(){
	maxWinnings = Math.max(payoffs[0][blockNo-1][trialNo-1], payoffs[1][blockNo-1][trialNo-1], payoffs[2][blockNo-1][trialNo-1], payoffs[3][blockNo-1][trialNo-1])
	return maxWinnings;	
}

// ------------ functions: data storage -------------

// function storing demographic data
function recordDemographics() {
	data.gender = getRadioButton("gender")
	data.age = document.getElementById("age").value
	data.language = document.getElementById("language").value
	data.country = document.getElementById("country").value
}

// function validating demographic data
function validateDemographicData(){
	
	nCompleted = 0;
	if (data.gender != undefined) nCompleted++;
	if (data.age != "") nCompleted ++;
	if (data.language != "") nCompleted ++;
	if (data.country != "Please select a country from the list") nCompleted++;
	
	var success = nCompleted == nDemographic;
	return success
}


// function storing data from an instruction read/check
function storeInstructionData() {
	var val
	var nCorrect=0
	var toc = Date.now() // stop the timer [see **]

	for( var q=0; q<nQuestions; q++) {
		val = getRadioButton("question"+q)
		if( val=="correct") nCorrect++
	}
	
	// store
	data.instructionTime[ checkCount ] = toc-tic  // [see ***]
	data.instructionCheckScore[ checkCount ] = nCorrect
	data.instructionFails = checkCount 
	
	// return
	var success = nCorrect == nQuestions
	return success
}

// function storing data from the trial
function writeTrialData() {
	var dataString;
	
	data.ID = data.ID;
	data.block = blockNo;
	data.trial = trialNo;
	data.choice = whichSelected;
	data.responseTime = trialToc - trialTic; // rt in ms is the difference
	data.pointsWon = pointsWon;
	data.blockWinnings = blockWinnings;
	data.whichFilled = whichFilled;
	data.tagText = tagText;
	if ((trialNo + 1) == changeNumber){data.whichFilled = "none";};  // fix a problem in how filled circle is logged
	if (trialNo > 1 || blockNo > 1){
		data.payoffs = [];
		data.walks = [];
	}
	if (trialNo == maxTrials && blockNo == nBlocks) {
		data.stopTime = Date.now();
	}
			
	dataString = JSON.stringify( data )
	//console.log( dataString ) // comment this out for the real thing
    $.post('submit', {"content": dataString}); // uncomment this to have it actually write [remember to load jQuery!!]
}


// function writing data to disk
function writeData(dataToWrite) {
	var dataString = JSON.stringify( dataToWrite )
	//console.log( dataString ) // comment this out for the real thing
    $.post('submit', {"content": dataString}); // uncomment this to have it actually write [remember to load jQuery!!]
}

// ------------ functions: generic UI helpers -------------

// move to the specified location
function scrollTo(hash) {
    location.hash = "#" + hash;
}

// get the value of a radio button
function getRadioButton( name ) {
	var radios = document.getElementsByName( name );
	for (var i = 0, length = radios.length; i < length; i++) {
	    if (radios[i].checked) {
	        return( radios[i].value )
		}
	}
}

// function to change the display property of a set of objects
function setDisplay( theClass, theValue ) {
	var classElements = document.getElementsByClassName( theClass )
	for( var i=0; i< classElements.length; i++ ) { 
		classElements[i].style.display = theValue
	}
}

// set the focus
function setFocus( theElement ) {
	document.getElementById( theElement ).focus()
}

// ------------ functions: specific UI helpers -------------

// alter the header
function setHeader( theValue ) {
	document.getElementById("header").innerText =  theValue
}

// alter the border (on one of the instruction boxes)
function setBoxBorder( whichBox, theValue ) {
	document.getElementById('instruction'+ whichBox +'inner').style.border=theValue
}

// clear all the check marks for the radio buttons on the instruction checks
function clearCheckRadio() {
	var radios = document.getElementsByClassName( 'checkRadio' );
	for (var i = 0, length = radios.length; i < length; i++) {
		radios[i].checked = false
	}
}

// ------------ functions: mathematical and other helpers -------------

// create array of arbitrary size and dimensionality
function createArray(length) {
    var arr = new Array(length || 0),
        i = length;
    if (arguments.length > 1) {
        var args = Array.prototype.slice.call(arguments, 1);
        while(i--) arr[length-1 - i] = createArray.apply(this, args);
    }
    return arr;
}

// before unload handler function
function onBeforeUnloadHandler(e) {
	
  // store it.
  data.unloadAttempts++
	
  var message = "You are about to leave this page, but have not yet finished the experiment.",
  e = e || window.event;
  // For IE and Firefox
  if (e) {
    e.returnValue = message;
  }

  // For Safari
  return message;
};

function drawArrow(fromx, fromy, tox, toy){
	//variables to be used when creating the arrow
	var c = document.getElementById("canvasHandle");
	var ctx = c.getContext("2d");
	var headlen = 10;

	var angle = Math.atan2(toy-fromy,tox-fromx);

	//starting path of the arrow from the start square to the end square and drawing the stroke
	ctx.beginPath();
	ctx.moveTo(fromx, fromy);
	ctx.lineTo(tox, toy);
	ctx.strokeStyle = "#00BFFF";
	ctx.lineWidth = 22;
	ctx.stroke();

	//starting a new path from the head of the arrow to one of the sides of the point
	ctx.beginPath();
	ctx.moveTo(tox, toy);
	ctx.lineTo(tox-headlen*Math.cos(angle-Math.PI/7),toy-headlen*Math.sin(angle-Math.PI/7));

	//path from the side point of the arrow, to the other side point
	ctx.lineTo(tox-headlen*Math.cos(angle+Math.PI/7),toy-headlen*Math.sin(angle+Math.PI/7));

	//path from the side point back to the tip of the arrow, and then again to the opposite side point
	ctx.lineTo(tox, toy);
	ctx.lineTo(tox-headlen*Math.cos(angle-Math.PI/7),toy-headlen*Math.sin(angle-Math.PI/7));

	//draws the paths created above
	ctx.strokeStyle = "#00BFFF";
	ctx.lineWidth = 22;
	ctx.stroke();
	ctx.fillStyle = "#00BFFF";
	ctx.fill();
}

