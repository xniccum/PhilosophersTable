var express = require('express')
var path = require('path')
var app = express()

var bodyParser = require('body-parser')
app.use(bodyParser.json())

var mongoose = require('mongoose')
mongoose.connect('mongodb://localhost/philosophers')

var db = mongoose.connection
db.on('error', console.error.bind(console, 'connection error:'))
db.once('open', function() {
	console.log('connected to mongo')
})

var philosopherSchema = mongoose.Schema({
    pid: String,
    leftNeighbor: String,
    rightNeighbor: String,
    hasLeftFork: Boolean,
    hasRightFork: Boolean,
    hasCup: Boolean
})

var Philosopher = mongoose.model('Philosopher',philosopherSchema)

Philosopher.remove({},function(err){
	if (err) console.log("ERROR: could not clear philosophers from database")
	else console.log("cleared all philosophers from database")
})

function isPhilosopher(obj){
	return (obj.pid != undefined)&&(obj.leftNeighbor != undefined)&&
		(obj.rightNeighbor != undefined)&&(obj.hasLeftFork != undefined)&&
		(obj.hasRightFork != undefined)&&(obj.hasCup != undefined);
}

function makePhilosopher(obj){
	return new Philosopher(
		{
			pid: obj.pid,
			leftNeighbor: obj.leftNeighbor,
			rightNeighbor: obj.rightNeighbor,
			hasLeftFork: obj.hasLeftFork,
			hasRightFork: obj.hasRightFork,
			hasCup: obj.hasCup
		})
}

function setPhilosopher(phil){
	query = {pid:phil.pid}
    update = {
    	pid: phil.pid, 
    	leftNeighbor: phil.leftNeighbor, 
    	rightNeighbor: phil.rightNeighbor,
    	hasLeftFork: phil.hasLeftFork,
    	hasRightFork: phil.hasRightFork,
    	hasCup: phil.hasCup
    }
    options = { upsert: true, new: true, setDefaultsOnInsert: true }
    Philosopher.findOneAndUpdate(query, update, options, handlePhilosopherResponse)
}

function handlePhilosopherResponse(err,data){
	if (err) {
		ans = "ERROR: failed to set philosopher: " + JSON.stringify(data)
		console.log(ans)
	}
	else {
		ans = "set philosopher: " + JSON.stringify(data)
		console.log(ans)
	}
}

app.get('/api/philosophers', function (req, res) {
	res.setHeader('Access-Control-Allow-Origin', '*')
	Philosopher.find({},function(err,data){
 		if (err) res.send([])
  		else res.send(data.sort({pid: 1}))
	})
})

app.get('/', function(req, res) {
    res.sendFile(path.join(__dirname + '/index.html'));
})

app.get('/source/displayPhilosophers.js', function(req, res) {
    res.sendFile(path.join(__dirname + '/displayPhilosophers.js'));
})

app.get('/source/displayPhilosophers.css', function(req, res) {
    res.sendFile(path.join(__dirname + '/displayPhilosophers.css'));
})

app.post('/api/philosophers', function (req, res) {
	obj = req.body
	if (isPhilosopher(obj)) {
		setPhilosopher(makePhilosopher(obj))
	}
	else{
		ans = "ERROR: failed to set philosopher: " + JSON.stringify(obj)
		console.log(ans)
		res.send(ans)
	}
})

app.get(/\/images\/.+\.png/, function(req, res){
	rurl = req.originalUrl
	fn = "/" + rurl.substring(rurl.indexOf("/images/") + "/images/".length, rurl.length)
    res.setHeader('Access-Control-Allow-Origin', '*')
    res.sendFile( __dirname + fn)
 })

app.listen(3000, function () {
	console.log('Example app listening on port 3000!')
})