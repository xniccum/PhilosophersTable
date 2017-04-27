philosophers = []

url = "http://127.0.0.1:3000/api/philosophers"

function getPhilosophers(){
	$.ajax({
		url: url,
		success: function (data) {
			philosophers = data
			displayPhilosophers(data)
    		//$('#philolist').text(JSON.stringify(data))
  		}
	});
}

setInterval(getPhilosophers, 500)

function philosopherToDOM(obj){
	div = document.createElement('div')
	plate = document.createElement('img')
	lfork = document.createElement('img')
	rfork = document.createElement('img')
	cup = document.createElement('img')
	plate.src = "images/plate.png"
	lfork.src = "images/fork.png"
	rfork.src = "images/fork.png"
	cup.src = "images/cup.png"
	plate.className = "plate true"
	lfork.className = "fork " + JSON.stringify(obj.hasLeftFork)
	rfork.className = "fork " + JSON.stringify(obj.hasRightFork)
	cup.className = "cup " + JSON.stringify(obj.hasCup)
	div.appendChild(lfork)
	div.appendChild(plate)
	div.appendChild(rfork)
	div.appendChild(cup)
	return div
}

function displayPhilosophers(lst){
	parent = document.getElementById("philolist")
	parent.innerHTML = ""
	lst.forEach(function(e){
		phil = philosopherToDOM(e)
		parent.appendChild(phil)
	})
}