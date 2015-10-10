var Person = function(firstAndLast) {
  var splitname=firstAndLast.split(' ');
  var firstName=splitname[0];
  var lastName=splitname[1];
  this.setFirstName = function (first) {
      console.log(first)
      firstName = first;
      };
  this.setLastName = function (last) {
      lastName = last;};
  this.setFullName = function (firstAndLast) {
    splitname=firstAndLast.split(' ');
    this.setFirstName(splitname[0]);
    this.setLastName(splitname[1]); 
  };
  this.getFirstName= function () {return firstName;};
  this.getLastName= function () {return lastName;};
  this.getFullName = function () {return (this.getFirstName()+' '+this.getLastName());};
};

var bob = new Person('Bob Ross');
bob.getFullName();
//bob.firstName;



function orbitalPeriod(arr) {
  var GM = 398600.4418;
  var earthRadius = 6367.4447;
  var mu=GM;
  for (var i in arr) {
      el=arr[i];
      var a=el.avgAlt+earthRadius;  
      var T=Math.round(2*Math.PI*Math.sqrt(Math.pow(a,3)/mu));
      el.orbitalPeriod=T;
      delete el.avgAlt;
  }

  return arr;
}

orbitalPeriod([{name : "sputkin", avgAlt : 35873.5553}]);

function pairwise(arr, arg) {
  var sum=0;
  var donelist=[];
  for (var i=0;i<arr.length-1;i++) {
    if (donelist.indexOf(i)>-1) {continue;}
    el1=arr[i];
    for (var j=i+1;j<arr.length;j++) {
      if (donelist.indexOf(j)>-1) {continue;}
      el2=arr[j];
      if (el1+el2==arg) {
        sum+=i+j;
        donelist.push(i);
        donelist.push(j);
        break;
      }
    }
  }
  return sum;
}

pairwise([0, 0, 0, 0, 1, 1], 1);


function(console) {
	var names = ["Ben", "Jafar", "Matt", "Priya", "Brian"];

	names.forEach(function(name) {
		console.log(name);
	});
}

newReleases.forEach(function(video) {
    videoAndTitlePairs.push({id:video.id,title:video.title});
  });
  
var sum = 0;
for (var i in process.argv){
        if (i>1){
                sum= sum + Number(process.argv[i]);
        }
}
console.log(sum)


var fs = require('fs')
var text=fs.readFileSync(process.argv[2]).toString().split('\n');
console.log(text.length-1)

var fs = require('fs')
var text;
fs.readFile(process.argv[2],'utf8', function (err, contents) {
        console.log(contents.split('\n').length-1);
})

