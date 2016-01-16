function bsort(a) {
	for(var i=1; i<a.length; ++i) {
		for(var j=i; j>=1; --j) {
			if (a[j] < a[j-1]) {
				var tmp = a[j-1];
				a[j-1] = a[j];
				a[j] = tmp;
			}
		}
	}
}

var a = [29,2,-5,17,4,8,5,8,1,-10,50,34]; 
bsort(a);
console.log(JSON.stringify(a));