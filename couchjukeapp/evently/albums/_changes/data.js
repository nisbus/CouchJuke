function(data) {
  // $.log(data)
    var p;
    return {
	albums : data.rows.map(function(r) {
		p =  r;
		p.album = r.key;
		p.count = r.value;
		console.log(p);
		return p;
    })
  }
};