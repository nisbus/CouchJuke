function(data) {
  // $.log(data)
    var p;
    return {
	artists : data.rows.map(function(r) {
		console.log(r);
		p =  r;
		p.artist = r.key;
		return p;
    })
  }
};
