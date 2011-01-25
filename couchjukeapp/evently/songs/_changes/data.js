function(data) {
  // $.log(data)
    var p;
    return {
	songs : data.rows.map(function(r) {
		p =  r;
		p.title = r.value.title;
		p.cover = r.value.cover;
		p.song = r.value.song;
		return p;
    })
  }
};