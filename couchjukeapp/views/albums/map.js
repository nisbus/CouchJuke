function(doc) {
    if(doc.album)
	emit(doc.album, 1); 
}