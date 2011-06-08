function(doc) {
    if(doc.artist)
	emit(doc.artist, 1);  
}
