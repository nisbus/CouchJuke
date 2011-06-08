function(doc) {
    if(doc.artist)
	emit(doc.artist, {id: doc._id, year: doc.year,genre: doc.genre,title: doc.title, rating: 
doc.rating,comment: doc.comment,track_count: doc.songs.length, cover: 
'/couchjuke/'+doc._id+'/cover', 
artist: doc.artist}); 
}
